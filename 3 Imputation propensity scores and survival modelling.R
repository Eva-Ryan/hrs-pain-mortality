# R code to load the HRS data cleaned and prepared using the code in
# "1 Select variables and merge waves.R" and  "2 Clean variables.R", impute
# missing values, calculate propensity scores, carry out matching, and fit
# survival models.

# Note: can skip the imputation step (takes about 2 hours) and just load in the
# imputed datasets from the file "imputations20_nonemild_modsev.rda" by running
# the load function in line 320

### Notes on Supplementary Analyses ###

# 1. To use the HRS sample weights, un-comment the line
# "s.weights = ~scaled_weights" in the matching / weighting functions, and the
# line "weights = scaled_weights" in the Cox models.

# 2. To repeat the analyses with a confounder (e.g., depressive symptoms)
# removed, comment out that variable (e.g., randCESD1998) in the formula for
# the propensity score model and in the code for the regression adjusted
# matching / doubly robust models.

# 3. To use a pain exposure definition

# 4. To use different follow-up lengths, change the "time" and "status"
# variables in the Cox model fitting code to those corresponding to the desired
# follow-up length e.g., for 15 year follow-up: "time" -> "time15" and
# "status" -> "status15".

####

#------------------------------------------------------------------------------

#------------------------
# load packages
library(tidyverse)
library(mice)
library(VIM)
library(tictoc)
library(MatchThem)
library(cobalt)
library(survival)
library(survminer)
library(ggfortify)
library(viridis)
library(gridExtra)
library(survey)
library(beepr)
library(compareGroups)
library(ggpubr)

#-----------------------------------------------------------------------------
# Prepare data

# get location of project directory
directory <- getwd()

# read in data - some variables were loading in as logicals rather than numbers
# corresponding to categories, so specified type in these cases
df <- read_csv(paste0(directory, "/HRS_data_clean.csv"),
               col_types = cols(sad2wksP2000 = col_character(),
                                cancerTrtmtLast2YrsP2004 = col_character(),
                                sad2wksP2002 = col_character(),
                                sad2wksP2004 = col_character(),
                                sad2wksP2006 = col_character(),
                                cancerTrtmtLast2YrsP2008 = col_character(),
                                cancerTrtmtLast2YrsP2016 = col_character(),
                                cancerTrtmtLast2YrsP2012 = col_character(),
                                htFeet1995 = col_character(),
                                htInches1995 = col_character(),
                                htFeet2000 = col_character(),
                                htInches2000 = col_character(),
                                htFeet2002 = col_character(),
                                htInches2002 = col_character(),
                                htFeet2004 = col_character(),
                                htInches2004 = col_character(),
                                everSmokeP2012 = col_character(),
                                smokeStatus2012 = col_character(),
                                everSmokeP2002 = col_character(),
                                smokeStatus2002 = col_character(),
                                everSmokeP2010 = col_character(),
                                smokeStatus2010 = col_character(),
                                everSmokeP2000  = col_character(),
                                smokeStatus2000 = col_character()
               ))

# prepare binary baseline pain variable (leave as 0/1 to make sure propensity
# scores are calculated correctly)
df <- df %>%
  mutate(painStatus1998 = car::recode(painUsualSeverity1998,
                                      "'No Pain' = 0;
                                      'Mild' = 0;
                                      'Moderate' = 1;
                                      'Severe' = 1"))

# scale 1998 sample weights so that they sum to one
df$scaled_weights <- df$fwgtr/sum(df$fwgtr)

# recode those with impossible years of death (pre 1998) to NA
df$yearOfDeath[which(df$yearOfDeath < 1998)] <- NA
df$monthOfDeath[which(df$yearOfDeath < 1998)] <- NA

# recode those who died after the study period of interest to NA
df$yearOfDeath[which(df$yearOfDeath > 2018)] <- NA
df$monthOfDeath[which(df$yearOfDeath > 2018)] <- NA

####################################
# create time and status variables #

# record survival time in months (to death or censoring) for various follow-up
# lengths: 1 year, 3 years, 5 years, 8 years, 10 years, 12 years, 15 years
# note, if someone drops out between waves, code their time of censoring as the
# first month of the next wave

# create time and status variables for the 21 year follow-up period first

# subtract 1998 from year to start at 1 = January 1998 and multiple by 12 to
# convert to months
df$time <- (df$yearOfDeath - 1998)*12 + df$monthOfDeath

# create "status" column
# record event: 1 = censored, 2 = dead
df$status <- 1
df$status[which(is.na(df$time) == FALSE)] <- 2

# fill in time to event for all the censored participants

# set final time period to December 2018
end_time <- (2019-1998)*12

# set time for those who survived and participating to final wave
df$time[which(df$intvwdP1998 == 1 & df$intvwdP2000 == 1 &
                df$intvwdP2002 == 1 & df$intvwdP2004 == 1 &
                df$intvwdP2006 == 1 & df$intvwdP2008 == 1 &
                df$intvwdP2010 == 1 & df$intvwdP2012 == 1 &
                df$intvwdP2014 == 1 & df$intvwdP2016 == 1 &
                df$intvwdP2018 == 1 & is.na(df$time) == TRUE,
              arr.ind = TRUE)] <- end_time

# set time for those who left during the study - a person's  censoring time is
# when they first left the study
df$time[which(is.na(df$time) == TRUE & df$intvwdP2000 == 0)] <- (2000-1998)*12 + 1
df$time[which(is.na(df$time) == TRUE & df$intvwdP2002 == 0)] <- (2002-1998)*12 + 1
df$time[which(is.na(df$time) == TRUE & df$intvwdP2004 == 0)] <- (2004-1998)*12 + 1
df$time[which(is.na(df$time) == TRUE & df$intvwdP2006 == 0)] <- (2006-1998)*12 + 1
df$time[which(is.na(df$time) == TRUE & df$intvwdP2008 == 0)] <- (2008-1998)*12 + 1
df$time[which(is.na(df$time) == TRUE & df$intvwdP2010 == 0)] <- (2010-1998)*12 + 1
df$time[which(is.na(df$time) == TRUE & df$intvwdP2012 == 0)] <- (2012-1998)*12 + 1
df$time[which(is.na(df$time) == TRUE & df$intvwdP2014 == 0)] <- (2014-1998)*12 + 1
df$time[which(is.na(df$time) == TRUE & df$intvwdP2016 == 0)] <- (2016-1998)*12 + 1
df$time[which(is.na(df$time) == TRUE & df$intvwdP2018 == 0)] <- (2018-1998)*12 + 1

# check
summary(as.factor(df$status))
summary(df$time)
table(df$time, df$status, useNA = "always")

# create variables - 1 year
df <- df %>%
  # create new time variable with max time = 12 months (December 1998)
  mutate(time1 = ifelse(time <= 12, time, 12)) %>%
  # create new status variable where participants are right censored if still
  # alive after 12 months
  mutate(status1 = ifelse(time <= 12, status, 1))

# create variables - 3 years
df <- df %>%
  # create new time variable with max time = 36 months (December 2000)
  mutate(time3 = ifelse(time <= 36, time, 36)) %>%
  # create new status variable where participants are right censored if still
  # alive after 36 months
  mutate(status3 = ifelse(time <= 36, status, 1))

# create variables - 5 years
df <- df %>%
  # create new time variable with max time = 60 months (December 2002)
  mutate(time5 = ifelse(time <= 60, time, 60)) %>%
  # create new status variable where participants are right censored if still
  # alive after 60 months
  mutate(status5 = ifelse(time <= 60, status, 1))

# create variables - 8 years
df <- df %>%
  # create new time variable with max time = 96 months (December 2005)
  mutate(time8 = ifelse(time <= 96, time, 96)) %>%
  # create new status variable where participants are right censored if still
  # alive after 96 months
  mutate(status8 = ifelse(time <= 96, status, 1))

# create variables - 10 years
df <- df %>%
  # create new time variable with max time = 120 months (December 2007)
  mutate(time10 = ifelse(time <= 120, time, 120)) %>%
  # create new status variable where participants are right censored if still
  # alive after 120 months
  mutate(status10 = ifelse(time <= 120, status, 1))

# create variables - 12 years
df <- df %>%
  # create new time variable with max time = 144 months (December 2009)
  mutate(time12 = ifelse(time <= 144, time, 144)) %>%
  # create new status variable where participants are right censored if still
  # alive after 144 months
  mutate(status12 = ifelse(time <= 144, status, 1))

# create variables - 15 years
df <- df %>%
  # create new time variable with max time = 180 months (December 2012)
  mutate(time15 = ifelse(time <= 180, time, 180)) %>%
  # create new status variable where participants are right censored if still
  # alive after 180 months
  mutate(status15 = ifelse(time <= 180, status, 1))


# select relevant variables only (1998 variables for propensity score model and
# survival data)
df <- df %>%
  select(ageIn1998, gender, race4Cats, maritalStatus1998,
         householdSize1998, numChildren1998, region4Cats1998, urbanicity1998,
         religionImportance1998, edu4Cats, wealthQuarts1998,
         jobStatus4Cats1998, foodSecurity1998, veteranStatus,
         insurance1998, cancerActiveP1998, diabetesP1998, lungDisP1998,
         anginaP1998, strokeP1998, bmi6Cats1998, hrtCondP1998, arthritisP1998,
         smokeStatus1998, randCESD1998, painStatus1998,
         painUsualSeverity1998, fwgtr, scaled_weights, status, time,
         status, time, status1, time1, status3, time3, status5, time5,
         status8, time8, status10, time10, status12, time12, status15, time15
  )


#------------------------
# convert relevant variables to factors
df <- df %>%
  mutate(gender = factor(gender, levels = c("male", "female"))) %>%
  mutate(race4Cats = factor(race4Cats, levels = c("White (non-Hispanic)",
                                                  "Black (non-Hispanic)",
                                                  "Hispanic",
                                                  "Other (non-Hispanic)"))) %>%
  mutate(maritalStatus1998 = factor(maritalStatus1998,
                                    levels = c("Married", "Separated/Divorced",
                                               "Widowed", "Never married",
                                               "Other"))) %>%
  mutate(region4Cats1998 = factor(region4Cats1998,
                                  levels = c("Northeast", "Mid-west", "South",
                                             "West"))) %>%
  mutate(urbanicity1998 = factor(urbanicity1998,
                                 levels = c("Urban", "Suburban",
                                            "Ex-urban/rural"))) %>%
  mutate(religionImportance1998 = factor(religionImportance1998,
                                         levels = c("Very important",
                                                    "Somewhat important",
                                                    "Not too important"))) %>%
  mutate(edu4Cats = factor(edu4Cats,
                           levels = c("No degree", "High school degree",
                                      "4-year college degree",
                                      "Graduate degree"))) %>%
  mutate(wealthQuarts1998 = factor(wealthQuarts1998,
                                   levels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(jobStatus4Cats1998 = factor(jobStatus4Cats1998,
                                     levels = c("Employed", "Unemployed",
                                                "Retired",
                                                "Not in labour force"))) %>%
  mutate(insurance1998 = factor(insurance1998,
                                levels = c("Uninsured", "Any private insurance",
                                           "Public insurance only"))) %>%
  mutate(bmi6Cats1998 = factor(bmi6Cats1998,
                               levels = c("Underweight (<18.5)",
                                          "Normal weight (<25)",
                                          "Overweight (<30),", "Obese 1 (<35)",
                                          "Obese 2 (<40)", "Obese 3 (40+)"))) %>%
  mutate(smokeStatus1998 = factor(smokeStatus1998,
                                  levels = c("Never smoker", "Former smoker",
                                             "Current smoker"))) %>%
  mutate(painUsualSeverity1998 = factor(painUsualSeverity1998,
                                        levels = c("No Pain", "Mild", "Moderate",
                                                   "Severe")))


# create lists of binary yes/no variables to convert to type factor
yn_vars <- c("foodSecurity1998", "veteranStatus",
             "cancerActiveP1998", "diabetesP1998", "lungDisP1998", "anginaP1998",
             "strokeP1998", "hrtCondP1998", "arthritisP1998")

# convert these yes/no to type factor
df[, names(df) %in% yn_vars] <- df[, names(df) %in% yn_vars] %>%
  lapply(FUN = function (x) factor(x, levels = c("no","yes")))

#------------------------
# check data
summary(df)

# check % missingness
na_count <- df %>% sapply(function(x) sum(is.na(x)))
na_percent <- paste0(round((na_count/nrow(df))*100, 1), "%")

data.frame(NA_count = na_count,
           NA_percent = na_percent)

# remove the 32 (0.2%) of observations missing pain status
df <- df %>%
  filter(is.na(painStatus1998) == FALSE)

# re-check % missingness
na_count <- df %>% sapply(function(x) sum(is.na(x)))
na_percent <- paste0(round((na_count/nrow(df))*100, 1), "%")

data.frame(NA_count = na_count,
           NA_percent = na_percent)


#------------------------------------------------------------------------------
# Impute missing data

# # look at missing data pattern for relevant variables
# md.pattern(select(df, -status, -time))
#
# # plot missing data pattern
# mice_plot <- aggr(select(df, -status, -time),
#                   col = c('navyblue','yellow'),
#                   numbers = TRUE, sortVars = TRUE,
#                   labels = names(select(df, -status, -time)),
#                   cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))

# initialise prediction and method matrix
init <- mice(df, maxit = 0)
imp_methods <- init$method
pred_matrix <- init$predictorMatrix

# specify what variables we don't want to used as predictors
pred_matrix[,c("painUsualSeverity1998", "fwgtr", "scaled_weights", "status",
               "time", "status1", "time1", "status3", "time3", "status5",
               "time5", "status8", "time8", "status10", "time10", "status12",
               "time12", "status15", "time15")] <- 0

# check prediction matrix
print(pred_matrix)

# specify what variables we don't want to be imputed
imp_methods[c("painUsualSeverity1998", "painStatus1998", "fwgtr",
              "scaled_weights", "status", "time", "status1", "time1", "status3",
              "time3", "status5", "time5", "status8", "time8", "status10",
              "time10", "status12", "time12", "status15", "time15")] <- ""

# check vector of imputation methods
imp_methods

# run multiple imputation
tic()
imputed <- mice(df,
                mthod = imp_methods,
                predictorMatrix = pred_matrix,
                m = 20,
                seed = 100)
toc()
beep()
# takes ~ 3.3 hours

# save results of imputation for later use
save(imputed, file = paste0(directory, "/imputations20_nonemild_modsev_range.rda"))

#------------------------------------------------------------------------------
# Propensity score methods

# read in imputed datasets
load(paste0(directory, "/imputations20_nonemild_modsev_range.rda"))

# define formula for propensity score model
f1 <- as.formula(painStatus1998 ~ ageIn1998 + I(ageIn1998^2) + gender + race4Cats +
                   maritalStatus1998 + householdSize1998 + numChildren1998 +
                   region4Cats1998 + urbanicity1998 + religionImportance1998 +
                   edu4Cats + wealthQuarts1998 + jobStatus4Cats1998 +
                   foodSecurity1998 + veteranStatus + insurance1998 +
                   cancerActiveP1998 + diabetesP1998 + lungDisP1998 +
                   anginaP1998 + strokeP1998 + bmi6Cats1998 + hrtCondP1998 +
                   arthritisP1998 +
                   smokeStatus1998 +
                   randCESD1998
)

#------------------------------------
# Propensity score matching

# 1-to-1 nearest neighbours matching with replacement
matched_rep <- matchthem(formula = f1,
                         datasets = imputed,
                         method = "nearest",
                         distance = "glm",
                         link = "logit",
                         caliper = 0.1,
                         std.caliper = TRUE,
                         estimand = "ATT",
                         #s.weights = ~scaled_weights,
                         discard = "both",
                         replace = TRUE)

# 1-to-1 nearest neighbours matching without replacement
matched_no_rep <- matchthem(formula = f1,
                            datasets = imputed,
                            method = "nearest",
                            distance = "glm",
                            link = "logit",
                            caliper = 0.1,
                            std.caliper = TRUE,
                            estimand = "ATT",
                            #s.weights = ~scaled_weights,
                            discard = "both",
                            replace = FALSE)

# 2-to-1 nearest neighbour matching with replacement
matched_rep2 <- matchthem(formula = f1,
                          datasets = imputed,
                          method = "nearest",
                          distance = "glm",
                          link = "logit",
                          caliper = 0.1,
                          std.caliper = TRUE,
                          estimand = "ATT",
                          ratio = 2,
                          #s.weights = ~scaled_weights,
                          discard = "both",
                          replace = TRUE)

# 2-to-1 nearest neighbour matching without replacement
matched_no_rep2 <- matchthem(formula = f1,
                             datasets = imputed,
                             method = "nearest",
                             distance = "glm",
                             link = "logit",
                             caliper = 0.1,
                             std.caliper = TRUE,
                             estimand = "ATT",
                             ratio = 2,
                             #s.weights = ~scaled_weights,
                             discard = "both",
                             replace = FALSE)

# remove limit on size of optimal matching problems
options("optmatch_max_problem_size" = Inf)

# optimal full matching
opt_full_matched <- matchthem(formula = f1,
                              datasets = imputed,
                              method = "full",
                              distance = "glm",
                              link = "logit",
                              caliper = 0.1,
                              std.caliper = TRUE,
                              estimand = "ATT",
                              #s.weights = ~scaled_weights,
                              discard = "both",
                              replace = FALSE)

# assess balance - tables

# set global option to standardize differences for continuous and binary
# variables
set.cobalt.options(binary = "std", continuous = "std")

# create tables
matched_rep.t <- bal.tab(matched_rep, stats = 'mean.diffs', un = TRUE)
matched_rep.t

matched_no_rep.t <- bal.tab(matched_no_rep, stats = 'mean.diffs', un = TRUE)
matched_no_rep.t

matched_rep2.t <- bal.tab(matched_rep2, stats = 'mean.diffs', un = TRUE)
matched_rep2.t

matched_no_rep2.t <- bal.tab(matched_no_rep2, stats = 'mean.diffs', un = TRUE)
matched_no_rep2.t

opt_full_matched.t <- bal.tab(opt_full_matched, stats = 'mean.diffs', un = TRUE)
opt_full_matched.t

#------------------------------------
# Inverse propensity score weighting

weighted <- weightthem(formula = f1,
                       datasets = imputed,
                       method = 'glm',
                       approach = 'within',
                       stabilize = TRUE,
                       estimand = "ATT",
                       #s.weights = "scaled_weights",
                       include.obj = TRUE)

# assess balance - table
weighted.t <- bal.tab(weighted, stats = 'mean.diffs', un = TRUE)
weighted.t

# create dataframe of old and new variable names to use to make the plot look
# nicer
plot_var_names <- data.frame(old = c("ageIn1998",
                                     "I(ageIn1998^2)",
                                     "gender_female",
                                     "race4Cats_White (non-Hispanic)",
                                     "race4Cats_Black (non-Hispanic)",
                                     "race4Cats_Hispanic",
                                     "race4Cats_Other (non-Hispanic)",
                                     "maritalStatus1998_Married",
                                     "maritalStatus1998_Separated/Divorced",
                                     "maritalStatus1998_Widowed",
                                     "maritalStatus1998_Never married",
                                     "maritalStatus1998_Other",
                                     "householdSize1998",
                                     "numChildren1998",
                                     "region4Cats1998_Northeast",
                                     "region4Cats1998_Mid-west",
                                     "region4Cats1998_South",
                                     "region4Cats1998_West",
                                     "urbanicity1998_Urban",
                                     "urbanicity1998_Suburban",
                                     "urbanicity1998_Ex-urban/rural",
                                     "religionImportance1998_Very important",
                                     "religionImportance1998_Somewhat important",
                                     "religionImportance1998_Not too important",
                                     "edu4Cats_No degree",
                                     "edu4Cats_High school degree",
                                     "edu4Cats_4-year college degree",
                                     "edu4Cats_Graduate degree",
                                     "wealthQuarts1998_Q1",
                                     "wealthQuarts1998_Q2",
                                     "wealthQuarts1998_Q3",
                                     "wealthQuarts1998_Q4",
                                     "jobStatus4Cats1998_Employed",
                                     "jobStatus4Cats1998_Unemployed",
                                     "jobStatus4Cats1998_Retired",
                                     "jobStatus4Cats1998_Not in labour force",
                                     "foodSecurity1998_yes",
                                     "veteranStatus_yes",
                                     "insurance1998_Uninsured",
                                     "insurance1998_Any private insurance",
                                     "insurance1998_Public insurance only",
                                     "cancerActiveP1998_yes",
                                     "diabetesP1998_yes",
                                     "lungDisP1998_yes",
                                     "anginaP1998_yes",
                                     "strokeP1998_yes",
                                     "bmi6Cats1998_Underweight (<18.5)",
                                     "bmi6Cats1998_Normal weight (<25)",
                                     "bmi6Cats1998_Overweight (<30),",
                                     "bmi6Cats1998_Obese 1 (<35)",
                                     "bmi6Cats1998_Obese 2 (<40)",
                                     "bmi6Cats1998_Obese 3 (40+)",
                                     "hrtCondP1998_yes",
                                     "arthritisP1998_yes",
                                     "smokeStatus1998_Never smoker",
                                     "smokeStatus1998_Former smoker",
                                     "smokeStatus1998_Current smoker",
                                     "randCESD1998"),
                             new = c("Age in years",
                                     "Age in years squared",
                                     "Sex: Female",
                                     "Race: White (non-Hispanic)",
                                     "Race: Black (non-Hispanic)",
                                     "Race: Hispanic",
                                     "Race: Other (non-Hispanic)",
                                     "Marital status: Married",
                                     "Marital status: Separated/divorced",
                                     "Marital status: Widowed",
                                     "Marital status: Never married",
                                     "Marital status: Other",
                                     "Household size",
                                     "Number of children",
                                     "Region: Northeast",
                                     "Region: Mid-west",
                                     "Region: South",
                                     "Region: West",
                                     "Urbanicity: Urban",
                                     "Urbanicity: Suburban",
                                     "Urbanicity: Ex-urban/rural",
                                     "Religion: Very important",
                                     "Religion: Somewhat important",
                                     "Religion: Not too important",
                                     "Education level: No degree",
                                     "Education level: High school degree",
                                     "Education level: 4-year college degree",
                                     "Education level: Graduate degree",
                                     "Wealth quartile: Q1",
                                     "Wealth quartile: Q2",
                                     "Wealth quartile: Q3",
                                     "Wealth quartile: Q4",
                                     "Employment status: Employed",
                                     "Employment status: Unemployed",
                                     "Employment status: Retired",
                                     "Employment status: Not in labour force",
                                     "Food security: Yes",
                                     "Veteran status: Yes",
                                     "Health insurance: Uninsured",
                                     "Health insurance: Any private insurance",
                                     "Health insurance: Public insurance only",
                                     "Active cancer: Yes",
                                     "Diabetes: Yes",
                                     "Lung disease: Yes",
                                     "Angina: Yes",
                                     "Stroke: Yes",
                                     "BMI category: Underweight (<18.5)",
                                     "BMI category: Normal weight (<25)",
                                     "BMI category: Overweight (<30)",
                                     "BMI category: Obese 1 (<35)",
                                     "BMI category: Obese 2 (<40)",
                                     "BMI category: Obese 3 (40+)",
                                     "Heart condition: Yes",
                                     "Arthritis: Yes",
                                     "Smoker status: Never smoker",
                                     "Smoker status: Former smoker",
                                     "Smoker status: Current smoker",
                                     "Depressive symptoms (CESD score)"))


# plot all matched/weighted datasets and original data
love.plot(matched_rep,
          stats = "m",
          abs = TRUE,
          weights = list(no_r = matched_no_rep,
                         r2 = matched_rep2,
                         no_r2 = matched_no_rep2,
                         opt_full = opt_full_matched,
                         w = weighted),
          drop.distance = TRUE,
          thresholds = c(m = .1),
          var.order = "unadjusted",
          var.names = plot_var_names,
          size = 2.5,
          agg.fun = "range",
          #stars = "std",
          sample.names = c("Original data", "1:1 matched (with replacement)",
                           "1:1 matched (without replacement)",
                           "2:1 matched (with replacement)",
                           "2:1 matched (without replacement)",
                           "Optimal full matched",
                           "Inverse propensity weighted"),
          position = "right")


# plot just the methods discussed in the main text
love.plot(matched_no_rep,
          stats = "m",
          abs = TRUE,
          weights = list(w = weighted),
          drop.distance = TRUE,
          thresholds = c(m = .1),
          var.order = "unadjusted",
          var.names = plot_var_names,
          size = 2.5,
          agg.fun = "range",
          #stars = "std",
          sample.names = c("Original data",
                           "1:1 matched (without replacement)",
                           "Inverse propensity weighted"),
          position = "right")


# plot the methods included as supplementary material
love.plot(matched_rep,
          stats = "m",
          abs = TRUE,
          weights = list(r2 = matched_rep2,
                         no_r2 = matched_no_rep2,
                         opt_full = opt_full_matched),
          drop.distance = TRUE,
          thresholds = c(m = .1),
          var.order = "unadjusted",
          var.names = plot_var_names,
          size = 2.5,
          agg.fun = "range",
          #stars = "std",
          sample.names = c("Original data", "1:1 matched (with replacement)",
                           "2:1 matched (with replacement)",
                           "2:1 matched (without replacement)",
                           "Optimal full matched"),
          position = "right")

#------------------------------------------------------------------------------
# Fit survival models

# NB - CHANGE TIME AND STATUS VARIABLES FOR DIFFERENT FOLLOW-UP LENGTHS

#----------------------------------------------
# fit model to each imputed dataset - just pain

# original data, no PS methods, pain only
fit_original <- with(imputed,
                     coxph(Surv(time, status) ~ painStatus1998,
                           robust = TRUE#,
                           #weights = scaled_weights
                     ))

# original data, no PS methods, pain age and sex
fit_original_age_sex <- with(imputed,
                             coxph(Surv(time, status) ~ painStatus1998 +
                                     ageIn1998 + I(ageIn1998^2) + gender,
                                   robust = TRUE#,
                                   #weights = scaled_weights
                             ))

# original data, no PS methods, pain and all confounders
fit_original_confounders <- with(imputed,
                                 coxph(Surv(time, status) ~ painStatus1998 +
                                         ageIn1998 + I(ageIn1998^2) + gender +
                                         race4Cats + maritalStatus1998 +
                                         householdSize1998 + numChildren1998 +
                                         region4Cats1998 + urbanicity1998 +
                                         religionImportance1998 + edu4Cats +
                                         wealthQuarts1998 + jobStatus4Cats1998 +
                                         foodSecurity1998 + veteranStatus +
                                         insurance1998 + cancerActiveP1998 +
                                         diabetesP1998 + lungDisP1998 + anginaP1998 +
                                         strokeP1998 + bmi6Cats1998 + hrtCondP1998 +
                                         arthritisP1998 +
                                         smokeStatus1998 +
                                         randCESD1998,
                                       robust = TRUE#,
                                       #weights = scaled_weights
                                 ))

# 1-to-1 matching with replacement
fit_matched_r <- with(matched_rep,
                      coxph(Surv(time, status) ~ painStatus1998,
                            robust = TRUE#,
                            #weights = scaled_weights
                      ))

# 1-to-1 matching without replacement
fit_matched_no_r <- with(matched_no_rep,
                         coxph(Surv(time, status) ~ painStatus1998,
                               robust = TRUE#,
                               #weights = scaled_weights
                         ))

# 2-to-1 matching with replacement
fit_matched_r2 <- with(matched_rep2,
                       coxph(Surv(time, status) ~ painStatus1998,
                             robust = TRUE#,
                             #weights = scaled_weights
                       ))

# 2-to-1 matching without replacement
fit_matched_no_r2 <- with(matched_no_rep2,
                          coxph(Surv(time, status) ~ painStatus1998,
                                robust = TRUE#,
                                #weights = scaled_weights
                          ))

# optimal full matching
fit_opt_full <- with(opt_full_matched,
                     coxph(Surv(time, status) ~ painStatus1998,
                           robust = TRUE#,
                           #weights = scaled_weights
                     ))

# weighting
fit_weighted <- with(weighted,
                     coxph(Surv(time, status) ~ painStatus1998,
                           robust = TRUE#,
                           #weights = scaled_weights
                     ))

#-----------------------------------------
# pool results across each imputed dataset

# original data, no PS methods, pain only
pooled_original <- pool(fit_original)
summary_original <- summary(pooled_original, conf.int = TRUE)

# original data, no PS methods, pain age and sex
pooled_original_age_sex <- pool(fit_original_age_sex)
summary_original_age_sex <- summary(pooled_original_age_sex, conf.int = TRUE)

# original data, no PS methods, pain and all confounders
pooled_original_confounders <- pool(fit_original_confounders)
summary_original_confounders <- summary(pooled_original_confounders, conf.int = TRUE)

# 1-to-1 matching with replacement
pooled_matched_r <- pool(fit_matched_r)
summary_matched_r <- summary(pooled_matched_r, conf.int = TRUE)

# 1-to-1 matching without replacement
pooled_matched_no_r <- pool(fit_matched_no_r)
summary_matched_no_r <- summary_matched_no_r <- summary(pooled_matched_no_r, conf.int = TRUE)

# 2-to-1 matching with replacement
pooled_matched_r2 <- pool(fit_matched_r2)
summary_matched_r2 <- summary(pooled_matched_r2, conf.int = TRUE)

# 2-to-1 matching without replacement
pooled_matched_no_r2 <- pool(fit_matched_no_r2)
summary_matched_no_r2 <- summary(pooled_matched_no_r2, conf.int = TRUE)

# optimal full matching
pooled_matched_opt_full <- pool(fit_opt_full)
summary_matched_opt_full <- summary(pooled_matched_opt_full, conf.int = TRUE)

# weighting
pooled_weighted <- pool(fit_weighted)
summary_weighted <- summary(pooled_weighted, conf.int = TRUE)

#-------------------------------------------------------------------------------
# fit model to each imputed dataset - doubly robust/regression-adjusted matching

# 1-to-1 matching with replacement
fit_matched_r_ra <- with(matched_rep,
                         coxph(Surv(time, status) ~ painStatus1998 +
                                 ageIn1998 + I(ageIn1998^2) + gender + race4Cats +
                                 maritalStatus1998 + householdSize1998 +
                                 numChildren1998 + region4Cats1998 + urbanicity1998 +
                                 religionImportance1998 + edu4Cats + wealthQuarts1998 +
                                 jobStatus4Cats1998 + foodSecurity1998 +
                                 veteranStatus + insurance1998 + cancerActiveP1998 +
                                 diabetesP1998 + lungDisP1998 + anginaP1998 +
                                 strokeP1998 + bmi6Cats1998 + hrtCondP1998 +
                                 arthritisP1998 +
                                 smokeStatus1998 + randCESD1998,
                               robust = TRUE#,
                               #weights = scaled_weights
                         ))

# 1-to-1 matching without replacement
fit_matched_no_r_ra <- with(matched_no_rep,
                            coxph(Surv(time, status) ~ painStatus1998 +
                                    ageIn1998 + I(ageIn1998^2) + gender + race4Cats +
                                    maritalStatus1998 + householdSize1998 +
                                    numChildren1998 + region4Cats1998 + urbanicity1998 +
                                    religionImportance1998 + edu4Cats + wealthQuarts1998 +
                                    jobStatus4Cats1998 + foodSecurity1998 +
                                    veteranStatus + insurance1998 + cancerActiveP1998 +
                                    diabetesP1998 + lungDisP1998 + anginaP1998 +
                                    strokeP1998 + bmi6Cats1998 + hrtCondP1998 +
                                    arthritisP1998 +
                                    smokeStatus1998 + randCESD1998,
                                  robust = TRUE#,
                                  #weights = scaled_weights
                            ))

# 2-to-1 matching with replacement
fit_matched_r2_ra <- with(matched_rep2,
                          coxph(Surv(time, status) ~ painStatus1998 +
                                  ageIn1998 + I(ageIn1998^2) + gender + race4Cats +
                                  maritalStatus1998 + householdSize1998 +
                                  numChildren1998 + region4Cats1998 + urbanicity1998 +
                                  religionImportance1998 + edu4Cats + wealthQuarts1998 +
                                  jobStatus4Cats1998 + foodSecurity1998 +
                                  veteranStatus + insurance1998 + cancerActiveP1998 +
                                  diabetesP1998 + lungDisP1998 + anginaP1998 +
                                  strokeP1998 + bmi6Cats1998 + hrtCondP1998 +
                                  arthritisP1998 +
                                  smokeStatus1998 + randCESD1998,
                                robust = TRUE#,
                                #weights = scaled_weights
                          ))

# 2-to-1 matching without replacement
fit_matched_no_r2_ra <- with(matched_no_rep2,
                             coxph(Surv(time, status) ~ painStatus1998 +
                                     ageIn1998 + I(ageIn1998^2) + gender + race4Cats +
                                     maritalStatus1998 + householdSize1998 +
                                     numChildren1998 + region4Cats1998 + urbanicity1998 +
                                     religionImportance1998 + edu4Cats + wealthQuarts1998 +
                                     jobStatus4Cats1998 + foodSecurity1998 +
                                     veteranStatus + insurance1998 + cancerActiveP1998 +
                                     diabetesP1998 + lungDisP1998 + anginaP1998 +
                                     strokeP1998 + bmi6Cats1998 + hrtCondP1998 +
                                     arthritisP1998 +
                                     smokeStatus1998 + randCESD1998,
                                   robust = TRUE#,
                                   #weights = scaled_weights
                             ))

# optimal full matching
fit_matched_opt_full_ra <- with(opt_full_matched,
                                coxph(Surv(time, status) ~ painStatus1998 +
                                        ageIn1998 + I(ageIn1998^2) + gender + race4Cats +
                                        maritalStatus1998 + householdSize1998 +
                                        numChildren1998 + region4Cats1998 + urbanicity1998 +
                                        religionImportance1998 + edu4Cats + wealthQuarts1998 +
                                        jobStatus4Cats1998 + foodSecurity1998 +
                                        veteranStatus + insurance1998 + cancerActiveP1998 +
                                        diabetesP1998 + lungDisP1998 + anginaP1998 +
                                        strokeP1998 + bmi6Cats1998 + hrtCondP1998 +
                                        arthritisP1998 +
                                        smokeStatus1998 + randCESD1998,
                                      robust = TRUE#,
                                      #weights = scaled_weights
                                ))

# weighting
fit_weighted_dr <- with(weighted,
                        coxph(Surv(time, status) ~ painStatus1998 +
                                ageIn1998 + I(ageIn1998^2) + gender + race4Cats +
                                maritalStatus1998 + householdSize1998 +
                                numChildren1998 + region4Cats1998 + urbanicity1998 +
                                religionImportance1998 + edu4Cats + wealthQuarts1998 +
                                jobStatus4Cats1998 + foodSecurity1998 +
                                veteranStatus + insurance1998 + cancerActiveP1998 +
                                diabetesP1998 + lungDisP1998 + anginaP1998 +
                                strokeP1998 + bmi6Cats1998 + hrtCondP1998 +
                                arthritisP1998 +
                                smokeStatus1998 + randCESD1998,
                              robust = TRUE#,
                              #weights = scaled_weights
                        ))

#-----------------------------------------
# pool results across each imputed dataset

# matching with replacement
pooled_matched_r_ra <- pool(fit_matched_r_ra)
summary_matched_r_ra <- summary(pooled_matched_r_ra, conf.int = TRUE)

# matching without replacement
pooled_matched_no_r_ra <- pool(fit_matched_no_r_ra)
summary_matched_no_r_ra <- summary(pooled_matched_no_r_ra, conf.int = TRUE)

# matching with replacement 2:1
pooled_matched_r2_ra <- pool(fit_matched_r2_ra)
summary_matched_r2_ra <- summary(pooled_matched_r2_ra, conf.int = TRUE)

# matching without replacement 2:1
pooled_matched_no_r2_ra <- pool(fit_matched_no_r2_ra)
summary_matched_no_r2_ra <- summary(pooled_matched_no_r2_ra, conf.int = TRUE)

# optimal full matching
pooled_matched_opt_full_ra <- pool(fit_matched_opt_full_ra)
summary_matched_opt_full_ra <- summary(pooled_matched_opt_full_ra, conf.int = TRUE)

# weighting
pooled_weighted_dr <- pool(fit_weighted_dr)
summary_weighted_dr <- summary(pooled_weighted_dr, conf.int = TRUE)


#-------------------------------
# Create results summary

# original data (no PS methods, pain only)
original_sum <- round(exp(c(summary_original$estimate,
                            summary_original$`2.5 %`,
                            summary_original$`97.5 %`)), 2)

# original data (no PS methods, pain, age & sex)
original_age_sex_sum <- round(exp(c(summary_original_age_sex$estimate[summary_original_age_sex$term == "painStatus1998"],
                                    summary_original_age_sex$`2.5 %`[summary_original_age_sex$term == "painStatus1998"],
                                    summary_original_age_sex$`97.5 %`[summary_original_age_sex$term == "painStatus1998"])),
                              2)

# original data (no PS methods, pain, confounders)
original_confounders_sum <- round(exp(c(summary_original_confounders$estimate[summary_original_confounders$term == "painStatus1998"],
                                        summary_original_confounders$`2.5 %`[summary_original_confounders$term == "painStatus1998"],
                                        summary_original_confounders$`97.5 %`[summary_original_confounders$term == "painStatus1998"])),
                                  2)

# matching with replacement (pain only)
matched_r_sum <- round(exp(c(summary_matched_r$estimate,
                             summary_matched_r$`2.5 %`,
                             summary_matched_r$`97.5 %`)), 2)

# matching without replacement (pain only)
matched_no_r_sum <- round(exp(c(summary_matched_no_r$estimate,
                                summary_matched_no_r$`2.5 %`,
                                summary_matched_no_r$`97.5 %`)), 2)

# matching with replacement 2:1 (pain only)
matched_r2_sum <- round(exp(c(summary_matched_r2$estimate,
                              summary_matched_r2$`2.5 %`,
                              summary_matched_r2$`97.5 %`)), 2)

# matching without replacement 2:1 (pain only)
matched_no_r2_sum <- round(exp(c(summary_matched_no_r2$estimate,
                                 summary_matched_no_r2$`2.5 %`,
                                 summary_matched_no_r2$`97.5 %`)), 2)

# optimal full matching (pain only)
matched_opt_full_sum <- round(exp(c(summary_matched_opt_full$estimate,
                                    summary_matched_opt_full$`2.5 %`,
                                    summary_matched_opt_full$`97.5 %`)), 2)

# PS weighting (pain only)
weighted_sum <- round(exp(c(summary_weighted$estimate,
                            summary_weighted$`2.5 %`,
                            summary_weighted$`97.5 %`)), 2)


# matching with replacement (regression adjusted)
matched_r_ra_sum <- round(exp(c(summary_matched_r_ra$estimate[summary_matched_r_ra$term == "painStatus1998"],
                                summary_matched_r_ra$`2.5 %`[summary_matched_r_ra$term == "painStatus1998"],
                                summary_matched_r_ra$`97.5 %`[summary_matched_r_ra$term == "painStatus1998"])), 2)

# matching without replacement (regression adjusted)
matched_no_r_ra_sum <- round(exp(c(summary_matched_no_r_ra$estimate[summary_matched_no_r_ra$term == "painStatus1998"],
                                   summary_matched_no_r_ra$`2.5 %`[summary_matched_no_r_ra$term == "painStatus1998"],
                                   summary_matched_no_r_ra$`97.5 %`[summary_matched_no_r_ra$term == "painStatus1998"])), 2)

# matching with replacement 2:1 (regression adjusted)
matched_r2_ra_sum <- round(exp(c(summary_matched_r2_ra$estimate[summary_matched_r2_ra$term == "painStatus1998"],
                                 summary_matched_r2_ra$`2.5 %`[summary_matched_r2_ra$term == "painStatus1998"],
                                 summary_matched_r2_ra$`97.5 %`[summary_matched_r2_ra$term == "painStatus1998"])), 2)

# matching without replacement 2:1 (regression adjusted)
matched_no_r2_ra_sum <- round(exp(c(summary_matched_no_r2_ra$estimate[summary_matched_no_r2_ra$term == "painStatus1998"],
                                    summary_matched_no_r2_ra$`2.5 %`[summary_matched_no_r2_ra$term == "painStatus1998"],
                                    summary_matched_no_r2_ra$`97.5 %`[summary_matched_no_r2_ra$term == "painStatus1998"])), 2)

# optimal full matching (regression adjusted)
matched_opt_full_ra_sum <- round(exp(c(summary_matched_opt_full_ra$estimate[summary_matched_opt_full_ra$term == "painStatus1998"],
                                       summary_matched_opt_full_ra$`2.5 %`[summary_matched_opt_full_ra$term == "painStatus1998"],
                                       summary_matched_opt_full_ra$`97.5 %`[summary_matched_opt_full_ra$term == "painStatus1998"])), 2)

# PS weighting (doubly robust)
weighted_dr_sum <- round(exp(c(summary_weighted_dr$estimate[summary_weighted_dr$term == "painStatus1998"],
                               summary_weighted_dr$`2.5 %`[summary_weighted_dr$term == "painStatus1998"],
                               summary_weighted_dr$`97.5 %`[summary_weighted_dr$term == "painStatus1998"])), 2)

# create final summary
final_sum <- as.data.frame(rbind(original_sum,
                                 original_age_sex_sum,
                                 original_confounders_sum,
                                 matched_r_sum,
                                 matched_no_r_sum,
                                 matched_r2_sum,
                                 matched_no_r2_sum,
                                 matched_opt_full_sum,
                                 weighted_sum,
                                 matched_r_ra_sum,
                                 matched_no_r_ra_sum,
                                 matched_r2_ra_sum,
                                 matched_no_r2_ra_sum,
                                 matched_opt_full_ra_sum,
                                 weighted_dr_sum))
colnames(final_sum) <- c("Hazard ratio", "95% CI lower", "95% CI higher")
rownames(final_sum) <- c("Original data (no PS methods, pain only)",
                         "Original data (no PS methods, pain, age & sex)",
                         "Original data (no PS methods, pain & confounders)",
                         "1:1 matched with replacement",
                         "1:1 matched without replacement",
                         "2:1 matched with replacement",
                         "2:1 matched without replacement",
                         "Optimal full matching",
                         "Propensity score weighted",
                         "1:1 matched with replacement (regression adjusted)",
                         "1:1 matched without replacement (regression adjusted)",
                         "2:1 matched with replacement (regression adjusted)",
                         "2:1 matched without replacement (regression adjusted)",
                         "Optimal full matching (regression adjusted)",
                         "Propensity score weighted (doubly robust)")

final_sum
beep()

#-------------------------------------------------------------------------------
# Summary statistics across the 20 imputations

#-----------------
# Unadjusted data

# merge all imputed datasets into one dataset
imp_dfs <- rbind(
  complete(imputed, action = 1),
  complete(imputed, action = 2),
  complete(imputed, action = 3),
  complete(imputed, action = 4),
  complete(imputed, action = 5),
  complete(imputed, action = 6),
  complete(imputed, action = 7),
  complete(imputed, action = 8),
  complete(imputed, action = 9),
  complete(imputed, action = 10),
  complete(imputed, action = 11),
  complete(imputed, action = 12),
  complete(imputed, action = 13),
  complete(imputed, action = 14),
  complete(imputed, action = 15),
  complete(imputed, action = 16),
  complete(imputed, action = 17),
  complete(imputed, action = 18),
  complete(imputed, action = 19),
  complete(imputed, action = 20)
)

# create bivariate summary table
imp_dfs_tab <- compareGroups(painStatus1998 ~ ageIn1998 + gender + race4Cats +
                               maritalStatus1998 + householdSize1998 + numChildren1998 +
                               region4Cats1998 + urbanicity1998 + religionImportance1998 +
                               edu4Cats + wealthQuarts1998 + jobStatus4Cats1998 +
                               foodSecurity1998 + veteranStatus + insurance1998 +
                               cancerActiveP1998 + diabetesP1998 + lungDisP1998 +
                               anginaP1998 + strokeP1998 + bmi6Cats1998 + hrtCondP1998 +
                               arthritisP1998 +
                               smokeStatus1998 +
                               randCESD1998,
                             data = imp_dfs)

summary(imp_dfs_tab)

beep()


#------------------------
# Kaplan-Meier curves

# define theme with centered title
centr = theme_bw() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

#################
# unadjusted data

# stratify by 1998 pain status
km_pain_fit1 <- survfit(Surv(time, status) ~ painStatus1998,
                        data = df)
km.plot1 <- ggsurvplot(km_pain_fit1, data = df,
                       legend.title = "1998 pain status",
                       legend.labs = c("None or mild pain", "Moderate or severe pain"),
                       legend = "top",
                       title = "Unadjusted data",
                       conf.int = TRUE,
                       # Add risk table
                       #risk.table = "abs_pct",
                       tables.height = 0.2,
                       tables.theme = theme_cleantable(),
                       fontsize = 4,
                       palette = "viridis",
                       ggtheme = centr
)


##############
# matched data

# extract matched datasets
df_all_matched <- rbind(complete(matched_no_rep, 1, all = FALSE),
                        complete(matched_no_rep, 2, all = FALSE),
                        complete(matched_no_rep, 3, all = FALSE),
                        complete(matched_no_rep, 4, all = FALSE),
                        complete(matched_no_rep, 5, all = FALSE),
                        complete(matched_no_rep, 6, all = FALSE),
                        complete(matched_no_rep, 7, all = FALSE),
                        complete(matched_no_rep, 8, all = FALSE),
                        complete(matched_no_rep, 9, all = FALSE),
                        complete(matched_no_rep, 10, all = FALSE),
                        complete(matched_no_rep, 11, all = FALSE),
                        complete(matched_no_rep, 12, all = FALSE),
                        complete(matched_no_rep, 13, all = FALSE),
                        complete(matched_no_rep, 14, all = FALSE),
                        complete(matched_no_rep, 15, all = FALSE),
                        complete(matched_no_rep, 16, all = FALSE),
                        complete(matched_no_rep, 17, all = FALSE),
                        complete(matched_no_rep, 18, all = FALSE),
                        complete(matched_no_rep, 19, all = FALSE),
                        complete(matched_no_rep, 20, all = FALSE))

# stratify by 1998 pain status
km_pain_fit2 <- survfit(Surv(time, status) ~ painStatus1998,
                        data = df_all_matched)
km.plot2 <- ggsurvplot(km_pain_fit2, data = df_all_matched,
                       #legend.title = "1998 pain status",
                       #legend.labs = c("None or mild pain", "Moderate or severe pain"),
                       legend = "none",
                       title = "Matched data",
                       conf.int = FALSE,
                       # Add risk table
                       #risk.table = "abs_pct",
                       tables.height = 0.2,
                       tables.theme = theme_cleantable(),
                       fontsize = 4,
                       palette = "viridis",
                       ggtheme = centr
)

# create joint plot
km.plots <- ggarrange(km.plot1$plot, km.plot2$plot,
                      ncol = 1, nrow = 2,
                      labels = c("A", "B"),
                      common.legend = TRUE,
                      legend = "bottom")

km.plots
