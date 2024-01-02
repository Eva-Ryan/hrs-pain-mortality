# R code to load the merged, cross-wave HRS file created in the R script
# "1 Select variables and merge waves.R" and clean the variables/create new
# variables
#------------------------------------------------------------------------------

# load packages
library(tidyverse)

# get location of project directory
directory <- getwd()

# load data
df <- read_csv(paste0(directory, "/HRS_1998_to_2018.csv"),
               col_types = cols(sad2wksP2000 = col_double(),
                                cancerTrtmtLast2YrsP2004 = col_double(),
                                sad2wksP2004 = col_double(),
                                sad2wksP2006 = col_double(),
                                cancerTrtmtLast2YrsP2008 = col_double(),
                                cancerTrtmtLast2YrsP2016 = col_double(),
                                cancerTrtmtLast2YrsP2012 = col_double(),
                                htFeet1995 = col_double(),
                                htInches1995 = col_double(),
                                htFeet2000 = col_double(),
                                htInches2000 = col_double(),
                                htFeet2004 = col_double(),
                                htInches2004 = col_double(),
                                everSmokeP2000 = col_double(),
                                everSmokeP2004 = col_double()
               )) # variables were loading in as logicals rather
# than numbers corresponding to categories,
# so specified type in these cases

# remove those with zero weights
df <- df %>%
  filter(fwhy0wgt == 7)


#------------------------------------------------------------------------------
# Participation status variables

#-------------------------
# Main status variable

# description from Hanna Grol-Prokopczyk's Stata .do file on cleaning HRS data:

# "xrescode includes lots of codes for response or non-response outcomes.
# 1 (or 1001), 2, and 5 (or 1005) are complete or partial interviews; all
# others are refusals to participate or non-locations, etc.

# NOTE:  the xrescode vars contain lots of missing values, because you
# need to have been eligible for interview to have a non-missing value
# (and, e.g., Rs who died, or refused in previous waves, are considered
# ineligible).  I create my variables somewhat differently:  if you were not
# interviewed (whether deemed eligible or not), intvwdP is set to 0."

df$intvwdP1998 <- 0
df$intvwdP1998[which(df$frescode == 1 | df$frescode == 2 | df$frescode == 5)] <- 1
df$intvwdP2000 <- 0
df$intvwdP2000[which(df$grescode == 1 | df$grescode == 5)] <- 1
df$intvwdP2002 <- 0
df$intvwdP2002[which(df$hrescode == 1001 | df$hrescode == 1005)] <- 1
df$intvwdP2004 <- 0
df$intvwdP2004[which(df$jrescode == 1001 | df$jrescode == 1005)] <- 1
df$intvwdP2006 <- 0
df$intvwdP2006[which(df$krescode == 1001 | df$krescode == 1005)] <- 1
df$intvwdP2008 <- 0
df$intvwdP2008[which(df$lrescode == 1001 | df$lrescode == 1005)] <- 1
df$intvwdP2010 <- 0
df$intvwdP2010[which(df$mrescode == 1001 | df$mrescode == 1005)] <- 1
df$intvwdP2012 <- 0
df$intvwdP2012[which(df$nrescode == 1001 | df$nrescode == 1005)] <- 1
df$intvwdP2014 <- 0
df$intvwdP2014[which(df$orescode == 1001 | df$orescode == 1005)] <- 1
df$intvwdP2016 <- 0
df$intvwdP2016[which(df$prescode == 1001 | df$prescode == 1005)] <- 1
df$intvwdP2018 <- 0
df$intvwdP2018[which(df$qrescode == 1001 | df$qrescode == 1005 |
                       df$qrescode == 1099)] <- 1

# there are some cases in which person supposedly didn't participate but they
# have pain data anyway - recode these individuals to show they were interviewed
df$intvwdP1998[which(is.na(df$troubledWithPainP1998) == FALSE)] <- 1
df$intvwdP2000[which(is.na(df$troubledWithPainP2000) == FALSE)] <- 1
df$intvwdP2002[which(is.na(df$troubledWithPainP2002) == FALSE)] <- 1
df$intvwdP2004[which(is.na(df$troubledWithPainP2004) == FALSE)] <- 1
df$intvwdP2006[which(is.na(df$troubledWithPainP2006) == FALSE)] <- 1
df$intvwdP2008[which(is.na(df$troubledWithPainP2008) == FALSE)] <- 1
df$intvwdP2010[which(is.na(df$troubledWithPainP2010) == FALSE)] <- 1
df$intvwdP2012[which(is.na(df$troubledWithPainP2012) == FALSE)] <- 1
df$intvwdP2014[which(is.na(df$troubledWithPainP2014) == FALSE)] <- 1
df$intvwdP2016[which(is.na(df$troubledWithPainP2016) == FALSE)] <- 1
df$intvwdP2018[which(is.na(df$troubledWithPainP2018) == FALSE)] <- 1

#--------------------
# Death flag

# xalive is 1 or 2 if alive or presumed alive, and 5 or 6 if dead, where x is
# the letter corresponding to each wave e.g., f for 1998 Wave
# There are lots of missing values for the xalive variables.
df$deadP1998 <- NA
df$deadP1998[which(df$falive == 1 | df$falive == 2)] <- 0
df$deadP1998[which(df$falive == 5 | df$falive == 6)] <- 1
df$deadP2000 <- NA
df$deadP2000[which(df$galive == 1 | df$galive == 2)] <- 0
df$deadP2000[which(df$galive == 5 | df$galive == 6)] <- 1
df$deadP2002 <- NA
df$deadP2002[which(df$halive == 1 | df$halive == 2)] <- 0
df$deadP2002[which(df$halive == 5 | df$halive == 6)] <- 1
df$deadP2004 <- NA
df$deadP2004[which(df$jalive == 1 | df$jalive == 2)] <- 0
df$deadP2004[which(df$jalive == 5 | df$jalive == 6)] <- 1
df$deadP2006 <- NA
df$deadP2006[which(df$kalive == 1 | df$kalive == 2)] <- 0
df$deadP2006[which(df$kalive == 5 | df$kalive == 6)] <- 1
df$deadP2008 <- NA
df$deadP2008[which(df$lalive == 1 | df$lalive == 2)] <- 0
df$deadP2008[which(df$lalive == 5 | df$lalive == 6)] <- 1
df$deadP2010 <- NA
df$deadP2010[which(df$malive == 1 | df$malive == 2)] <- 0
df$deadP2010[which(df$malive == 5 | df$malive == 6)] <- 1
df$deadP2012 <- NA
df$deadP2012[which(df$nalive == 1 | df$nalive == 2)] <- 0
df$deadP2012[which(df$nalive == 5 | df$nalive == 6)] <- 1
df$deadP2014 <- NA
df$deadP2014[which(df$oalive == 1 | df$oalive == 2)] <- 0
df$deadP2014[which(df$oalive == 5 | df$oalive == 6)] <- 1
df$deadP2016 <- NA
df$deadP2016[which(df$palive == 1 | df$palive == 2)] <- 0
df$deadP2016[which(df$palive == 5 | df$palive == 6)] <- 1
df$deadP2018 <- NA
df$deadP2018[which(df$qalive == 1 | df$qalive == 2)] <- 0
df$deadP2018[which(df$qalive == 5 | df$qalive == 6)] <- 1

# there are some cases in which a person is missing xalive status
# but they have pain data - correct:
df$deadP1998[which(is.na(df$troubledWithPainP1998) == FALSE)] <- 0
df$deadP2000[which(is.na(df$troubledWithPainP2000) == FALSE)] <- 0
df$deadP2002[which(is.na(df$troubledWithPainP2002) == FALSE)] <- 0
df$deadP2004[which(is.na(df$troubledWithPainP2004) == FALSE)] <- 0
df$deadP2006[which(is.na(df$troubledWithPainP2006) == FALSE)] <- 0
df$deadP2008[which(is.na(df$troubledWithPainP2008) == FALSE)] <- 0
df$deadP2010[which(is.na(df$troubledWithPainP2010) == FALSE)] <- 0
df$deadP2012[which(is.na(df$troubledWithPainP2012) == FALSE)] <- 0
df$deadP2014[which(is.na(df$troubledWithPainP2014) == FALSE)] <- 0
df$deadP2016[which(is.na(df$troubledWithPainP2016) == FALSE)] <- 0
df$deadP2018[which(is.na(df$troubledWithPainP2018) == FALSE)] <- 0

# check for errors between each pair of waves - all ok
table(df$deadP1998, df$deadP2000)
table(df$deadP2000, df$deadP2002)
table(df$deadP2002, df$deadP2004)
table(df$deadP2004, df$deadP2006)
table(df$deadP2006, df$deadP2008)
table(df$deadP2008, df$deadP2010)
table(df$deadP2010, df$deadP2012)
table(df$deadP2012, df$deadP2014)
table(df$deadP2014, df$deadP2016)
table(df$deadP2016, df$deadP2018)


#------------------------------------------------------------------------------
# Sociodemographic / SES variables

#----------------------
# Gender
df <- df %>%
  mutate(gender = factor(gender, levels = c(1,2), labels = c("male", "female")))

#----------------------
# Age in 1998
df <- df %>%
  mutate(birthYear = ifelse(birthYear == 0, NA, birthYear)) %>%
  mutate(ageIn1998 = 1998 - birthYear) %>%
  # create age categories
  mutate(ageIn1998ByDecade = factor(
    cut(ageIn1998, breaks = c(0, 39, 49, 59, 69, 79, 89, Inf)),
    labels = c("Under 40", "40s (40-49)", "50s (50-59)", "60s (60-69)",
               "70s (71-79)", "80s (80-89)", "90 plus")))

########## NB ###########
# select just those aged 51+ in 1998
df <- df %>%
  filter(ageIn1998 >= 51)


#----------------------
# Race

# description from Hanna Grol-Prokopczyk's Stata .do file on cleaning HRS data:

# "Plan:  first create these vars based on 2014 Tracker info (i.e., using
# vars "hispanic" and "race").  Then, where available, overwrite these vars
# with the detail made available in the 2006 demographic module (since that's
# when Rs could specify multiple races for the first time)."

df <- df %>%
  # pool type-unknown, Mexican, and other Hispanic (0-2)
  mutate(hispPTemp = car::recode(hispanic,
                                 "0 = NA;
                                 1 = 1;
                                 2 = 1;
                                 3 = 1;
                                 5 = 0")) %>%
  # rename to avoid confusion
  rename(hispanic_frTracker = hispanic)

# create new race variable
df <- df %>%
  mutate(race4Cats = car::recode(race,
                                 "0 = NA;
                                 1 = 1;
                                 2 = 2;
                                 7 = 4")) %>%
  # rename to avoid confusion
  rename(race_frTracker = race)

# add Hispanic
df$race4Cats[df$hispPTemp == 1] <- 3

# update using 2006 values
df$race4Cats[which(df$kb091m2006 == 1)] <- 1
df$race4Cats[which(df$kb091m2006 == 2)] <- 2
df$race4Cats[which(df$kb091m2006 == 97)] <- 4
df$race4Cats[which(df$kb0282006 == 1)] <- 3
df$hispPTemp[which(df$kb0282006 == 1)] <- 1

# make a factor variable and label categories
df <- df %>%
  mutate(race4Cats = factor(race4Cats, levels = c(1, 2, 3, 4),
                            labels = c("White (non-Hispanic)",
                                       "Black (non-Hispanic)",
                                       "Hispanic",
                                       "Other (non-Hispanic)")))

#-----------------------
# Education

# recode "unknown degree" to NA in original variable
df <- df %>%
  mutate(degree = car::recode(degree, "9 = NA"))

# create new education category variable
df <- df %>%
  mutate(edu4Cats = car::recode(degree,
                                "0 = 1;
                                1 = 2;
                                2 = 2;
                                3 = 2;
                                4 = 3;
                                5 = 4;
                                6 = 4")) %>%
  # convert to factor and label categories
  mutate(edu4Cats = factor(edu4Cats, levels = c(1, 2, 3, 4),
                           labels = c("No degree", "High school degree",
                                      "4-year college degree",
                                      "Graduate degree")))

#-----------------------
# Income

# create income quartile categorical variable for each year
df <- df %>%
  mutate(hhIncQuartiles1998 = cut(hhInc1998,
                                  breaks = (quantile(hhInc1998,
                                                     c(0, 0.25, 0.5, 0.75, 1),
                                                     na.rm = TRUE)),
                                  labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(hhIncQuartiles2000 = cut(hhInc2000,
                                  breaks = (quantile(hhInc2000,
                                                     c(0, 0.25, 0.5, 0.75, 1),
                                                     na.rm = TRUE)),
                                  labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(hhIncQuartiles2002 = cut(hhInc2002,
                                  breaks = (quantile(hhInc2002,
                                                     c(0, 0.25, 0.5, 0.75, 1),
                                                     na.rm = TRUE)),
                                  labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(hhIncQuartiles2004 = cut(hhInc2004,
                                  breaks = (quantile(hhInc2004,
                                                     c(0, 0.25, 0.5, 0.75, 1),
                                                     na.rm = TRUE)),
                                  labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(hhIncQuartiles2006 = cut(hhInc2006,
                                  breaks = (quantile(hhInc2006,
                                                     c(0, 0.25, 0.5, 0.75, 1),
                                                     na.rm = TRUE)),
                                  labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(hhIncQuartiles2008 = cut(hhInc2008,
                                  breaks = (quantile(hhInc2008,
                                                     c(0, 0.25, 0.5, 0.75, 1),
                                                     na.rm = TRUE)),
                                  labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(hhIncQuartiles2010 = cut(hhInc2010,
                                  breaks = (quantile(hhInc2010,
                                                     c(0, 0.25, 0.5, 0.75, 1),
                                                     na.rm = TRUE)),
                                  labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(hhIncQuartiles2012 = cut(hhInc2012,
                                  breaks = (quantile(hhInc2012,
                                                     c(0, 0.25, 0.5, 0.75, 1),
                                                     na.rm = TRUE)),
                                  labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(hhIncQuartiles2014 = cut(hhInc2014,
                                  breaks = (quantile(hhInc2014,
                                                     c(0, 0.25, 0.5, 0.75, 1),
                                                     na.rm = TRUE)),
                                  labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(hhIncQuartiles2016 = cut(hhInc2016,
                                  breaks = (quantile(hhInc2016,
                                                     c(0, 0.25, 0.5, 0.75, 1),
                                                     na.rm = TRUE)),
                                  labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(hhIncQuartiles2018 = cut(hhInc2018,
                                  breaks = (quantile(hhInc2018,
                                                     c(0, 0.25, 0.5, 0.75, 1),
                                                     na.rm = TRUE)),
                                  labels = c("Q1", "Q2", "Q3", "Q4")))

#-----------------------
# Wealth

# create wealth quartile categorical variable for each year
df <- df %>%
  mutate(wealthQuarts1998 = cut(wealth1998,
                                breaks = (quantile(wealth1998,
                                                   c(0, 0.25, 0.5, 0.75, 1),
                                                   na.rm = TRUE)),
                                labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(wealthQuarts2000 = cut(wealth2000,
                                breaks = (quantile(wealth2000,
                                                   c(0, 0.25, 0.5, 0.75, 1),
                                                   na.rm = TRUE)),
                                labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(wealthQuarts2002 = cut(wealth2002,
                                breaks = (quantile(wealth2002,
                                                   c(0, 0.25, 0.5, 0.75, 1),
                                                   na.rm = TRUE)),
                                labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(wealthQuarts2004 = cut(wealth2004,
                                breaks = (quantile(wealth2004,
                                                   c(0, 0.25, 0.5, 0.75, 1),
                                                   na.rm = TRUE)),
                                labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(wealthQuarts2006 = cut(wealth2006,
                                breaks = (quantile(wealth2006,
                                                   c(0, 0.25, 0.5, 0.75, 1),
                                                   na.rm = TRUE)),
                                labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(wealthQuarts2008 = cut(wealth2008,
                                breaks = (quantile(wealth2008,
                                                   c(0, 0.25, 0.5, 0.75, 1),
                                                   na.rm = TRUE)),
                                labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(wealthQuarts2010 = cut(wealth2010,
                                breaks = (quantile(wealth2010,
                                                   c(0, 0.25, 0.5, 0.75, 1),
                                                   na.rm = TRUE)),
                                labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(wealthQuarts2012 = cut(wealth2012,
                                breaks = (quantile(wealth2012,
                                                   c(0, 0.25, 0.5, 0.75, 1),
                                                   na.rm = TRUE)),
                                labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(wealthQuarts2014 = cut(wealth2014,
                                breaks = (quantile(wealth2014,
                                                   c(0, 0.25, 0.5, 0.75, 1),
                                                   na.rm = TRUE)),
                                labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(wealthQuarts2016 = cut(wealth2016,
                                breaks = (quantile(wealth2016,
                                                   c(0, 0.25, 0.5, 0.75, 1),
                                                   na.rm = TRUE)),
                                labels = c("Q1", "Q2", "Q3", "Q4"))) %>%
  mutate(wealthQuarts2018 = cut(wealth2018,
                                breaks = (quantile(wealth2018,
                                                   c(0, 0.25, 0.5, 0.75, 1),
                                                   na.rm = TRUE)),
                                labels = c("Q1", "Q2", "Q3", "Q4")))

#----------------------------
# Census region of residence

# create vector of factor level names for census region
census_region <- c("New England", "Mid-Atlantic", "East North Central",
                   "West North Central", "South Atlantic", "East South Central",
                   "West South Central", "Mountain", "Pacific")

# create vector of factor level names for 4 category region variable
region4 <- c("Northeast", "Mid-west", "South", "West")

# tidy up census region variable
df <- df %>%
  mutate(region1998 = factor(
    ifelse(region1998 >= 1 & region1998 <= 9, region1998, NA),
    levels = c(1:9), labels = census_region)) %>%
  mutate(region2000 = factor(
    ifelse(region2000 >= 1 & region2000 <= 9, region2000, NA),
    levels = c(1:9), labels = census_region)) %>%
  mutate(region2002 = factor(
    ifelse(region2002 >= 1 & region2002 <= 9, region2002, NA),
    levels = c(1:9), labels = census_region)) %>%
  mutate(region2004 = factor(
    ifelse(region2004 >= 1 & region2004 <= 9, region2004, NA),
    levels = c(1:9), labels = census_region)) %>%
  mutate(region2006 = factor(
    ifelse(region2006 >= 1 & region2006 <= 9, region2006, NA),
    levels = c(1:9), labels = census_region)) %>%
  mutate(region2008 = factor(
    ifelse(region2008 >= 1 & region2008 <= 9, region2008, NA),
    levels = c(1:9), labels = census_region)) %>%
  mutate(region2010 = factor(
    ifelse(region2010 >= 1 & region2010 <= 9, region2010, NA),
    levels = c(1:9), labels = census_region)) %>%
  mutate(region2012 = factor(
    ifelse(region2012 >= 1 & region2012 <= 9, region2012, NA),
    levels = c(1:9), labels = census_region)) %>%
  mutate(region2014 = factor(
    ifelse(region2014 >= 1 & region2014 <= 9, region2014, NA),
    levels = c(1:9), labels = census_region)) %>%
  mutate(region2016 = factor(
    ifelse(region2016 >= 1 & region2016 <= 9, region2016, NA),
    levels = c(1:9), labels = census_region)) %>%
  mutate(region2018 = factor(
    ifelse(region2018 >= 1 & region2018 <= 9, region2018, NA),
    levels = c(1:9), labels = census_region))

# recode to a 4 category variable
df <- df %>%
  mutate(region4Cats1998 = factor(car::recode(region1998,
                                              "'New England' = 1;
                                              'Mid-Atlantic' = 1;
                                              'East North Central' = 2;
                                              'West North Central' = 2;
                                              'South Atlantic' = 3;
                                              'East South Central' = 3;
                                              'West South Central' = 3;
                                              'Mountain' = 4;
                                              'Pacific' = 4"),
                                  levels = c(1:4), labels = region4)) %>%
  mutate(region4Cats2000 = factor(car::recode(region2000,
                                              "'New England' = 1;
                                              'Mid-Atlantic' = 1;
                                              'East North Central' = 2;
                                              'West North Central' = 2;
                                              'South Atlantic' = 3;
                                              'East South Central' = 3;
                                              'West South Central' = 3;
                                              'Mountain' = 4;
                                              'Pacific' = 4"),
                                  levels = c(1:4), labels = region4)) %>%
  mutate(region4Cats2002 = factor(car::recode(region2002,
                                              "'New England' = 1;
                                              'Mid-Atlantic' = 1;
                                              'East North Central' = 2;
                                              'West North Central' = 2;
                                              'South Atlantic' = 3;
                                              'East South Central' = 3;
                                              'West South Central' = 3;
                                              'Mountain' = 4;
                                              'Pacific' = 4"),
                                  levels = c(1:4), labels = region4)) %>%
  mutate(region4Cats2004 = factor(car::recode(region2004,
                                              "'New England' = 1;
                                              'Mid-Atlantic' = 1;
                                              'East North Central' = 2;
                                              'West North Central' = 2;
                                              'South Atlantic' = 3;
                                              'East South Central' = 3;
                                              'West South Central' = 3;
                                              'Mountain' = 4;
                                              'Pacific' = 4"),
                                  levels = c(1:4), labels = region4)) %>%
  mutate(region4Cats2006 = factor(car::recode(region2006,
                                              "'New England' = 1;
                                              'Mid-Atlantic' = 1;
                                              'East North Central' = 2;
                                              'West North Central' = 2;
                                              'South Atlantic' = 3;
                                              'East South Central' = 3;
                                              'West South Central' = 3;
                                              'Mountain' = 4;
                                              'Pacific' = 4"),
                                  levels = c(1:4), labels = region4)) %>%
  mutate(region4Cats2008 = factor(car::recode(region2008,
                                              "'New England' = 1;
                                              'Mid-Atlantic' = 1;
                                              'East North Central' = 2;
                                              'West North Central' = 2;
                                              'South Atlantic' = 3;
                                              'East South Central' = 3;
                                              'West South Central' = 3;
                                              'Mountain' = 4;
                                              'Pacific' = 4"),
                                  levels = c(1:4), labels = region4)) %>%
  mutate(region4Cats2010 = factor(car::recode(region2010,
                                              "'New England' = 1;
                                              'Mid-Atlantic' = 1;
                                              'East North Central' = 2;
                                              'West North Central' = 2;
                                              'South Atlantic' = 3;
                                              'East South Central' = 3;
                                              'West South Central' = 3;
                                              'Mountain' = 4;
                                              'Pacific' = 4"),
                                  levels = c(1:4), labels = region4)) %>%
  mutate(region4Cats2012 = factor(car::recode(region2012,
                                              "'New England' = 1;
                                              'Mid-Atlantic' = 1;
                                              'East North Central' = 2;
                                              'West North Central' = 2;
                                              'South Atlantic' = 3;
                                              'East South Central' = 3;
                                              'West South Central' = 3;
                                              'Mountain' = 4;
                                              'Pacific' = 4"),
                                  levels = c(1:4), labels = region4)) %>%
  mutate(region4Cats2014 = factor(car::recode(region2014,
                                              "'New England' = 1;
                                              'Mid-Atlantic' = 1;
                                              'East North Central' = 2;
                                              'West North Central' = 2;
                                              'South Atlantic' = 3;
                                              'East South Central' = 3;
                                              'West South Central' = 3;
                                              'Mountain' = 4;
                                              'Pacific' = 4"),
                                  levels = c(1:4), labels = region4)) %>%
  mutate(region4Cats2016 = factor(car::recode(region2016,
                                              "'New England' = 1;
                                              'Mid-Atlantic' = 1;
                                              'East North Central' = 2;
                                              'West North Central' = 2;
                                              'South Atlantic' = 3;
                                              'East South Central' = 3;
                                              'West South Central' = 3;
                                              'Mountain' = 4;
                                              'Pacific' = 4"),
                                  levels = c(1:4), labels = region4)) %>%
  mutate(region4Cats2018 = factor(car::recode(region2018,
                                              "'New England' = 1;
                                              'Mid-Atlantic' = 1;
                                              'East North Central' = 2;
                                              'West North Central' = 2;
                                              'South Atlantic' = 3;
                                              'East South Central' = 3;
                                              'West South Central' = 3;
                                              'Mountain' = 4;
                                              'Pacific' = 4"),
                                  levels = c(1:4), labels = region4))


#----------------------------
# Urbanicity

# create vector of factor level names for urbanicity
urbanicity <- c("Urban", "Suburban", "Ex-urban/rural")

df <- df %>%
  mutate(urbanicity1998 = factor(
    ifelse(urbanicity1998 >= 1 & urbanicity1998 <= 3, urbanicity1998, NA),
    levels = c(1,2,3), labels = urbanicity)) %>%
  mutate(urbanicity2000 = factor(
    ifelse(urbanicity2000 >= 1 & urbanicity2000 <= 3, urbanicity2000, NA),
    levels = c(1,2,3), labels = urbanicity)) %>%
  mutate(urbanicity2002 = factor(
    ifelse(urbanicity2002 >= 1 & urbanicity2002 <= 3, urbanicity2002, NA),
    levels = c(1,2,3), labels = urbanicity)) %>%
  mutate(urbanicity2004 = factor(
    ifelse(urbanicity2004 >= 1 & urbanicity2004 <= 3, urbanicity2004, NA),
    levels = c(1,2,3), labels = urbanicity)) %>%
  mutate(urbanicity2006 = factor(
    ifelse(urbanicity2006 >= 1 & urbanicity2006 <= 3, urbanicity2006, NA),
    levels = c(1,2,3), labels = urbanicity)) %>%
  mutate(urbanicity2008 = factor(
    ifelse(urbanicity2008 >= 1 & urbanicity2008 <= 3, urbanicity2008, NA),
    levels = c(1,2,3), labels = urbanicity)) %>%
  mutate(urbanicity2010 = factor(
    ifelse(urbanicity2010 >= 1 & urbanicity2010 <= 3, urbanicity2010, NA),
    levels = c(1,2,3), labels = urbanicity)) %>%
  mutate(urbanicity2012 = factor(
    ifelse(urbanicity2012 >= 1 & urbanicity2012 <= 3, urbanicity2012, NA),
    levels = c(1,2,3), labels = urbanicity)) %>%
  mutate(urbanicity2014 = factor(
    ifelse(urbanicity2014 >= 1 & urbanicity2014 <= 3, urbanicity2014, NA),
    levels = c(1,2,3), labels = urbanicity)) %>%
  mutate(urbanicity2016 = factor(
    ifelse(urbanicity2016 >= 1 & urbanicity2016 <= 3, urbanicity2016, NA),
    levels = c(1,2,3), labels = urbanicity)) %>%
  mutate(urbanicity2018 = factor(
    ifelse(urbanicity2018 >= 1 & urbanicity2018 <= 3, urbanicity2018, NA),
    levels = c(1,2,3), labels = urbanicity))

#----------------------------
# Marital status

# create vector of marital status categories
marital_status <- c("Married", "Separated/Divorced", "Widowed", "Never married",
                    "Other")

# recode variable
df <- df %>%
  mutate(maritalStatus1998 = ifelse(maritalStatus1998 >= 1 &
                                      maritalStatus1998 <= 7,
                                    maritalStatus1998, NA)) %>%
  mutate(maritalStatus1998 = car::recode(maritalStatus1998,
                                         "1 = 1; 3 = 2; 4 = 2; 5 = 3; 6 = 4;
                                         7 = 5")) %>%
  mutate(maritalStatus2000 = ifelse(maritalStatus2000 >= 1 &
                                      maritalStatus2000 <= 7,
                                    maritalStatus2000, NA)) %>%
  mutate(maritalStatus2000 = car::recode(maritalStatus2000,
                                         "1 = 1; 3 = 2; 4 = 2; 5 = 3; 6 = 4;
                                         7 = 5")) %>%
  mutate(maritalStatus2002 = ifelse(maritalStatus2002 >= 1 &
                                      maritalStatus2002 <= 7,
                                    maritalStatus2002, NA)) %>%
  mutate(maritalStatus2002 = car::recode(maritalStatus2002,
                                         "1 = 1; 3 = 2; 4 = 2; 5 = 3; 6 = 4;
                                         7 = 5")) %>%
  mutate(maritalStatus2004 = ifelse(maritalStatus2004 >= 1 &
                                      maritalStatus2004 <= 7,
                                    maritalStatus2004, NA)) %>%
  # Annulled became a category (2) from 2004 on - recode to the
  # "Seperated/Divorced" category
  mutate(maritalStatus2004 = car::recode(maritalStatus2004,
                                         "1 = 1; 2 = 2; 3 = 2; 4 = 2; 5 = 3;
                                         6 = 4; 7 = 5")) %>%
  mutate(maritalStatus2006 = ifelse(maritalStatus2006 >= 1 &
                                      maritalStatus2006 <= 7,
                                    maritalStatus2006, NA)) %>%
  mutate(maritalStatus2006 = car::recode(maritalStatus2006,
                                         "1 = 1; 2 = 2; 3 = 2; 4 = 2; 5 = 3;
                                         6 = 4; 7 = 5")) %>%
  mutate(maritalStatus2008 = ifelse(maritalStatus2008 >= 1 &
                                      maritalStatus2008 <= 7,
                                    maritalStatus2008, NA)) %>%
  mutate(maritalStatus2008 = car::recode(maritalStatus2008,
                                         "1 = 1; 2 = 2; 3 = 2; 4 = 2; 5 = 3;
                                         6 = 4; 7 = 5")) %>%
  mutate(maritalStatus2010 = ifelse(maritalStatus2010 >= 1 &
                                      maritalStatus2010 <= 7,
                                    maritalStatus2010, NA)) %>%
  mutate(maritalStatus2010 = car::recode(maritalStatus2010,
                                         "1 = 1; 2 = 2; 3 = 2; 4 = 2; 5 = 3;
                                         6 = 4; 7 = 5")) %>%
  mutate(maritalStatus2012 = ifelse(maritalStatus2012 >= 1 &
                                      maritalStatus2012 <= 7,
                                    maritalStatus2012, NA)) %>%
  mutate(maritalStatus2012 = car::recode(maritalStatus2012,
                                         "1 = 1; 2 = 2; 3 = 2; 4 = 2; 5 = 3;
                                         6 = 4; 7 = 5")) %>%
  mutate(maritalStatus2014 = ifelse(maritalStatus2014 >= 1 &
                                      maritalStatus2014 <= 7,
                                    maritalStatus2014, NA)) %>%
  mutate(maritalStatus2014 = car::recode(maritalStatus2014,
                                         "1 = 1; 2 = 2; 3 = 2; 4 = 2; 5 = 3;
                                         6 = 4; 7 = 5")) %>%
  mutate(maritalStatus2016 = ifelse(maritalStatus2016 >= 1 &
                                      maritalStatus2016 <= 7,
                                    maritalStatus2016, NA)) %>%
  mutate(maritalStatus2016 = car::recode(maritalStatus2016,
                                         "1 = 1; 2 = 2; 3 = 2; 4 = 2; 5 = 3;
                                         6 = 4; 7 = 5")) %>%
  mutate(maritalStatus2018 = ifelse(maritalStatus2018 >= 1 &
                                      maritalStatus2018 <= 7,
                                    maritalStatus2018, NA)) %>%
  mutate(maritalStatus2018 = car::recode(maritalStatus2018,
                                         "1 = 1; 2 = 2; 3 = 2; 4 = 2; 5 = 3;
                                         6 = 4; 7 = 5"))

# factor variable
df <- df %>%
  mutate(maritalStatus1998 = factor(maritalStatus1998,
                                    levels = c(1:5), labels = marital_status)) %>%
  mutate(maritalStatus2000 = factor(maritalStatus2000,
                                    levels = c(1:5), labels = marital_status)) %>%
  mutate(maritalStatus2002 = factor(maritalStatus2002,
                                    levels = c(1:5), labels = marital_status)) %>%
  mutate(maritalStatus2004 = factor(maritalStatus2004,
                                    levels = c(1:5), labels = marital_status)) %>%
  mutate(maritalStatus2006 = factor(maritalStatus2006,
                                    levels = c(1:5), labels = marital_status)) %>%
  mutate(maritalStatus2008 = factor(maritalStatus2008,
                                    levels = c(1:5), labels = marital_status)) %>%
  mutate(maritalStatus2010 = factor(maritalStatus2010,
                                    levels = c(1:5), labels = marital_status)) %>%
  mutate(maritalStatus2012 = factor(maritalStatus2012,
                                    levels = c(1:5), labels = marital_status)) %>%
  mutate(maritalStatus2014 = factor(maritalStatus2014,
                                    levels = c(1:5), labels = marital_status)) %>%
  mutate(maritalStatus2016 = factor(maritalStatus2016,
                                    levels = c(1:5), labels = marital_status)) %>%
  mutate(maritalStatus2018 = factor(maritalStatus2018,
                                    levels = c(1:5), labels = marital_status))

#----------------------------
# Household size

summary(as.factor(df$householdSize1998)) # no cleaning needed
summary(as.factor(df$householdSize2000))
summary(as.factor(df$householdSize2002))
summary(as.factor(df$householdSize2004))
summary(as.factor(df$householdSize2006))
summary(as.factor(df$householdSize2008))
summary(as.factor(df$householdSize2010))
summary(as.factor(df$householdSize2012))
summary(as.factor(df$householdSize2014))
summary(as.factor(df$householdSize2016))
summary(as.factor(df$householdSize2018))


#----------------------------
# Number of children

summary(as.factor(df$numChildren1998)) # no cleaning needed
summary(as.factor(df$numChildren2000))
summary(as.factor(df$numChildren2002))
summary(as.factor(df$numChildren2004))
summary(as.factor(df$numChildren2006))
summary(as.factor(df$numChildren2008))
summary(as.factor(df$numChildren2010))
summary(as.factor(df$numChildren2012))
summary(as.factor(df$numChildren2014))
summary(as.factor(df$numChildren2016))
summary(as.factor(df$numChildren2018))

#----------------------------
# Religion

# create vector of factor level names for importance of religion
imp_religion <- c("Very important", "Somewhat important", "Not too important")

df <- df %>%
  mutate(religionImportance1998 = factor(
    ifelse(religionImportance1998 >= 1 & religionImportance1998 <= 5,
           religionImportance1998, NA),
    levels = c(1,3,5), labels = imp_religion)) %>%
  mutate(religionImportance2000 = factor(
    ifelse(religionImportance2000 >= 1 & religionImportance2000 <= 5,
           religionImportance2000, NA),
    levels = c(1,3,5), labels = imp_religion)) %>%
  mutate(religionImportance2002 = factor(
    ifelse(religionImportance2002 >= 1 & religionImportance2002 <= 5,
           religionImportance2002, NA),
    levels = c(1,3,5), labels = imp_religion)) %>%
  mutate(religionImportance2004 = factor(
    ifelse(religionImportance2004 >= 1 & religionImportance2004 <= 5,
           religionImportance2004, NA),
    levels = c(1,3,5), labels = imp_religion)) %>%
  mutate(religionImportance2006 = factor(
    ifelse(religionImportance2006 >= 1 & religionImportance2006 <= 5,
           religionImportance2006, NA),
    levels = c(1,3,5), labels = imp_religion)) %>%
  mutate(religionImportance2008 = factor(
    ifelse(religionImportance2008 >= 1 & religionImportance2008 <= 5,
           religionImportance2008, NA),
    levels = c(1,3,5), labels = imp_religion)) %>%
  mutate(religionImportance2010 = factor(
    ifelse(religionImportance2010 >= 1 & religionImportance2010 <= 5,
           religionImportance2010, NA),
    levels = c(1,3,5), labels = imp_religion)) %>%
  mutate(religionImportance2012 = factor(
    ifelse(religionImportance2012 >= 1 & religionImportance2012 <= 5,
           religionImportance2012, NA),
    levels = c(1,3,5), labels = imp_religion))


#----------------------------
# Current employment status

# create vector of current employment categories
employment_status <- c("Employed", "Unemployed", "Retired", "Not in labour force")

# recode to a 4 category variable:
# Employed = working now
# Unemployed = unemployed and looking for work / temporarily laid off, on sick
# or other leave / disabled / homemaker / other
# Retired = retired
df <- df %>%
  mutate(jobStatus1998 = ifelse(jobStatus1998 >= 1 & jobStatus1998 <= 7,
                                jobStatus1998, NA)) %>%
  mutate(jobStatus4Cats1998 = factor(car::recode(jobStatus1998,
                                                 "1 = 1;
                                                 2 = 2; 3 = 2; 7 = 2;
                                                 5 = 3;
                                                 4 = 4; 6 = 4"),
                                     levels = c(1:4), labels = employment_status)) %>%
  mutate(jobStatus2000 = ifelse(jobStatus2000 >= 1 & jobStatus2000 <= 7,
                                jobStatus2000, NA)) %>%
  mutate(jobStatus4Cats2000 = factor(car::recode(jobStatus2000,
                                                 "1 = 1;
                                                 2 = 2; 3 = 2; 7 = 2;
                                                 5 = 3;
                                                 4 = 4; 6 = 4"),
                                     levels = c(1:4), labels = employment_status)) %>%
  mutate(jobStatus2002 = ifelse(jobStatus2002 >= 1 & jobStatus2002 <= 7,
                                jobStatus2002, NA)) %>%
  mutate(jobStatus4Cats2002 = factor(car::recode(jobStatus2002,
                                                 "1 = 1;
                                                 2 = 2; 3 = 2; 7 = 2;
                                                 5 = 3;
                                                 4 = 4; 6 = 4"),
                                     levels = c(1:4), labels = employment_status)) %>%
  mutate(jobStatus2004 = ifelse(jobStatus2004 >= 1 & jobStatus2004 <= 7,
                                jobStatus2004, NA)) %>%
  mutate(jobStatus4Cats2004 = factor(car::recode(jobStatus2004,
                                                 "1 = 1;
                                                 2 = 2; 3 = 2; 7 = 2;
                                                 5 = 3;
                                                 4 = 4; 6 = 4"),
                                     levels = c(1:4), labels = employment_status)) %>%
  mutate(jobStatus2006 = ifelse(jobStatus2006 >= 1 & jobStatus2006 <= 7,
                                jobStatus2006, NA)) %>%
  mutate(jobStatus4Cats2006 = factor(car::recode(jobStatus2006,
                                                 "1 = 1;
                                                 2 = 2; 3 = 2; 7 = 2;
                                                 5 = 3;
                                                 4 = 4; 6 = 4"),
                                     levels = c(1:4), labels = employment_status)) %>%
  mutate(jobStatus2008 = ifelse(jobStatus2008 >= 1 & jobStatus2008 <= 7,
                                jobStatus2008, NA)) %>%
  mutate(jobStatus4Cats2008 = factor(car::recode(jobStatus2008,
                                                 "1 = 1;
                                                 2 = 2; 3 = 2; 7 = 2;
                                                 5 = 3;
                                                 4 = 4; 6 = 4"),
                                     levels = c(1:4), labels = employment_status)) %>%
  mutate(jobStatus2010 = ifelse(jobStatus2010 >= 1 & jobStatus2010 <= 7,
                                jobStatus2010, NA)) %>%
  mutate(jobStatus4Cats2010 = factor(car::recode(jobStatus2010,
                                                 "1 = 1;
                                                 2 = 2; 3 = 2; 7 = 2;
                                                 5 = 3;
                                                 4 = 4; 6 = 4"),
                                     levels = c(1:4), labels = employment_status)) %>%
  mutate(jobStatus2012 = ifelse(jobStatus2012 >= 1 & jobStatus2012 <= 7,
                                jobStatus2012, NA)) %>%
  mutate(jobStatus4Cats2012 = factor(car::recode(jobStatus2012,
                                                 "1 = 1;
                                                 2 = 2; 3 = 2; 7 = 2;
                                                 5 = 3;
                                                 4 = 4; 6 = 4"),
                                     levels = c(1:4), labels = employment_status)) %>%
  mutate(jobStatus2014 = ifelse(jobStatus2014 >= 1 & jobStatus2014 <= 8,
                                jobStatus2014, NA)) %>%
  mutate(jobStatus4Cats2014 = factor(car::recode(jobStatus2014,
                                                 "1 = 1;
                                                 2 = 2; 3 = 2; 7 = 2;
                                                 5 = 3;
                                                 4 = 4; 6 = 4"),
                                     levels = c(1:4), labels = employment_status)) %>%
  mutate(jobStatus2016 = ifelse(jobStatus2016 >= 1 & jobStatus2016 <= 8,
                                jobStatus2016, NA)) %>%
  mutate(jobStatus4Cats2016 = factor(car::recode(jobStatus2016,
                                                 "1 = 1;
                                                 2 = 2; 3 = 2; 7 = 2;
                                                 5 = 3;
                                                 4 = 4; 6 = 4"),
                                     levels = c(1:4), labels = employment_status)) %>%
  mutate(jobStatus2018 = ifelse(jobStatus2018 >= 1 & jobStatus2018 <= 8,
                                jobStatus2018, NA)) %>%
  mutate(jobStatus4Cats2018 = factor(car::recode(jobStatus2018,
                                                 "1 = 1;
                                                 2 = 2; 3 = 2; 7 = 2;
                                                 5 = 3;
                                                 4 = 4; 6 = 4"),
                                     levels = c(1:4), labels = employment_status))


#-----------------------------------------
# Financial security - food security

# recode "in the last two years, have you always had enough money to buy food?"
# to 1 = yes, 0 = no
df <- df %>%
  mutate(foodSecurity1998 = ifelse(foodSecurity1998 >= 1 &
                                       foodSecurity1998 <= 5,
                                     foodSecurity1998, NA)) %>%
  mutate(foodSecurity1998 = factor(car::recode(foodSecurity1998,
                                                 "1 = 1;
                                                 5 = 0"),
                                     levels = c(0,1), labels = c("no", "yes"))) %>%
  mutate(foodSecurity2000 = ifelse(foodSecurity2000 >= 1 &
                                       foodSecurity2000 <= 5,
                                     foodSecurity2000, NA)) %>%
  mutate(foodSecurity2000 = factor(car::recode(foodSecurity2000,
                                                 "1 = 1;
                                                 5 = 0"),
                                     levels = c(0,1), labels = c("no", "yes"))) %>%
  mutate(foodSecurity2002 = ifelse(foodSecurity2002 >= 1 &
                                       foodSecurity2002 <= 5,
                                     foodSecurity2002, NA)) %>%
  mutate(foodSecurity2002 = factor(car::recode(foodSecurity2002,
                                                 "1 = 1;
                                                 5 = 0"),
                                     levels = c(0,1), labels = c("no", "yes"))) %>%
  mutate(foodSecurity2004 = ifelse(foodSecurity2004 >= 1 &
                                       foodSecurity2004 <= 5,
                                     foodSecurity2004, NA)) %>%
  mutate(foodSecurity2004 = factor(car::recode(foodSecurity2004,
                                                 "1 = 1;
                                                 5 = 0"),
                                     levels = c(0,1), labels = c("no", "yes"))) %>%
  mutate(foodSecurity2006 = ifelse(foodSecurity2006 >= 1 &
                                       foodSecurity2006 <= 5,
                                     foodSecurity2006, NA)) %>%
  mutate(foodSecurity2006 = factor(car::recode(foodSecurity2006,
                                                 "1 = 1;
                                                 5 = 0"),
                                     levels = c(0,1), labels = c("no", "yes"))) %>%
  mutate(foodSecurity2008 = ifelse(foodSecurity2008 >= 1 &
                                       foodSecurity2008 <= 5,
                                     foodSecurity2008, NA)) %>%
  mutate(foodSecurity2008 = factor(car::recode(foodSecurity2008,
                                                 "1 = 1;
                                                 5 = 0"),
                                     levels = c(0,1), labels = c("no", "yes"))) %>%
  mutate(foodSecurity2010 = ifelse(foodSecurity2010 >= 1 &
                                       foodSecurity2010 <= 5,
                                     foodSecurity2010, NA)) %>%
  mutate(foodSecurity2010 = factor(car::recode(foodSecurity2010,
                                                 "1 = 1;
                                                 5 = 0"),
                                     levels = c(0,1), labels = c("no", "yes"))) %>%
  mutate(foodSecurity2012 = ifelse(foodSecurity2012 >= 1 &
                                       foodSecurity2012 <= 5,
                                     foodSecurity2012, NA)) %>%
  mutate(foodSecurity2012 = factor(car::recode(foodSecurity2012,
                                                 "1 = 1;
                                                 5 = 0"),
                                     levels = c(0,1), labels = c("no", "yes"))) %>%
  mutate(foodSecurity2014 = ifelse(foodSecurity2014 >= 1 &
                                       foodSecurity2014 <= 5,
                                     foodSecurity2014, NA)) %>%
  mutate(foodSecurity2014 = factor(car::recode(foodSecurity2014,
                                                 "1 = 1;
                                                 5 = 0"),
                                     levels = c(0,1), labels = c("no", "yes"))) %>%
  mutate(foodSecurity2016 = ifelse(foodSecurity2016 >= 1 &
                                       foodSecurity2016 <= 5,
                                     foodSecurity2016, NA)) %>%
  mutate(foodSecurity2016 = factor(car::recode(foodSecurity2016,
                                                 "1 = 1;
                                                 5 = 0"),
                                     levels = c(0,1), labels = c("no", "yes"))) %>%
  mutate(foodSecurity2018 = ifelse(foodSecurity2018 >= 1 &
                                       foodSecurity2018 <= 5,
                                     foodSecurity2018, NA)) %>%
  mutate(foodSecurity2018 = factor(car::recode(foodSecurity2018,
                                                 "1 = 1;
                                                 5 = 0"),
                                     levels = c(0,1), labels = c("no", "yes")))


#----------------------------
# Veteran status

# coded 0 = no, 1 = yes

# convert to factor variable
df <- df %>%
  mutate(veteranStatus = factor(veteranStatus,
                                levels = c(0,1), labels = c("no", "yes")))

#------------------------------------------------------------------------------
# Pain variables

#-------------------------
# Often troubled by pain

# recode anything other than 1 (Yes) and 5 (No) to NA and convert to factor
df <- df %>%
  mutate(troubledWithPainP1998 = factor(car::recode(troubledWithPainP1998,
                                                    "1 = 1;
                                                    5 = 5;
                                                    8 = NA"),
                                        levels = c(1,5),
                                        labels = c("yes", "no"))) %>%
  mutate(troubledWithPainP2000 = factor(car::recode(troubledWithPainP2000,
                                                    "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                        levels = c(1,5),
                                        labels = c("yes", "no"))) %>%
  mutate(troubledWithPainP2002 = factor(car::recode(troubledWithPainP2002,
                                                    "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                        levels = c(1,5),
                                        labels = c("yes", "no"))) %>%
  mutate(troubledWithPainP2004 = factor(car::recode(troubledWithPainP2004,
                                                    "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                        levels = c(1,5),
                                        labels = c("yes", "no"))) %>%
  mutate(troubledWithPainP2006 = factor(car::recode(troubledWithPainP2006,
                                                    "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                        levels = c(1,5),
                                        labels = c("yes", "no"))) %>%
  mutate(troubledWithPainP2008 = factor(car::recode(troubledWithPainP2008,
                                                    "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                        levels = c(1,5),
                                        labels = c("yes", "no"))) %>%
  mutate(troubledWithPainP2010 = factor(car::recode(troubledWithPainP2010,
                                                    "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                        levels = c(1,5),
                                        labels = c("yes", "no"))) %>%
  mutate(troubledWithPainP2012 = factor(car::recode(troubledWithPainP2012,
                                                    "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                        levels = c(1,5),
                                        labels = c("yes", "no"))) %>%
  mutate(troubledWithPainP2014 = factor(car::recode(troubledWithPainP2014,
                                                    "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                        levels = c(1,5),
                                        labels = c("yes", "no"))) %>%
  mutate(troubledWithPainP2016 = factor(car::recode(troubledWithPainP2016,
                                                    "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                        levels = c(1,5),
                                        labels = c("yes", "no"))) %>%
  mutate(troubledWithPainP2018 = factor(car::recode(troubledWithPainP2018,
                                                    "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    -8 = NA"),
                                        levels = c(1,5),
                                        labels = c("yes", "no")))

#-------------------------
# Usual pain severity

# recode to 0 for those who said they were not troubled by pain
df$painUsualSeverity1998[which(df$troubledWithPainP1998 == "no")] <- 0
df$painUsualSeverity2000[which(df$troubledWithPainP2000 == "no")] <- 0
df$painUsualSeverity2002[which(df$troubledWithPainP2002 == "no")] <- 0
df$painUsualSeverity2004[which(df$troubledWithPainP2004 == "no")] <- 0
df$painUsualSeverity2006[which(df$troubledWithPainP2006 == "no")] <- 0
df$painUsualSeverity2008[which(df$troubledWithPainP2008 == "no")] <- 0
df$painUsualSeverity2010[which(df$troubledWithPainP2010 == "no")] <- 0
df$painUsualSeverity2012[which(df$troubledWithPainP2012 == "no")] <- 0
df$painUsualSeverity2014[which(df$troubledWithPainP2014 == "no")] <- 0
df$painUsualSeverity2016[which(df$troubledWithPainP2016 == "no")] <- 0
df$painUsualSeverity2018[which(df$troubledWithPainP2018 == "no")] <- 0

# recode any answer other than 0 (no pain), 1 (mild) 2 (moderate), or
# 3 (severe) to NA and convert to factor
df <- df %>%
  mutate(painUsualSeverity1998 = factor(
    ifelse(painUsualSeverity1998 >= 0 &
             painUsualSeverity1998 <= 3,
           painUsualSeverity1998,
           NA), levels = c(0,1,2,3),
    labels = c("No Pain", "Mild", "Moderate", "Severe"))) %>%
  mutate(painUsualSeverity2000 = factor(
    ifelse(painUsualSeverity2000 >= 0 &
             painUsualSeverity2000 <= 3,
           painUsualSeverity2000,
           NA), levels = c(0,1,2,3),
    labels = c("No Pain", "Mild", "Moderate", "Severe"))) %>%
  mutate(painUsualSeverity2002 = factor(
    ifelse(painUsualSeverity2002 >= 0 &
             painUsualSeverity2002 <= 3,
           painUsualSeverity2002,
           NA), levels = c(0,1,2,3),
    labels = c("No Pain", "Mild", "Moderate", "Severe"))) %>%
  mutate(painUsualSeverity2004 = factor(
    ifelse(painUsualSeverity2004 >= 0 &
             painUsualSeverity2004 <= 3,
           painUsualSeverity2004,
           NA), levels = c(0,1,2,3),
    labels = c("No Pain", "Mild", "Moderate", "Severe"))) %>%
  mutate(painUsualSeverity2006 = factor(
    ifelse(painUsualSeverity2006 >= 0 &
             painUsualSeverity2006 <= 3,
           painUsualSeverity2006,
           NA), levels = c(0,1,2,3),
    labels = c("No Pain", "Mild", "Moderate", "Severe"))) %>%
  mutate(painUsualSeverity2008 = factor(
    ifelse(painUsualSeverity2008 >= 0 &
             painUsualSeverity2008 <= 3,
           painUsualSeverity2008,
           NA), levels = c(0,1,2,3),
    labels = c("No Pain", "Mild", "Moderate", "Severe"))) %>%
  mutate(painUsualSeverity2010 = factor(
    ifelse(painUsualSeverity2010 >= 0 &
             painUsualSeverity2010 <= 3,
           painUsualSeverity2010,
           NA), levels = c(0,1,2,3),
    labels = c("No Pain", "Mild", "Moderate", "Severe"))) %>%
  mutate(painUsualSeverity2012 = factor(
    ifelse(painUsualSeverity2012 >= 0 &
             painUsualSeverity2012 <= 3,
           painUsualSeverity2012,
           NA), levels = c(0,1,2,3),
    labels = c("No Pain", "Mild", "Moderate", "Severe"))) %>%
  mutate(painUsualSeverity2014 = factor(
    ifelse(painUsualSeverity2014 >= 0 &
             painUsualSeverity2014 <= 3,
           painUsualSeverity2014,
           NA), levels = c(0,1,2,3),
    labels = c("No Pain", "Mild", "Moderate", "Severe"))) %>%
  mutate(painUsualSeverity2016 = factor(
    ifelse(painUsualSeverity2016 >= 0 &
             painUsualSeverity2016 <= 3,
           painUsualSeverity2016,
           NA), levels = c(0,1,2,3),
    labels = c("No Pain", "Mild", "Moderate", "Severe"))) %>%
  mutate(painUsualSeverity2018 = factor(
    ifelse(painUsualSeverity2018 >= 0 &
             painUsualSeverity2018 <= 3,
           painUsualSeverity2018,
           NA), levels = c(0,1,2,3),
    labels = c("No Pain", "Mild", "Moderate", "Severe")))

#-------------------------------
# Pain prevents usual activities

# recode anything other than 1 (Yes) and 5 (No) to NA and convert to factor
df <- df %>%
  mutate(painPreventActivityP1998 = factor(car::recode(painPreventActivityP1998,
                                                       "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                           levels = c(1,5),
                                           labels = c("yes", "no"))) %>%
  mutate(painPreventActivityP2000 = factor(car::recode(painPreventActivityP2000,
                                                       "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                           levels = c(1,5),
                                           labels = c("yes", "no"))) %>%
  mutate(painPreventActivityP2002 = factor(car::recode(painPreventActivityP2002,
                                                       "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                           levels = c(1,5),
                                           labels = c("yes", "no"))) %>%
  mutate(painPreventActivityP2004 = factor(car::recode(painPreventActivityP2004,
                                                       "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                           levels = c(1,5),
                                           labels = c("yes", "no"))) %>%
  mutate(painPreventActivityP2006 = factor(car::recode(painPreventActivityP2006,
                                                       "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                           levels = c(1,5),
                                           labels = c("yes", "no"))) %>%
  mutate(painPreventActivityP2008 = factor(car::recode(painPreventActivityP2008,
                                                       "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                           levels = c(1,5),
                                           labels = c("yes", "no"))) %>%
  mutate(painPreventActivityP2010 = factor(car::recode(painPreventActivityP2010,
                                                       "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                           levels = c(1,5),
                                           labels = c("yes", "no"))) %>%
  mutate(painPreventActivityP2012 = factor(car::recode(painPreventActivityP2012,
                                                       "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                           levels = c(1,5),
                                           labels = c("yes", "no"))) %>%
  mutate(painPreventActivityP2014 = factor(car::recode(painPreventActivityP2014,
                                                       "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                           levels = c(1,5),
                                           labels = c("yes", "no"))) %>%
  mutate(painPreventActivityP2016 = factor(car::recode(painPreventActivityP2016,
                                                       "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA"),
                                           levels = c(1,5),
                                           labels = c("yes", "no"))) %>%
  mutate(painPreventActivityP2018 = factor(car::recode(painPreventActivityP2018,
                                                       "1 = 1;
                                                    5 = 5;
                                                    8 = NA;
                                                    9 = NA;
                                                    -8 = NA"),
                                           levels = c(1,5),
                                           labels = c("yes", "no")))

#------------------------------------------------------------------------------
# Other health variables

#---------------------------------
# Cancer

# # description from Hanna Grol-Prokopczyk's Stata .do file on cleaning HRS data:

# "Goal is to create a flag showing if someone was in active cancer
# treatment (or palliative care) at time of current wave, to (potentially)
# exclude people suffering from cancer pain rather than chronic
# non-cancer pain.

# Based on available variables (none of which ask about remission
# explicitly), I create a flag called cancerActiveP, which is 1 if
# cancerTrtmtLast2YrsP == 1, cancerNewSinceLastWaveP == 1, or
# cancerBetterSameWorse == Worse.  Is set to 1 for 1-4% of sample,
# depending on wave (where "sample" means sampleWt1998 > 0 & !missing).
# I tried various permutations of these variables; including the third
# makes a very minor difference, but to be conservative I include it.

# If person denies a previous report of cancer, believe them--
# i.e., treat a denial as a "No".  In 1998, 3 was a denial.  From 2000
# onward, 3 means they dispute prior report but now have cancer, and
# 4 is a denial of cancer."

df <- df %>%
  mutate(cancerEverP1998 = ifelse(cancerEverP1998 >= 1 & cancerEverP1998 <= 5,
                                  cancerEverP1998, NA)) %>%
  mutate(cancerEverP1998 = car::recode(cancerEverP1998,
                                       "5 = 0;
                                       3 = 0")) %>%
  mutate(cancerEverP1998 = factor(cancerEverP1998, levels = c(0,1),
                                  labels = c("no", "yes"))) %>%
  mutate(cancerEverP2000 = ifelse(cancerEverP2000 >= 1 & cancerEverP2000 <= 5,
                                  cancerEverP2000, NA)) %>%
  mutate(cancerEverP2000 = car::recode(cancerEverP2000,
                                       "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(cancerEverP2000 = factor(cancerEverP2000, levels = c(0,1),
                                  labels = c("no", "yes"))) %>%
  mutate(cancerEverP2002 = ifelse(cancerEverP2002 >= 1 & cancerEverP2002 <= 5,
                                  cancerEverP2002, NA)) %>%
  mutate(cancerEverP2002 = car::recode(cancerEverP2002,
                                       "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(cancerEverP2002 = factor(cancerEverP2002, levels = c(0,1),
                                  labels = c("no", "yes"))) %>%
  mutate(cancerEverP2004 = ifelse(cancerEverP2004 >= 1 & cancerEverP2004 <= 5,
                                  cancerEverP2004, NA)) %>%
  mutate(cancerEverP2004 = car::recode(cancerEverP2004,
                                       "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(cancerEverP2004 = factor(cancerEverP2004, levels = c(0,1),
                                  labels = c("no", "yes"))) %>%
  mutate(cancerEverP2006 = ifelse(cancerEverP2006 >= 1 & cancerEverP2006 <= 5,
                                  cancerEverP2006, NA)) %>%
  mutate(cancerEverP2006 = car::recode(cancerEverP2006,
                                       "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(cancerEverP2006 = factor(cancerEverP2006, levels = c(0,1),
                                  labels = c("no", "yes"))) %>%
  mutate(cancerEverP2008 = ifelse(cancerEverP2008 >= 1 & cancerEverP2008 <= 5,
                                  cancerEverP2008, NA)) %>%
  mutate(cancerEverP2008 = car::recode(cancerEverP2008,
                                       "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(cancerEverP2008 = factor(cancerEverP2008, levels = c(0,1),
                                  labels = c("no", "yes"))) %>%
  mutate(cancerEverP2010 = ifelse(cancerEverP2010 >= 1 & cancerEverP2010 <= 5,
                                  cancerEverP2010, NA)) %>%
  mutate(cancerEverP2010 = car::recode(cancerEverP2010,
                                       "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(cancerEverP2010 = factor(cancerEverP2010, levels = c(0,1),
                                  labels = c("no", "yes"))) %>%
  mutate(cancerEverP2012 = ifelse(cancerEverP2012 >= 1 & cancerEverP2012 <= 5,
                                  cancerEverP2012, NA)) %>%
  mutate(cancerEverP2012 = car::recode(cancerEverP2012,
                                       "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(cancerEverP2012 = factor(cancerEverP2012, levels = c(0,1),
                                  labels = c("no", "yes"))) %>%
  mutate(cancerEverP2014 = ifelse(cancerEverP2014 >= 1 & cancerEverP2014 <= 5,
                                  cancerEverP2014, NA)) %>%
  mutate(cancerEverP2014 = car::recode(cancerEverP2014,
                                       "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(cancerEverP2014 = factor(cancerEverP2014, levels = c(0,1),
                                  labels = c("no", "yes"))) %>%
  mutate(cancerEverP2016 = ifelse(cancerEverP2016 >= 1 & cancerEverP2016 <= 5,
                                  cancerEverP2016, NA)) %>%
  mutate(cancerEverP2016 = car::recode(cancerEverP2016,
                                       "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(cancerEverP2016 = factor(cancerEverP2016, levels = c(0,1),
                                  labels = c("no", "yes"))) %>%
  mutate(cancerEverP2018 = ifelse(cancerEverP2018 >= 1 & cancerEverP2018 <= 5,
                                  cancerEverP2018, NA)) %>%
  mutate(cancerEverP2018 = car::recode(cancerEverP2018,
                                       "5 = 0;
                                       4 = 0")) %>%
  mutate(cancerEverP2018 = factor(cancerEverP2018, levels = c(0,1),
                                  labels = c("no", "yes")))

# Note:  in 1998, Q is whether you're *now* receiving cancer treatment;
# in subsequent waves, it's whether you've received cancer treatment in
# last 2 years.  I will name the var to be consistent with the latter,
# more common question.  1 = yes, 5 = no, in orig vars.


# Various other cancer-related variables, to create and/or validate
# cancerActiveP flag.

# Cancer same/better/worse

# labels for cancer status categories
cancer_labs <- c("Better", "Same", "Worse")

# change any values other than 1/2/3 to NA and
# convert to factor
df <- df %>%
  mutate(cancerBetterSameWorse1998 = factor(
    ifelse(cancerBetterSameWorse1998 >= 1 & cancerBetterSameWorse1998 <= 3,
           cancerBetterSameWorse1998, NA), levels = c(1,2,3),
    labels = cancer_labs)) %>%
  mutate(cancerBetterSameWorse2000 = factor(
    ifelse(cancerBetterSameWorse2000 >= 1 & cancerBetterSameWorse2000 <= 3,
           cancerBetterSameWorse2000, NA), levels = c(1,2,3),
    labels = cancer_labs)) %>%
  mutate(cancerBetterSameWorse2002 = factor(
    ifelse(cancerBetterSameWorse2002 >= 1 & cancerBetterSameWorse2002 <= 3,
           cancerBetterSameWorse2002, NA), levels = c(1,2,3),
    labels = cancer_labs)) %>%
  mutate(cancerBetterSameWorse2004 = factor(
    ifelse(cancerBetterSameWorse2004 >= 1 & cancerBetterSameWorse2004 <= 3,
           cancerBetterSameWorse2004, NA), levels = c(1,2,3),
    labels = cancer_labs)) %>%
  mutate(cancerBetterSameWorse2006 = factor(
    ifelse(cancerBetterSameWorse2006 >= 1 & cancerBetterSameWorse2006 <= 3,
           cancerBetterSameWorse2006, NA), levels = c(1,2,3),
    labels = cancer_labs)) %>%
  mutate(cancerBetterSameWorse2008 = factor(
    ifelse(cancerBetterSameWorse2008 >= 1 & cancerBetterSameWorse2008 <= 3,
           cancerBetterSameWorse2008, NA), levels = c(1,2,3),
    labels = cancer_labs)) %>%
  mutate(cancerBetterSameWorse2010 = factor(
    ifelse(cancerBetterSameWorse2010 >= 1 & cancerBetterSameWorse2010 <= 3,
           cancerBetterSameWorse2010, NA), levels = c(1,2,3),
    labels = cancer_labs)) %>%
  mutate(cancerBetterSameWorse2012 = factor(
    ifelse(cancerBetterSameWorse2012 >= 1 & cancerBetterSameWorse2012 <= 3,
           cancerBetterSameWorse2012, NA), levels = c(1,2,3),
    labels = cancer_labs)) %>%
  mutate(cancerBetterSameWorse2014 = factor(
    ifelse(cancerBetterSameWorse2014 >= 1 & cancerBetterSameWorse2014 <= 3,
           cancerBetterSameWorse2014, NA), levels = c(1,2,3),
    labels = cancer_labs)) %>%
  mutate(cancerBetterSameWorse2016 = factor(
    ifelse(cancerBetterSameWorse2016 >= 1 & cancerBetterSameWorse2016 <= 3,
           cancerBetterSameWorse2016, NA), levels = c(1,2,3),
    labels = cancer_labs)) %>%
  mutate(cancerBetterSameWorse2018 = factor(
    ifelse(cancerBetterSameWorse2018 >= 1 & cancerBetterSameWorse2018 <= 3,
           cancerBetterSameWorse2018, NA), levels = c(1,2,3),
    labels = cancer_labs))

# Cancer treatment last two years (NOTE: no responses in 2004, 2008, 2012, 2016)
# recode anything other than 1 (Yes) and 5 (No) to NA and convert to factor
df <- df %>%
  mutate(cancerTrtmtLast2YrsP1998 = factor(car::recode(cancerTrtmtLast2YrsP1998,
                                                       "1 = 1;
                                                    5 = 0"),
                                           levels = c(0,1),
                                           labels = c("no","yes"))) %>%
  mutate(cancerTrtmtLast2YrsP2000 = factor(car::recode(cancerTrtmtLast2YrsP2000,
                                                       "1 = 1;
                                                    5 = 0;
                                                       8 = NA"),
                                           levels = c(0,1),
                                           labels = c("no","yes"))) %>%
  mutate(cancerTrtmtLast2YrsP2002 = factor(car::recode(cancerTrtmtLast2YrsP2002,
                                                       "1 = 1;
                                                    5 = 0;
                                                       8 = NA"),
                                           levels = c(0,1),
                                           labels = c("no","yes"))) %>%
  mutate(cancerTrtmtLast2YrsP2004 = factor(car::recode(cancerTrtmtLast2YrsP2004,
                                                       "1 = 1;
                                                    5 = 0"),
                                           levels = c(0,1),
                                           labels = c("no","yes"))) %>%
  mutate(cancerTrtmtLast2YrsP2006 = factor(car::recode(cancerTrtmtLast2YrsP2006,
                                                       "1 = 1;
                                                    5 = 0;
                                                       8 = NA"),
                                           levels = c(0,1),
                                           labels = c("no","yes"))) %>%
  mutate(cancerTrtmtLast2YrsP2008 = factor(car::recode(cancerTrtmtLast2YrsP2008,
                                                       "1 = 1;
                                                    5 = 0"),
                                           levels = c(0,1),
                                           labels = c("no","yes"))) %>%
  mutate(cancerTrtmtLast2YrsP2010 = factor(car::recode(cancerTrtmtLast2YrsP2010,
                                                       "1 = 1;
                                                    5 = 0;
                                                       8 = NA"),
                                           levels = c(0,1),
                                           labels = c("no","yes"))) %>%
  mutate(cancerTrtmtLast2YrsP2012 = factor(car::recode(cancerTrtmtLast2YrsP2012,
                                                       "1 = 1;
                                                    5 = 0;
                                                       8 = NA"),
                                           levels = c(0,1),
                                           labels = c("no","yes"))) %>%
  mutate(cancerTrtmtLast2YrsP2014 = factor(car::recode(cancerTrtmtLast2YrsP2014,
                                                       "1 = 1;
                                                    5 = 0;
                                                       8 = NA"),
                                           levels = c(0,1),
                                           labels = c("no","yes"))) %>%
  mutate(cancerTrtmtLast2YrsP2016 = factor(car::recode(cancerTrtmtLast2YrsP2016,
                                                       "1 = 1;
                                                    5 = 0;
                                                       8 = 0"),
                                           levels = c(0,1),
                                           labels = c("no","yes"))) %>%
  mutate(cancerTrtmtLast2YrsP2018 = factor(car::recode(cancerTrtmtLast2YrsP2018,
                                                       "1 = 1;
                                                    5 = 0;
                                                       8 = NA;
                                                       -8 = NA"),
                                           levels = c(0,1),
                                           labels = c("no","yes")))

# Cancer new since last wave
df <- df %>%
  mutate(cancerNewSinceLastWaveP1998 = factor(
    ifelse(cancerNewSinceLastWaveP1998 == 1 | cancerNewSinceLastWaveP1998 == 5,
           cancerNewSinceLastWaveP1998, NA),
    levels = c(1,5),
    labels = c("yes","no"))) %>%
  mutate(cancerNewSinceLastWaveP2000 = factor(
    ifelse(cancerNewSinceLastWaveP2000 == 1 | cancerNewSinceLastWaveP2000 == 5,
           cancerNewSinceLastWaveP2000, NA),
    levels = c(1,5),
    labels = c("yes","no"))) %>%
  mutate(cancerNewSinceLastWaveP2002 = factor(
    ifelse(cancerNewSinceLastWaveP2002 == 1 | cancerNewSinceLastWaveP2002 == 5,
           cancerNewSinceLastWaveP2002, NA),
    levels = c(1,5),
    labels = c("yes","no"))) %>%
  mutate(cancerNewSinceLastWaveP2004 = factor(
    ifelse(cancerNewSinceLastWaveP2004 == 1 | cancerNewSinceLastWaveP2004 == 5,
           cancerNewSinceLastWaveP2004, NA),
    levels = c(1,5),
    labels = c("yes","no"))) %>%
  mutate(cancerNewSinceLastWaveP2006 = factor(
    ifelse(cancerNewSinceLastWaveP2006 == 1 | cancerNewSinceLastWaveP2006 == 5,
           cancerNewSinceLastWaveP2006, NA),
    levels = c(1,5),
    labels = c("yes","no"))) %>%
  mutate(cancerNewSinceLastWaveP2008 = factor(
    ifelse(cancerNewSinceLastWaveP2008 == 1 | cancerNewSinceLastWaveP2008 == 5,
           cancerNewSinceLastWaveP2008, NA),
    levels = c(1,5),
    labels = c("yes","no"))) %>%
  mutate(cancerNewSinceLastWaveP2010 = factor(
    ifelse(cancerNewSinceLastWaveP2010 == 1 | cancerNewSinceLastWaveP2010 == 5,
           cancerNewSinceLastWaveP2010, NA),
    levels = c(1,5),
    labels = c("yes","no"))) %>%
  mutate(cancerNewSinceLastWaveP2012 = factor(
    ifelse(cancerNewSinceLastWaveP2012 == 1 | cancerNewSinceLastWaveP2012 == 5,
           cancerNewSinceLastWaveP2012, NA),
    levels = c(1,5),
    labels = c("yes","no"))) %>%
  mutate(cancerNewSinceLastWaveP2014 = factor(
    ifelse(cancerNewSinceLastWaveP2014 == 1 | cancerNewSinceLastWaveP2014 == 5,
           cancerNewSinceLastWaveP2014, NA),
    levels = c(1,5),
    labels = c("yes","no"))) %>%
  mutate(cancerNewSinceLastWaveP2016 = factor(
    ifelse(cancerNewSinceLastWaveP2016 == 1 | cancerNewSinceLastWaveP2016 == 5,
           cancerNewSinceLastWaveP2016, NA),
    levels = c(1,5),
    labels = c("yes","no"))) %>%
  mutate(cancerNewSinceLastWaveP2018 = factor(
    ifelse(cancerNewSinceLastWaveP2018 == 1 | cancerNewSinceLastWaveP2018 == 5,
           cancerNewSinceLastWaveP2018, NA),
    levels = c(1,5),
    labels = c("yes","no")))

# create active cancer variable for each wave - 0 by default, change to 1 if
# evidence of active cancer based on responses to cancerTrmtLast2Yrs,
# canceBetterSameWorse, and cancerNewSinceLastWave
df$cancerActiveP1998 <- 0
df$cancerActiveP2000 <- 0
df$cancerActiveP2002 <- 0
df$cancerActiveP2004 <- 0
df$cancerActiveP2006 <- 0
df$cancerActiveP2008 <- 0
df$cancerActiveP2010 <- 0
df$cancerActiveP2012 <- 0
df$cancerActiveP2014 <- 0
df$cancerActiveP2016 <- 0
df$cancerActiveP2018 <- 0

df$cancerActiveP1998[which(df$cancerTrtmtLast2YrsP1998 == "yes")] <- 1
df$cancerActiveP1998[which(df$cancerBetterSameWorse1998 == "Worse")] <- 1
df$cancerActiveP1998[which(df$cancerNewSinceLastWaveP1998 == "yes")] <- 1
df$cancerActiveP1998[which(is.na(df$cancerEverP1998) == TRUE &
                             is.na(df$cancerTrtmtLast2YrsP1998) == TRUE &
                             is.na(df$cancerBetterSameWorse1998) == TRUE &
                             is.na(df$cancerNewSinceLastWaveP1998) == TRUE)] <- NA

df$cancerActiveP2000[which(df$cancerTrtmtLast2YrsP2000 == "yes")] <- 1
df$cancerActiveP2000[which(df$cancerBetterSameWorse2000 == "Worse")] <- 1
df$cancerActiveP2000[which(df$cancerNewSinceLastWaveP2000 == "yes")] <- 1
df$cancerActiveP2000[which(is.na(df$cancerEverP2000) == TRUE &
                             is.na(df$cancerTrtmtLast2YrsP2000) == TRUE &
                             is.na(df$cancerBetterSameWorse2000) == TRUE &
                             is.na(df$cancerNewSinceLastWaveP2000) == TRUE)] <- NA

df$cancerActiveP2002[which(df$cancerTrtmtLast2YrsP2002 == "yes")] <- 1
df$cancerActiveP2002[which(df$cancerBetterSameWorse2002 == "Worse")] <- 1
df$cancerActiveP2002[which(df$cancerNewSinceLastWaveP2002 == "yes")] <- 1
df$cancerActiveP2002[which(is.na(df$cancerEverP2002) == TRUE &
                             is.na(df$cancerTrtmtLast2YrsP2002) == TRUE &
                             is.na(df$cancerBetterSameWorse2002) == TRUE &
                             is.na(df$cancerNewSinceLastWaveP2002) == TRUE)] <- NA

df$cancerActiveP2004[which(df$cancerTrtmtLast2YrsP2004 == "yes")] <- 1
df$cancerActiveP2004[which(df$cancerBetterSameWorse2004 == "Worse")] <- 1
df$cancerActiveP2004[which(df$cancerNewSinceLastWaveP2004 == "yes")] <- 1
df$cancerActiveP2004[which(is.na(df$cancerEverP2004) == TRUE &
                             is.na(df$cancerTrtmtLast2YrsP2004) == TRUE &
                             is.na(df$cancerBetterSameWorse2004) == TRUE &
                             is.na(df$cancerNewSinceLastWaveP2004) == TRUE)] <- NA

df$cancerActiveP2006[which(df$cancerTrtmtLast2YrsP2006 == "yes")] <- 1
df$cancerActiveP2006[which(df$cancerBetterSameWorse2006 == "Worse")] <- 1
df$cancerActiveP2006[which(df$cancerNewSinceLastWaveP2006 == "yes")] <- 1
df$cancerActiveP2006[which(is.na(df$cancerEverP2006) == TRUE &
                             is.na(df$cancerTrtmtLast2YrsP2006) == TRUE &
                             is.na(df$cancerBetterSameWorse2006) == TRUE &
                             is.na(df$cancerNewSinceLastWaveP2006) == TRUE)] <- NA

df$cancerActiveP2008[which(df$cancerTrtmtLast2YrsP2008 == "yes")] <- 1
df$cancerActiveP2008[which(df$cancerBetterSameWorse2008 == "Worse")] <- 1
df$cancerActiveP2008[which(df$cancerNewSinceLastWaveP2008 == "yes")] <- 1
df$cancerActiveP2008[which(is.na(df$cancerEverP2008) == TRUE &
                             is.na(df$cancerTrtmtLast2YrsP2008) == TRUE &
                             is.na(df$cancerBetterSameWorse2008) == TRUE &
                             is.na(df$cancerNewSinceLastWaveP2008) == TRUE)] <- NA

df$cancerActiveP2010[which(df$cancerTrtmtLast2YrsP2010 == "yes")] <- 1
df$cancerActiveP2010[which(df$cancerBetterSameWorse2010 == "Worse")] <- 1
df$cancerActiveP2010[which(df$cancerNewSinceLastWaveP2010 == "yes")] <- 1
df$cancerActiveP2010[which(is.na(df$cancerEverP2010) == TRUE &
                             is.na(df$cancerTrtmtLast2YrsP2010) == TRUE &
                             is.na(df$cancerBetterSameWorse2010) == TRUE &
                             is.na(df$cancerNewSinceLastWaveP2010) == TRUE)] <- NA

df$cancerActiveP2012[which(df$cancerTrtmtLast2YrsP2012 == "yes")] <- 1
df$cancerActiveP2012[which(df$cancerBetterSameWorse2012 == "Worse")] <- 1
df$cancerActiveP2012[which(df$cancerNewSinceLastWaveP2012 == "yes")] <- 1
df$cancerActiveP2012[which(is.na(df$cancerEverP2012) == TRUE &
                             is.na(df$cancerTrtmtLast2YrsP2012) == TRUE &
                             is.na(df$cancerBetterSameWorse2012) == TRUE &
                             is.na(df$cancerNewSinceLastWaveP2012) == TRUE)] <- NA

df$cancerActiveP2014[which(df$cancerTrtmtLast2YrsP2014 == "yes")] <- 1
df$cancerActiveP2014[which(df$cancerBetterSameWorse2014 == "Worse")] <- 1
df$cancerActiveP2014[which(df$cancerNewSinceLastWaveP2014 == "yes")] <- 1
df$cancerActiveP2014[which(is.na(df$cancerEverP2014) == TRUE &
                             is.na(df$cancerTrtmtLast2YrsP2014) == TRUE &
                             is.na(df$cancerBetterSameWorse2014) == TRUE &
                             is.na(df$cancerNewSinceLastWaveP2014) == TRUE)] <- NA

df$cancerActiveP2016[which(df$cancerTrtmtLast2YrsP2016 == "yes")] <- 1
df$cancerActiveP2016[which(df$cancerBetterSameWorse2016 == "Worse")] <- 1
df$cancerActiveP2016[which(df$cancerNewSinceLastWaveP2016 == "yes")] <- 1
df$cancerActiveP2016[which(is.na(df$cancerEverP2016) == TRUE &
                             is.na(df$cancerTrtmtLast2YrsP2016) == TRUE &
                             is.na(df$cancerBetterSameWorse2016) == TRUE &
                             is.na(df$cancerNewSinceLastWaveP2016) == TRUE)] <- NA

df$cancerActiveP2018[which(df$cancerTrtmtLast2YrsP2018 == "yes")] <- 1
df$cancerActiveP2018[which(df$cancerBetterSameWorse2018 == "Worse")] <- 1
df$cancerActiveP2018[which(df$cancerNewSinceLastWaveP2018 == "yes")] <- 1
df$cancerActiveP2018[which(is.na(df$cancerEverP2018) == TRUE &
                             is.na(df$cancerTrtmtLast2YrsP2018) == TRUE &
                             is.na(df$cancerBetterSameWorse2018) == TRUE &
                             is.na(df$cancerNewSinceLastWaveP2018) == TRUE)] <- NA

# convert all to factor
df <- df %>%
  mutate(cancerActiveP1998 = factor(cancerActiveP1998,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(cancerActiveP2000 = factor(cancerActiveP2000,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(cancerActiveP2002 = factor(cancerActiveP2002,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(cancerActiveP2004 = factor(cancerActiveP2004,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(cancerActiveP2006 = factor(cancerActiveP2006,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(cancerActiveP2008 = factor(cancerActiveP2008,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(cancerActiveP2010 = factor(cancerActiveP2010,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(cancerActiveP2012 = factor(cancerActiveP2012,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(cancerActiveP2014 = factor(cancerActiveP2014,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(cancerActiveP2016 = factor(cancerActiveP2016,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(cancerActiveP2018 = factor(cancerActiveP2018,
                                    levels = c(0,1), labels = c("no","yes")))

#------------------------------------------------------------------------------
# Health condition variables

#-----------------------------------
# Diabetes

# If a person denies a previous report of diabetes, code that they do not have
# diabetes

# In 2018, 4 means they never had diabetes, and 6 means they had diabetes before
# but don't anymore

df <- df %>%
  mutate(diabetesP1998 = ifelse(diabetesP1998 >= 1 & diabetesP1998 <= 5,
                                diabetesP1998, NA)) %>%
  mutate(diabetesP1998 = car::recode(diabetesP1998,
                                     "5 = 0;
                                       3 = 0")) %>%
  mutate(diabetesP1998 = factor(diabetesP1998, levels = c(0,1),
                                labels = c("no", "yes"))) %>%
  mutate(diabetesP2000 = ifelse(diabetesP2000 >= 1 & diabetesP2000 <= 5,
                                diabetesP2000, NA)) %>%
  mutate(diabetesP2000 = car::recode(diabetesP2000,
                                     "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(diabetesP2000 = factor(diabetesP2000, levels = c(0,1),
                                labels = c("no", "yes"))) %>%
  mutate(diabetesP2002 = ifelse(diabetesP2002 >= 1 & diabetesP2002 <= 5,
                                diabetesP2002, NA)) %>%
  mutate(diabetesP2002 = car::recode(diabetesP2002,
                                     "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(diabetesP2002 = factor(diabetesP2002, levels = c(0,1),
                                labels = c("no", "yes"))) %>%
  mutate(diabetesP2004 = ifelse(diabetesP2004 >= 1 & diabetesP2004 <= 5,
                                diabetesP2004, NA)) %>%
  mutate(diabetesP2004 = car::recode(diabetesP2004,
                                     "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(diabetesP2004 = factor(diabetesP2004, levels = c(0,1),
                                labels = c("no", "yes"))) %>%
  mutate(diabetesP2006 = ifelse(diabetesP2006 >= 1 & diabetesP2006 <= 5,
                                diabetesP2006, NA)) %>%
  mutate(diabetesP2006 = car::recode(diabetesP2006,
                                     "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(diabetesP2006 = factor(diabetesP2006, levels = c(0,1),
                                labels = c("no", "yes"))) %>%
  mutate(diabetesP2008 = ifelse(diabetesP2008 >= 1 & diabetesP2008 <= 5,
                                diabetesP2008, NA)) %>%
  mutate(diabetesP2008 = car::recode(diabetesP2008,
                                     "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(diabetesP2008 = factor(diabetesP2008, levels = c(0,1),
                                labels = c("no", "yes"))) %>%
  mutate(diabetesP2010 = ifelse(diabetesP2010 >= 1 & diabetesP2010 <= 5,
                                diabetesP2010, NA)) %>%
  mutate(diabetesP2010 = car::recode(diabetesP2010,
                                     "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(diabetesP2010 = factor(diabetesP2010, levels = c(0,1),
                                labels = c("no", "yes"))) %>%
  mutate(diabetesP2012 = ifelse(diabetesP2012 >= 1 & diabetesP2012 <= 5,
                                diabetesP2012, NA)) %>%
  mutate(diabetesP2012 = car::recode(diabetesP2012,
                                     "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(diabetesP2012 = factor(diabetesP2012, levels = c(0,1),
                                labels = c("no", "yes"))) %>%
  mutate(diabetesP2014 = ifelse(diabetesP2014 >= 1 & diabetesP2014 <= 5,
                                diabetesP2014, NA)) %>%
  mutate(diabetesP2014 = car::recode(diabetesP2014,
                                     "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(diabetesP2014 = factor(diabetesP2014, levels = c(0,1),
                                labels = c("no", "yes"))) %>%
  mutate(diabetesP2016 = ifelse(diabetesP2016 >= 1 & diabetesP2016 <= 5,
                                diabetesP2016, NA)) %>%
  mutate(diabetesP2016 = car::recode(diabetesP2016,
                                     "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(diabetesP2016 = factor(diabetesP2016, levels = c(0,1),
                                labels = c("no", "yes"))) %>%
  mutate(diabetesP2018 = ifelse(diabetesP2018 >= 1 & diabetesP2018 <= 6,
                                diabetesP2018, NA)) %>%
  mutate(diabetesP2018 = car::recode(diabetesP2018,
                                     "4 = 0;
                                       5 = 0;
                                       6 = 0")) %>%
  mutate(diabetesP2018 = factor(diabetesP2018, levels = c(0,1),
                                labels = c("no", "yes")))

# Since diabetes is so rarely cured, propagate it forward
df$diabetesP2000[which(is.na(df$diabetesP2000) == TRUE &
                         df$diabetesP1998 == 1 & df$deadP2000 == 0)] <- "yes"
df$diabetesP2002[which(is.na(df$diabetesP2002) == TRUE &
                         df$diabetesP2000 == 1 & df$deadP2002 == 0)] <- "yes"
df$diabetesP2004[which(is.na(df$diabetesP2004) == TRUE &
                         df$diabetesP2002 == 1 & df$deadP2004 == 0)] <- "yes"
df$diabetesP2006[which(is.na(df$diabetesP2006) == TRUE &
                         df$diabetesP2004 == 1 & df$deadP2006 == 0)] <- "yes"
df$diabetesP2008[which(is.na(df$diabetesP2008) == TRUE &
                         df$diabetesP2006 == 1 & df$deadP2008 == 0)] <- "yes"
df$diabetesP2010[which(is.na(df$diabetesP2010) == TRUE &
                         df$diabetesP2008 == 1 & df$deadP2010 == 0)] <- "yes"
df$diabetesP2012[which(is.na(df$diabetesP2012) == TRUE &
                         df$diabetesP2010 == 1 & df$deadP2012 == 0)] <- "yes"
df$diabetesP2014[which(is.na(df$diabetesP2014) == TRUE &
                         df$diabetesP2012 == 1 & df$deadP2014 == 0)] <- "yes"
df$diabetesP2016[which(is.na(df$diabetesP2016) == TRUE &
                         df$diabetesP2014 == 1 & df$deadP2016 == 0)] <- "yes"
df$diabetesP2018[which(is.na(df$diabetesP2018) == TRUE &
                         df$diabetesP2016 == 1 & df$deadP2018 == 0)] <- "yes"


#-----------------------------------
# Lung disease

# If a person denies a previous report of lung disease, code that they do not
# have lung disease

# In 2018, 4 means they never had lung disease and 6 means they had lung disease
# before but don't anymore

df <- df %>%
  mutate(lungDisP1998 = ifelse(lungDisP1998 >= 1 & lungDisP1998 <= 5,
                               lungDisP1998, NA)) %>%
  mutate(lungDisP1998 = car::recode(lungDisP1998,
                                    "5 = 0;
                                       3 = 0")) %>%
  mutate(lungDisP1998 = factor(lungDisP1998, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(lungDisP2000 = ifelse(lungDisP2000 >= 1 & lungDisP2000 <= 5,
                               lungDisP2000, NA)) %>%
  mutate(lungDisP2000 = car::recode(lungDisP2000,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(lungDisP2000 = factor(lungDisP2000, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(lungDisP2002 = ifelse(lungDisP2002 >= 1 & lungDisP2002 <= 5,
                               lungDisP2002, NA)) %>%
  mutate(lungDisP2002 = car::recode(lungDisP2002,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(lungDisP2002 = factor(lungDisP2002, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(lungDisP2004 = ifelse(lungDisP2004 >= 1 & lungDisP2004 <= 5,
                               lungDisP2004, NA)) %>%
  mutate(lungDisP2004 = car::recode(lungDisP2004,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(lungDisP2004 = factor(lungDisP2004, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(lungDisP2006 = ifelse(lungDisP2006 >= 1 & lungDisP2006 <= 5,
                               lungDisP2006, NA)) %>%
  mutate(lungDisP2006 = car::recode(lungDisP2006,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(lungDisP2006 = factor(lungDisP2006, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(lungDisP2008 = ifelse(lungDisP2008 >= 1 & lungDisP2008 <= 5,
                               lungDisP2008, NA)) %>%
  mutate(lungDisP2008 = car::recode(lungDisP2008,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(lungDisP2008 = factor(lungDisP2008, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(lungDisP2010 = ifelse(lungDisP2010 >= 1 & lungDisP2010 <= 5,
                               lungDisP2010, NA)) %>%
  mutate(lungDisP2010 = car::recode(lungDisP2010,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(lungDisP2010 = factor(lungDisP2010, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(lungDisP2012 = ifelse(lungDisP2012 >= 1 & lungDisP2012 <= 5,
                               lungDisP2012, NA)) %>%
  mutate(lungDisP2012 = car::recode(lungDisP2012,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(lungDisP2012 = factor(lungDisP2012, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(lungDisP2014 = ifelse(lungDisP2014 >= 1 & lungDisP2014 <= 5,
                               lungDisP2014, NA)) %>%
  mutate(lungDisP2014 = car::recode(lungDisP2014,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(lungDisP2014 = factor(lungDisP2014, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(lungDisP2016 = ifelse(lungDisP2016 >= 1 & lungDisP2016 <= 5,
                               lungDisP2016, NA)) %>%
  mutate(lungDisP2016 = car::recode(lungDisP2016,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(lungDisP2016 = factor(lungDisP2016, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(lungDisP2018 = ifelse(lungDisP2018 >= 1 & lungDisP2018 <= 6,
                               lungDisP2018, NA)) %>%
  mutate(lungDisP2018 = car::recode(lungDisP2018,
                                    "5 = 0;
                                    4 = 0;
                                    6 = 0")) %>%
  mutate(lungDisP2018 = factor(lungDisP2018, levels = c(0,1),
                               labels = c("no", "yes")))

# Since COPD cannot be cured (per Medline), propogate it forward, i.e.,
# if R reported lung disease in wave X, set it to 1 in wave X+1 (if missing),
# assuming R is still alive.
df$lungDisP2000[which(is.na(df$lungDisP2000) == TRUE &
                        df$lungDisP1998 == 1 & df$deadP2000 == 0)] <- "yes"
df$lungDisP2002[which(is.na(df$lungDisP2002) == TRUE &
                        df$lungDisP2000 == 1 & df$deadP2002 == 0)] <- "yes"
df$lungDisP2004[which(is.na(df$lungDisP2004) == TRUE &
                        df$lungDisP2002 == 1 & df$deadP2004 == 0)] <- "yes"
df$lungDisP2006[which(is.na(df$lungDisP2006) == TRUE &
                        df$lungDisP2004 == 1 & df$deadP2006 == 0)] <- "yes"
df$lungDisP2008[which(is.na(df$lungDisP2008) == TRUE &
                        df$lungDisP2006 == 1 & df$deadP2008 == 0)] <- "yes"
df$lungDisP2010[which(is.na(df$lungDisP2010) == TRUE &
                        df$lungDisP2008 == 1 & df$deadP2010 == 0)] <- "yes"
df$lungDisP2012[which(is.na(df$lungDisP2012) == TRUE &
                        df$lungDisP2010 == 1 & df$deadP2012 == 0)] <- "yes"
df$lungDisP2014[which(is.na(df$lungDisP2014) == TRUE &
                        df$lungDisP2012 == 1 & df$deadP2014 == 0)] <- "yes"
df$lungDisP2016[which(is.na(df$lungDisP2016) == TRUE &
                        df$lungDisP2014 == 1 & df$deadP2016 == 0)] <- "yes"
df$lungDisP2018[which(is.na(df$lungDisP2018) == TRUE &
                        df$lungDisP2016 == 1 & df$deadP2018 == 0)] <- "yes"

#-----------------------------------
# Heart condition

# If a person denies a previous report of heart condition, code that they do not
# have heart condition

# In 2018, 4 means they never had a heart condition and 6 means they had a heart
# condition before but don't anymore

df <- df %>%
  mutate(hrtCondP1998 = ifelse(hrtCondP1998 >= 1 & hrtCondP1998 <= 5,
                               hrtCondP1998, NA)) %>%
  mutate(hrtCondP1998 = car::recode(hrtCondP1998,
                                    "5 = 0;
                                       3 = 0")) %>%
  mutate(hrtCondP1998 = factor(hrtCondP1998, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(hrtCondP2000 = ifelse(hrtCondP2000 >= 1 & hrtCondP2000 <= 5,
                               hrtCondP2000, NA)) %>%
  mutate(hrtCondP2000 = car::recode(hrtCondP2000,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(hrtCondP2000 = factor(hrtCondP2000, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(hrtCondP2002 = ifelse(hrtCondP2002 >= 1 & hrtCondP2002 <= 5,
                               hrtCondP2002, NA)) %>%
  mutate(hrtCondP2002 = car::recode(hrtCondP2002,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(hrtCondP2002 = factor(hrtCondP2002, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(hrtCondP2004 = ifelse(hrtCondP2004 >= 1 & hrtCondP2004 <= 5,
                               hrtCondP2004, NA)) %>%
  mutate(hrtCondP2004 = car::recode(hrtCondP2004,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(hrtCondP2004 = factor(hrtCondP2004, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(hrtCondP2006 = ifelse(hrtCondP2006 >= 1 & hrtCondP2006 <= 5,
                               hrtCondP2006, NA)) %>%
  mutate(hrtCondP2006 = car::recode(hrtCondP2006,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(hrtCondP2006 = factor(hrtCondP2006, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(hrtCondP2008 = ifelse(hrtCondP2008 >= 1 & hrtCondP2008 <= 5,
                               hrtCondP2008, NA)) %>%
  mutate(hrtCondP2008 = car::recode(hrtCondP2008,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(hrtCondP2008 = factor(hrtCondP2008, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(hrtCondP2010 = ifelse(hrtCondP2010 >= 1 & hrtCondP2010 <= 5,
                               hrtCondP2010, NA)) %>%
  mutate(hrtCondP2010 = car::recode(hrtCondP2010,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(hrtCondP2010 = factor(hrtCondP2010, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(hrtCondP2012 = ifelse(hrtCondP2012 >= 1 & hrtCondP2012 <= 5,
                               hrtCondP2012, NA)) %>%
  mutate(hrtCondP2012 = car::recode(hrtCondP2012,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(hrtCondP2012 = factor(hrtCondP2012, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(hrtCondP2014 = ifelse(hrtCondP2014 >= 1 & hrtCondP2014 <= 5,
                               hrtCondP2014, NA)) %>%
  mutate(hrtCondP2014 = car::recode(hrtCondP2014,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(hrtCondP2014 = factor(hrtCondP2014, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(hrtCondP2016 = ifelse(hrtCondP2016 >= 1 & hrtCondP2016 <= 5,
                               hrtCondP2016, NA)) %>%
  mutate(hrtCondP2016 = car::recode(hrtCondP2016,
                                    "5 = 0;
                                       3 = 1;
                                       4 = 0")) %>%
  mutate(hrtCondP2016 = factor(hrtCondP2016, levels = c(0,1),
                               labels = c("no", "yes"))) %>%
  mutate(hrtCondP2018 = ifelse(hrtCondP2018 >= 1 & hrtCondP2018 <= 6,
                               hrtCondP2018, NA)) %>%
  mutate(hrtCondP2018 = car::recode(hrtCondP2018,
                                    "5 = 0;
                                       4 = 0;
                                    6 = 0")) %>%
  mutate(hrtCondP2018 = factor(hrtCondP2018, levels = c(0,1),
                               labels = c("no", "yes")))

#-----------------------------------
# Angina

# Do not propagate as there are treatments like angioplasty that could reduce pain.

# set to 0 if response to screener question about heart conditions is also 0
df$anginaP1998[which(df$hrtCondP1998 == "no")] <- 0
df$anginaP2000[which(df$hrtCondP2000 == "no")] <- 0
df$anginaP2002[which(df$hrtCondP2002 == "no")] <- 0
df$anginaP2004[which(df$hrtCondP2004 == "no")] <- 0
df$anginaP2006[which(df$hrtCondP2006 == "no")] <- 0
df$anginaP2008[which(df$hrtCondP2008 == "no")] <- 0
df$anginaP2010[which(df$hrtCondP2010 == "no")] <- 0
df$anginaP2012[which(df$hrtCondP2012 == "no")] <- 0
df$anginaP2014[which(df$hrtCondP2014 == "no")] <- 0
df$anginaP2016[which(df$hrtCondP2016 == "no")] <- 0
df$anginaP2018[which(df$hrtCondP2018 == "no")] <- 0

df <- df %>%
  mutate(anginaP1998 = ifelse(anginaP1998 >= 0 & anginaP1998 <= 5,
                              anginaP1998, NA)) %>%
  mutate(anginaP1998 = car::recode(anginaP1998,
                                   "0 = 0;
                                   5 = 0")) %>%
  mutate(anginaP1998 = factor(anginaP1998, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(anginaP2000 = ifelse(anginaP2000 >= 0 & anginaP2000 <= 5,
                              anginaP2000, NA)) %>%
  mutate(anginaP2000 = car::recode(anginaP2000,
                                   "0 = 0;
                                   5 = 0")) %>%
  mutate(anginaP2000 = factor(anginaP2000, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(anginaP2002 = ifelse(anginaP2002 >= 0 & anginaP2002 <= 5,
                              anginaP2002, NA)) %>%
  mutate(anginaP2002 = car::recode(anginaP2002,
                                   "0 = 0;
                                   5 = 0")) %>%
  mutate(anginaP2002 = factor(anginaP2002, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(anginaP2004 = ifelse(anginaP2004 >= 0 & anginaP2004 <= 5,
                              anginaP2004, NA)) %>%
  mutate(anginaP2004 = car::recode(anginaP2004,
                                   "0 = 0;
                                   5 = 0")) %>%
  mutate(anginaP2004 = factor(anginaP2004, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(anginaP2006 = ifelse(anginaP2006 >= 0 & anginaP2006 <= 5,
                              anginaP2006, NA)) %>%
  mutate(anginaP2006 = car::recode(anginaP2006,
                                   "0 = 0;
                                   5 = 0")) %>%
  mutate(anginaP2006 = factor(anginaP2006, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(anginaP2008 = ifelse(anginaP2008 >= 0 & anginaP2008 <= 5,
                              anginaP2008, NA)) %>%
  mutate(anginaP2008 = car::recode(anginaP2008,
                                   "0 = 0;
                                   5 = 0")) %>%
  mutate(anginaP2008 = factor(anginaP2008, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(anginaP2010 = ifelse(anginaP2010 >= 0 & anginaP2010 <= 5,
                              anginaP2010, NA)) %>%
  mutate(anginaP2010 = car::recode(anginaP2010,
                                   "0 = 0;
                                   5 = 0")) %>%
  mutate(anginaP2010 = factor(anginaP2010, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(anginaP2012 = ifelse(anginaP2012 >= 0 & anginaP2012 <= 5,
                              anginaP2012, NA)) %>%
  mutate(anginaP2012 = car::recode(anginaP2012,
                                   "0 = 0;
                                   5 = 0")) %>%
  mutate(anginaP2012 = factor(anginaP2012, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(anginaP2014 = ifelse(anginaP2014 >= 0 & anginaP2014 <= 5,
                              anginaP2014, NA)) %>%
  mutate(anginaP2014 = car::recode(anginaP2014,
                                   "0 = 0;
                                   5 = 0")) %>%
  mutate(anginaP2014 = factor(anginaP2014, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(anginaP2016 = ifelse(anginaP2016 >= 0 & anginaP2016 <= 5,
                              anginaP2016, NA)) %>%
  mutate(anginaP2016 = car::recode(anginaP2016,
                                   "0 = 0;
                                   5 = 0")) %>%
  mutate(anginaP2016 = factor(anginaP2016, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(anginaP2018 = ifelse(anginaP2018 >= 0 & anginaP2018 <= 5,
                              anginaP2018, NA)) %>%
  mutate(anginaP2018 = car::recode(anginaP2018,
                                   "0 = 0;
                                   5 = 0")) %>%
  mutate(anginaP2018 = factor(anginaP2018, levels = c(0,1),
                              labels = c("no", "yes")))


#-----------------------------------
# Stroke

# If a person denies a previous report of stroke, code that they have not had a
# stroke

# Do not count "possible strokes" (2) as strokes; set to No.

# In 2018, there was no 3, and 4 means they never had a stroke

df <- df %>%
  mutate(strokeP1998 = ifelse(strokeP1998 >= 1 & strokeP1998 <= 5,
                              strokeP1998, NA)) %>%
  mutate(strokeP1998 = car::recode(strokeP1998,
                                   "5 = 0;
                                    2 = 0;
                                  3 = 0")) %>%
  mutate(strokeP1998 = factor(strokeP1998, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(strokeP2000 = ifelse(strokeP2000 >= 1 & strokeP2000 <= 5,
                              strokeP2000, NA)) %>%
  mutate(strokeP2000 = car::recode(strokeP2000,
                                   "5 = 0;
                                  4 = 0;
                                  3 = 1;
                                  2 = 0")) %>%
  mutate(strokeP2000 = factor(strokeP2000, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(strokeP2002 = ifelse(strokeP2002 >= 1 & strokeP2002 <= 5,
                              strokeP2002, NA)) %>%
  mutate(strokeP2002 = car::recode(strokeP2002,
                                   "5 = 0;
                                  4 = 0;
                                  3 = 1;
                                  2 = 0")) %>%
  mutate(strokeP2002 = factor(strokeP2002, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(strokeP2004 = ifelse(strokeP2004 >= 1 & strokeP2004 <= 5,
                              strokeP2004, NA)) %>%
  mutate(strokeP2004 = car::recode(strokeP2004,
                                   "5 = 0;
                                  4 = 0;
                                  3 = 1;
                                  2 = 0")) %>%
  mutate(strokeP2004 = factor(strokeP2004, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(strokeP2006 = ifelse(strokeP2006 >= 1 & strokeP2006 <= 5,
                              strokeP2006, NA)) %>%
  mutate(strokeP2006 = car::recode(strokeP2006,
                                   "5 = 0;
                                  4 = 0;
                                  3 = 1;
                                  2 = 0")) %>%
  mutate(strokeP2006 = factor(strokeP2006, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(strokeP2008 = ifelse(strokeP2008 >= 1 & strokeP2008 <= 5,
                              strokeP2008, NA)) %>%
  mutate(strokeP2008 = car::recode(strokeP2008,
                                   "5 = 0;
                                  4 = 0;
                                  3 = 1;
                                  2 = 0")) %>%
  mutate(strokeP2008 = factor(strokeP2008, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(strokeP2010 = ifelse(strokeP2010 >= 1 & strokeP2010 <= 5,
                              strokeP2010, NA)) %>%
  mutate(strokeP2010 = car::recode(strokeP2010,
                                   "5 = 0;
                                  4 = 0;
                                  3 = 1;
                                  2 = 0")) %>%
  mutate(strokeP2010 = factor(strokeP2010, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(strokeP2012 = ifelse(strokeP2012 >= 1 & strokeP2012 <= 5,
                              strokeP2012, NA)) %>%
  mutate(strokeP2012 = car::recode(strokeP2012,
                                   "5 = 0;
                                  4 = 0;
                                  3 = 1;
                                  2 = 0")) %>%
  mutate(strokeP2012 = factor(strokeP2012, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(strokeP2014 = ifelse(strokeP2014 >= 1 & strokeP2014 <= 5,
                              strokeP2014, NA)) %>%
  mutate(strokeP2014 = car::recode(strokeP2014,
                                   "5 = 0;
                                  4 = 0;
                                  3 = 1;
                                  2 = 0")) %>%
  mutate(strokeP2014 = factor(strokeP2014, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(strokeP2016 = ifelse(strokeP2016 >= 1 & strokeP2016 <= 5,
                              strokeP2016, NA)) %>%
  mutate(strokeP2016 = car::recode(strokeP2016,
                                   "5 = 0;
                                  4 = 0;
                                  3 = 1;
                                  2 = 0")) %>%
  mutate(strokeP2016 = factor(strokeP2016, levels = c(0,1),
                              labels = c("no", "yes"))) %>%
  mutate(strokeP2018 = ifelse(strokeP2018 >= 1 & strokeP2018 <= 6,
                              strokeP2018, NA)) %>%
  mutate(strokeP2018 = car::recode(strokeP2018,
                                   "5 = 0;
                                  4 = 0;
                                  2 = 0")) %>%
  mutate(strokeP2018 = factor(strokeP2018, levels = c(0,1),
                              labels = c("no", "yes")))

# propogate stroke values forward
df$strokeP2000[which(is.na(df$strokeP2000) == TRUE &
                       df$strokeP1998 == 1 & df$deadP2000 == 0)] <- "yes"
df$strokeP2002[which(is.na(df$strokeP2002) == TRUE &
                       df$strokeP2000 == 1 & df$deadP2002 == 0)] <- "yes"
df$strokeP2004[which(is.na(df$strokeP2004) == TRUE &
                       df$strokeP2002 == 1 & df$deadP2004 == 0)] <- "yes"
df$strokeP2006[which(is.na(df$strokeP2006) == TRUE &
                       df$strokeP2004 == 1 & df$deadP2006 == 0)] <- "yes"
df$strokeP2008[which(is.na(df$strokeP2008) == TRUE &
                       df$strokeP2006 == 1 & df$deadP2008 == 0)] <- "yes"
df$strokeP2010[which(is.na(df$strokeP2010) == TRUE &
                       df$strokeP2008 == 1 & df$deadP2010 == 0)] <- "yes"
df$strokeP2012[which(is.na(df$strokeP2012) == TRUE &
                       df$strokeP2010 == 1 & df$deadP2012 == 0)] <- "yes"
df$strokeP2014[which(is.na(df$strokeP2014) == TRUE &
                       df$strokeP2012 == 1 & df$deadP2014 == 0)] <- "yes"
df$strokeP2016[which(is.na(df$strokeP2016) == TRUE &
                       df$strokeP2014 == 1 & df$deadP2016 == 0)] <- "yes"
df$strokeP2018[which(is.na(df$strokeP2018) == TRUE &
                       df$strokeP2016 == 1 & df$deadP2018 == 0)] <- "yes"

#-----------------------------------
# Arthritis

# If a person denies a previous report of arthritis, code that they do not
# have arthritis

# In 2018, there was no 3, 4 means they never had arthritis, and 6 means they
# had arthritis but don't have it now

df <- df %>%
  mutate(arthritisP1998 = ifelse(arthritisP1998 >= 1 & arthritisP1998 <= 5,
                                 arthritisP1998, NA)) %>%
  mutate(arthritisP1998 = car::recode(arthritisP1998,
                                      "5 = 0;
                                  3 = 0")) %>%
  mutate(arthritisP1998 = factor(arthritisP1998, levels = c(0,1),
                                 labels = c("no", "yes"))) %>%
  mutate(arthritisP2000 = ifelse(arthritisP2000 >= 1 & arthritisP2000 <= 5,
                                 arthritisP2000, NA)) %>%
  mutate(arthritisP2000 = car::recode(arthritisP2000,
                                      "5 = 0;
                                  4 = 0;
                                  3 = 1")) %>%
  mutate(arthritisP2000 = factor(arthritisP2000, levels = c(0,1),
                                 labels = c("no", "yes"))) %>%
  mutate(arthritisP2002 = ifelse(arthritisP2002 >= 1 & arthritisP2002 <= 5,
                                 arthritisP2002, NA)) %>%
  mutate(arthritisP2002 = car::recode(arthritisP2002,
                                      "5 = 0;
                                  4 = 0;
                                  3 = 1")) %>%
  mutate(arthritisP2002 = factor(arthritisP2002, levels = c(0,1),
                                 labels = c("no", "yes"))) %>%
  mutate(arthritisP2004 = ifelse(arthritisP2004 >= 1 & arthritisP2004 <= 5,
                                 arthritisP2004, NA)) %>%
  mutate(arthritisP2004 = car::recode(arthritisP2004,
                                      "5 = 0;
                                      4 = 0;
                                      3 = 1")) %>%
  mutate(arthritisP2004 = factor(arthritisP2004, levels = c(0,1),
                                 labels = c("no", "yes"))) %>%
  mutate(arthritisP2006 = ifelse(arthritisP2006 >= 1 & arthritisP2006 <= 5,
                                 arthritisP2006, NA)) %>%
  mutate(arthritisP2006 = car::recode(arthritisP2006,
                                      "5 = 0;
                                  4 = 0;
                                  3 = 1")) %>%
  mutate(arthritisP2006 = factor(arthritisP2006, levels = c(0,1),
                                 labels = c("no", "yes"))) %>%
  mutate(arthritisP2008 = ifelse(arthritisP2008 >= 1 & arthritisP2008 <= 5,
                                 arthritisP2008, NA)) %>%
  mutate(arthritisP2008 = car::recode(arthritisP2008,
                                      "5 = 0;
                                  4 = 0;
                                  3 = 1")) %>%
  mutate(arthritisP2008 = factor(arthritisP2008, levels = c(0,1),
                                 labels = c("no", "yes"))) %>%
  mutate(arthritisP2010 = ifelse(arthritisP2010 >= 1 & arthritisP2010 <= 5,
                                 arthritisP2010, NA)) %>%
  mutate(arthritisP2010 = car::recode(arthritisP2010,
                                      "5 = 0;
                                  4 = 0;
                                  3 = 1")) %>%
  mutate(arthritisP2010 = factor(arthritisP2010, levels = c(0,1),
                                 labels = c("no", "yes"))) %>%
  mutate(arthritisP2012 = ifelse(arthritisP2012 >= 1 & arthritisP2012 <= 5,
                                 arthritisP2012, NA)) %>%
  mutate(arthritisP2012 = car::recode(arthritisP2012,
                                      "5 = 0;
                                  4 = 0;
                                  3 = 1")) %>%
  mutate(arthritisP2012 = factor(arthritisP2012, levels = c(0,1),
                                 labels = c("no", "yes"))) %>%
  mutate(arthritisP2014 = ifelse(arthritisP2014 >= 1 & arthritisP2014 <= 5,
                                 arthritisP2014, NA)) %>%
  mutate(arthritisP2014 = car::recode(arthritisP2014,
                                      "5 = 0;
                                  4 = 0;
                                  3 = 1")) %>%
  mutate(arthritisP2014 = factor(arthritisP2014, levels = c(0,1),
                                 labels = c("no", "yes"))) %>%
  mutate(arthritisP2016 = ifelse(arthritisP2016 >= 1 & arthritisP2016 <= 5,
                                 arthritisP2016, NA)) %>%
  mutate(arthritisP2016 = car::recode(arthritisP2016,
                                      "5 = 0;
                                  4 = 0;
                                  3 = 1")) %>%
  mutate(arthritisP2016 = factor(arthritisP2016, levels = c(0,1),
                                 labels = c("no", "yes"))) %>%
  mutate(arthritisP2018 = ifelse(arthritisP2018 >= 1 & arthritisP2018 <= 6,
                                 arthritisP2018, NA)) %>%
  mutate(arthritisP2018 = car::recode(arthritisP2018,
                                      "5 = 0;
                                  4 = 0;
                                  6 = 0")) %>%
  mutate(arthritisP2018 = factor(arthritisP2018, levels = c(0,1),
                                 labels = c("no", "yes")))

#-----------------------------------
# BMI category includes data from pre-1998 for the sake of propagating forward

# set impossible values to NA
df <- df %>%
  mutate(weight1992 = ifelse(weight1992 >= 997, NA, weight1992)) %>%
  mutate(htFeet1992 = ifelse(htFeet1992 >= 8, NA, htFeet1992)) %>%
  mutate(htInches1992 = ifelse(htInches1992 > 12, NA, htInches1992)) %>%
  mutate(weight1993 = ifelse(weight1993 >= 997, NA, weight1993)) %>%
  mutate(htInches1993 = ifelse(htInches1993 > 96, NA, htInches1993)) %>%
  mutate(weight1994 = ifelse(weight1994 >= 997, NA, weight1994)) %>%
  mutate(htFeet1994 = ifelse(htFeet1994 >= 8, NA, htFeet1994)) %>%
  mutate(htInches1994 = ifelse(htInches1994 > 12, NA, htInches1994)) %>%
  mutate(weight1995 = ifelse(weight1995 >= 997, NA, weight1995)) %>%
  mutate(htFeet1995 = ifelse(htFeet1995 >= 8, NA, htFeet1995)) %>%
  mutate(htInches1995 = ifelse(htInches1995 > 12, NA, htInches1995)) %>%
  mutate(weight1996 = ifelse(weight1996 >= 997, NA, weight1996)) %>%
  mutate(htFeet1996 = ifelse(htFeet1996 >= 8, NA, htFeet1996)) %>%
  mutate(htInches1996 = ifelse(htInches1996 > 12, NA, htInches1996)) %>%
  mutate(weight1998 = ifelse(weight1998 >= 997, NA, weight1998)) %>%
  mutate(htFeet1998 = ifelse(htFeet1998 >= 8, NA, htFeet1998)) %>%
  mutate(htInches1998 = ifelse(htInches1998 > 12, NA, htInches1998)) %>%
  mutate(weight2000 = ifelse(weight2000 >= 997, NA, weight2000)) %>%
  mutate(htFeet2000 = ifelse(htFeet2000 >= 8, NA, htFeet2000)) %>%
  mutate(htInches2000 = ifelse(htInches2000 > 12, NA, htInches2000)) %>%
  mutate(weight2002 = ifelse(weight2002 >= 997, NA, weight2002)) %>%
  mutate(htFeet2002 = ifelse(htFeet2002 >= 8, NA, htFeet2002)) %>%
  mutate(htInches2002 = ifelse(htInches2002 > 12, NA, htInches2002)) %>%
  mutate(weight2004 = ifelse(weight2004 >= 997, NA, weight2004)) %>%
  mutate(htFeet2004 = ifelse(htFeet2004 >= 8, NA, htFeet2004)) %>%
  mutate(htInches2004 = ifelse(htInches2004 > 12, NA, htInches2004)) %>%
  mutate(weight2006 = ifelse(weight2006 >= 997, NA, weight2006)) %>%
  mutate(htFeet2006 = ifelse(htFeet2006 >= 8, NA, htFeet2006)) %>%
  mutate(htInches2006 = ifelse(htInches2006 > 12, NA, htInches2006)) %>%
  mutate(weight2008 = ifelse(weight2008 >= 997, NA, weight2008)) %>%
  mutate(htFeet2008 = ifelse(htFeet2008 >= 8, NA, htFeet2008)) %>%
  mutate(htInches2008 = ifelse(htInches2008 > 12, NA, htInches2008)) %>%
  mutate(weight2010 = ifelse(weight2010 >= 997, NA, weight2010)) %>%
  mutate(htFeet2010 = ifelse(htFeet2010 >= 8, NA, htFeet2010)) %>%
  mutate(htInches2010 = ifelse(htInches2010 > 12, NA, htInches2010)) %>%
  mutate(weight2012 = ifelse(weight2012 >= 997, NA, weight2012)) %>%
  mutate(htFeet2012 = ifelse(htFeet2012 >= 8, NA, htFeet2012)) %>%
  mutate(htInches2012 = ifelse(htInches2012 > 12, NA, htInches2012)) %>%
  mutate(weight2014 = ifelse(weight2014 >= 997, NA, weight2014)) %>%
  mutate(htFeet2014 = ifelse(htFeet2014 >= 8, NA, htFeet2014)) %>%
  mutate(htInches2014 = ifelse(htInches2014 > 12, NA, htInches2014)) %>%
  mutate(weight2016 = ifelse(weight2016 >= 997, NA, weight2016)) %>%
  mutate(htFeet2016 = ifelse(htFeet2016 >= 8, NA, htFeet2016)) %>%
  mutate(htInches2016 = ifelse(htInches2016 > 12, NA, htInches2016)) %>%
  mutate(weight2018 = ifelse(weight2018 >= 997 | weight2018 < 0, NA, weight2018)) %>%
  mutate(htFeet2018 = ifelse(htFeet2018 >= 8, NA, htFeet2018)) %>%
  mutate(htInches2018 = ifelse(htInches2018 > 12, NA, htInches2018))

# calculate height in inches
df <- df %>%
  mutate(height1992 = ((12 * htFeet1992) + htInches1992)) %>%
  mutate(height1993 = htInches1993) %>% # recordd in inches only in 1993
  mutate(height1994 = ((12 * htFeet1994) + htInches1994)) %>%
  mutate(height1995 = ((12 * htFeet1995) + htInches1995)) %>%
  mutate(height1996 = ((12 * htFeet1996) + htInches1996)) %>%
  mutate(height1998 = ((12 * htFeet1998) + htInches1998)) %>%
  mutate(height2000 = ((12 * htFeet2000) + htInches2000)) %>%
  mutate(height2002 = ((12 * htFeet2002) + htInches2002)) %>%
  mutate(height2004 = ((12 * htFeet2004) + htInches2004)) %>%
  mutate(height2006 = ((12 * htFeet2006) + htInches2006)) %>%
  mutate(height2008 = ((12 * htFeet2008) + htInches2008)) %>%
  mutate(height2010 = ((12 * htFeet2010) + htInches2010)) %>%
  mutate(height2012 = ((12 * htFeet2012) + htInches2012)) %>%
  mutate(height2014 = ((12 * htFeet2014) + htInches2014)) %>%
  mutate(height2016 = ((12 * htFeet2016) + htInches2016)) %>%
  mutate(height2018 = ((12 * htFeet2018) + htInches2018))

# the heightXXXX and weightXXXX variables are self-reported measures, while the
# measuredHgtXXXX and measuredWgtXXXX variables are actual measurements
# recorded by the interviewer. As far as I can tell, this collecting of
# physical measurements started in 2004.

# Proposed strategy:
#   - start with self-reported heights (more consistently recorded)
#   - if possible, fill missing height values with measured values for
#     that year
#   - if values are still missing, propagate forward and then backwards across
#     all waves (not just nearby ones)


# make sure no unreasonable measurements first - set max weight at 400 pounds and
# max height at < 8 Ft (<96). Set min weight to 60 pounds and min height to 3 Ft
# (36 inches)
df <- df %>%
  mutate(measuredWgt2004 = ifelse(measuredWgt2004 > 60 & measuredWgt2004 <= 400,
                                  measuredWgt2004, NA)) %>%
  mutate(measuredHgt2004 = ifelse(measuredHgt2004 > 36 & measuredHgt2004 <= 96,
                                  measuredHgt2004, NA)) %>%
  mutate(measuredWgt2006 = ifelse(measuredWgt2006 > 60 & measuredWgt2006 <= 400,
                                  measuredWgt2006, NA)) %>%
  mutate(measuredHgt2006 = ifelse(measuredHgt2006 > 36 & measuredHgt2006 <= 96,
                                  measuredHgt2006, NA)) %>%
  mutate(measuredWgt2008 = ifelse(measuredWgt2008 > 60 & measuredWgt2008 <= 400,
                                  measuredWgt2008, NA)) %>%
  mutate(measuredHgt2008 = ifelse(measuredHgt2008 > 36 & measuredHgt2008 <= 96,
                                  measuredHgt2008, NA)) %>%
  mutate(measuredWgt2010 = ifelse(measuredWgt2010 > 60 & measuredWgt2010 <= 400,
                                  measuredWgt2010, NA)) %>%
  mutate(measuredHgt2010 = ifelse(measuredHgt2010 > 36 & measuredHgt2010 <= 96,
                                  measuredHgt2010, NA)) %>%
  mutate(measuredWgt2012 = ifelse(measuredWgt2012 > 60 & measuredWgt2012 <= 400,
                                  measuredWgt2012, NA)) %>%
  mutate(measuredHgt2012 = ifelse(measuredHgt2012 > 36 & measuredHgt2012 <= 96,
                                  measuredHgt2012, NA)) %>%
  mutate(measuredWgt2014 = ifelse(measuredWgt2014 > 60 & measuredWgt2014 <= 400,
                                  measuredWgt2014, NA)) %>%
  mutate(measuredHgt2014 = ifelse(measuredHgt2014 > 36 & measuredHgt2014 <= 96,
                                  measuredHgt2014, NA)) %>%
  mutate(measuredWgt2016 = ifelse(measuredWgt2016 > 60 & measuredWgt2016 <= 400,
                                  measuredWgt2016, NA)) %>%
  mutate(measuredHgt2016 = ifelse(measuredHgt2016 > 36 & measuredHgt2016 <= 96,
                                  measuredHgt2016, NA)) %>%
  mutate(measuredWgt2018 = ifelse(measuredWgt2018 > 60 & measuredWgt2018 <= 400,
                                  measuredWgt2018, NA)) %>%
  mutate(measuredHgt2018 = ifelse(measuredHgt2018 > 36 & measuredHgt2018 <= 96,
                                  measuredHgt2018, NA))

# first, try fill in missing values with measured values at each wave

# height
df$height2004[which(is.na(df$height2004) == TRUE &
                      df$deadP2004 == 0)] <- df$measuredHgt2004[which(is.na(df$height2004) == TRUE &
                                                                        df$deadP2004 == 0)]
df$height2006[which(is.na(df$height2006) == TRUE &
                      df$deadP2006 == 0)] <- df$measuredHgt2006[which(is.na(df$height2006) == TRUE &
                                                                        df$deadP2006 == 0)]
df$height2008[which(is.na(df$height2008) == TRUE &
                      df$deadP2008 == 0)] <- df$measuredHgt2008[which(is.na(df$height2008) == TRUE &
                                                                        df$deadP2008 == 0)]
df$height2010[which(is.na(df$height2010) == TRUE &
                      df$deadP2010 == 0)] <- df$measuredHgt2010[which(is.na(df$height2010) == TRUE &
                                                                        df$deadP2010 == 0)]
df$height2012[which(is.na(df$height2012) == TRUE &
                      df$deadP2012 == 0)] <- df$measuredHgt2012[which(is.na(df$height2012) == TRUE &
                                                                        df$deadP2012 == 0)]
df$height2014[which(is.na(df$height2014) == TRUE &
                      df$deadP2014 == 0)] <- df$measuredHgt2014[which(is.na(df$height2014) == TRUE &
                                                                        df$deadP2014 == 0)]
df$height2016[which(is.na(df$height2016) == TRUE &
                      df$deadP2016 == 0)] <- df$measuredHgt2016[which(is.na(df$height2016) == TRUE &
                                                                        df$deadP2016 == 0)]
df$height2018[which(is.na(df$height2018) == TRUE &
                      df$deadP2018 == 0)] <- df$measuredHgt2018[which(is.na(df$height2018) == TRUE &
                                                                        df$deadP2018 == 0)]
# weight
df$weight2004[which(is.na(df$weight2004) == TRUE &
                      df$deadP2004 == 0)] <- df$measuredWgt2004[which(is.na(df$weight2004) == TRUE &
                                                                        df$deadP2004 == 0)]
df$weight2006[which(is.na(df$weight2006) == TRUE &
                      df$deadP2006 == 0)] <- df$measuredWgt2006[which(is.na(df$weight2006) == TRUE &
                                                                        df$deadP2006 == 0)]
df$weight2008[which(is.na(df$weight2008) == TRUE &
                      df$deadP2008 == 0)] <- df$measuredWgt2008[which(is.na(df$weight2008) == TRUE &
                                                                        df$deadP2008 == 0)]
df$weight2010[which(is.na(df$weight2010) == TRUE &
                      df$deadP2010 == 0)] <- df$measuredWgt2010[which(is.na(df$weight2010) == TRUE &
                                                                        df$deadP2010 == 0)]
df$weight2012[which(is.na(df$weight2012) == TRUE &
                      df$deadP2012 == 0)] <- df$measuredWgt2012[which(is.na(df$weight2012) == TRUE &
                                                                        df$deadP2012 == 0)]
df$weight2014[which(is.na(df$weight2014) == TRUE &
                      df$deadP2014 == 0)] <- df$measuredWgt2014[which(is.na(df$weight2014) == TRUE &
                                                                        df$deadP2014 == 0)]
df$weight2016[which(is.na(df$weight2016) == TRUE &
                      df$deadP2016 == 0)] <- df$measuredWgt2016[which(is.na(df$weight2016) == TRUE &
                                                                        df$deadP2016 == 0)]
df$weight2018[which(is.na(df$weight2018) == TRUE &
                      df$deadP2018 == 0)] <- df$measuredWgt2018[which(is.na(df$weight2018) == TRUE &
                                                                        df$deadP2018 == 0)]

# Propagate height forward first, then backwards.
df$height1993[which(is.na(df$height1993) == TRUE)] <- df$height1992[which(is.na(df$height1993) == TRUE)]
df$height1994[which(is.na(df$height1994) == TRUE)] <- df$height1993[which(is.na(df$height1994) == TRUE)]
df$height1995[which(is.na(df$height1995) == TRUE)] <- df$height1994[which(is.na(df$height1995) == TRUE)]
df$height1996[which(is.na(df$height1996) == TRUE)] <- df$height1995[which(is.na(df$height1996) == TRUE)]
df$height1998[which(is.na(df$height1998) == TRUE &
                      df$deadP1998 == 0)] <- df$height1996[which(is.na(df$height1998) == TRUE &
                                                                   df$deadP1998 == 0)]
df$height2000[which(is.na(df$height2000) == TRUE &
                      df$deadP2000 == 0)] <- df$height1998[which(is.na(df$height2000) == TRUE &
                                                                   df$deadP2000 == 0)]
df$height2002[which(is.na(df$height2002) == TRUE &
                      df$deadP2002 == 0)] <- df$height2000[which(is.na(df$height2002) == TRUE &
                                                                   df$deadP2002 == 0)]
df$height2004[which(is.na(df$height2004) == TRUE &
                      df$deadP2004 == 0)] <- df$height2002[which(is.na(df$height2004) == TRUE &
                                                                   df$deadP2004 == 0)]
df$height2006[which(is.na(df$height2006) == TRUE &
                      df$deadP2006 == 0)] <- df$height2004[which(is.na(df$height2006) == TRUE &
                                                                   df$deadP2006 == 0)]
df$height2008[which(is.na(df$height2008) == TRUE &
                      df$deadP2008 == 0)] <- df$height2006[which(is.na(df$height2008) == TRUE &
                                                                   df$deadP2008 == 0)]
df$height2010[which(is.na(df$height2010) == TRUE &
                      df$deadP2010 == 0)] <- df$height2008[which(is.na(df$height2010) == TRUE &
                                                                   df$deadP2010 == 0)]
df$height2012[which(is.na(df$height2012) == TRUE &
                      df$deadP2012 == 0)] <- df$height2010[which(is.na(df$height2012) == TRUE &
                                                                   df$deadP2012 == 0)]
df$height2014[which(is.na(df$height2014) == TRUE &
                      df$deadP2014 == 0)] <- df$height2012[which(is.na(df$height2014) == TRUE &
                                                                   df$deadP2014 == 0)]
df$height2016[which(is.na(df$height2016) == TRUE &
                      df$deadP2016 == 0)] <- df$height2014[which(is.na(df$height2016) == TRUE &
                                                                   df$deadP2016 == 0)]
df$height2018[which(is.na(df$height2018) == TRUE &
                      df$deadP2018 == 0)] <- df$height2016[which(is.na(df$height2018) == TRUE &
                                                                   df$deadP2018 == 0)]
# propagate backwards
df$height2016[which(is.na(df$height2016) == TRUE)] <- df$height2018[which(is.na(df$height2016) == TRUE)]
df$height2014[which(is.na(df$height2014) == TRUE)] <- df$height2016[which(is.na(df$height2014) == TRUE)]
df$height2012[which(is.na(df$height2012) == TRUE)] <- df$height2014[which(is.na(df$height2012) == TRUE)]
df$height2010[which(is.na(df$height2010) == TRUE)] <- df$height2012[which(is.na(df$height2010) == TRUE)]
df$height2008[which(is.na(df$height2008) == TRUE)] <- df$height2010[which(is.na(df$height2008) == TRUE)]
df$height2006[which(is.na(df$height2006) == TRUE)] <- df$height2008[which(is.na(df$height2006) == TRUE)]
df$height2004[which(is.na(df$height2004) == TRUE)] <- df$height2006[which(is.na(df$height2004) == TRUE)]
df$height2002[which(is.na(df$height2002) == TRUE)] <- df$height2004[which(is.na(df$height2002) == TRUE)]
df$height2000[which(is.na(df$height2000) == TRUE)] <- df$height2002[which(is.na(df$height2000) == TRUE)]
df$height1998[which(is.na(df$height1998) == TRUE)] <- df$height2000[which(is.na(df$height1998) == TRUE)]

# do not propogate weight backwards or forewards as it is more susceptible to
# fluctuation than height

# create vector of BMI category labels
bmi_cats <- c("Underweight (<18.5)", "Normal weight (<25)",
              "Overweight (<30),", "Obese 1 (<35)", "Obese 2 (<40)",
              "Obese 3 (40+)")

# calculate BMI and convert to factor - Use conversion factor (of 703) since
# original formula expects kgs and meters.
df <- df %>%
  mutate(bmi1998 = (( weight1998 / (height1998 ^ 2) ) * 703)) %>%
  mutate(bmi2000 = (( weight2000 / (height2000 ^ 2) ) * 703)) %>%
  mutate(bmi2002 = (( weight2002 / (height2002 ^ 2) ) * 703)) %>%
  mutate(bmi2004 = (( weight2004 / (height2004 ^ 2) ) * 703)) %>%
  mutate(bmi2006 = (( weight2006 / (height2006 ^ 2) ) * 703)) %>%
  mutate(bmi2008 = (( weight2008 / (height2008 ^ 2) ) * 703)) %>%
  mutate(bmi2010 = (( weight2010 / (height2010 ^ 2) ) * 703)) %>%
  mutate(bmi2012 = (( weight2012 / (height2012 ^ 2) ) * 703)) %>%
  mutate(bmi2014 = (( weight2014 / (height2014 ^ 2) ) * 703)) %>%
  mutate(bmi2016 = (( weight2016 / (height2016 ^ 2) ) * 703)) %>%
  mutate(bmi2018 = (( weight2018 / (height2018 ^ 2) ) * 703)) %>%
  mutate(bmi6Cats1998 = factor(
    cut(bmi1998, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(bmi6Cats2000 = factor(
    cut(bmi2000, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(bmi6Cats2002 = factor(
    cut(bmi2002, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(bmi6Cats2004 = factor(
    cut(bmi2004, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(bmi6Cats2006 = factor(
    cut(bmi2006, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(bmi6Cats2008 = factor(
    cut(bmi2008, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(bmi6Cats2010 = factor(
    cut(bmi2010, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(bmi6Cats2012 = factor(
    cut(bmi2012, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(bmi6Cats2014 = factor(
    cut(bmi2014, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(bmi6Cats2016 = factor(
    cut(bmi2016, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(bmi6Cats2018 = factor(
    cut(bmi2018, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats))

# create BMI classes from RAND longitudinal dataset calculated BMI
df <- df %>%
  mutate(randbmi6Cats1998 = factor(
    cut(randBMI1998, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(randbmi6Cats2000 = factor(
    cut(randBMI2000, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(randbmi6Cats2002 = factor(
    cut(randBMI2002, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(randbmi6Cats2004 = factor(
    cut(randBMI2004, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(randbmi6Cats2006 = factor(
    cut(randBMI2006, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(randbmi6Cats2008 = factor(
    cut(randBMI2008, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(randbmi6Cats2010 = factor(
    cut(randBMI2010, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(randbmi6Cats2012 = factor(
    cut(randBMI2012, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(randbmi6Cats2014 = factor(
    cut(randBMI2014, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(randbmi6Cats2016 = factor(
    cut(randBMI2016, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats)) %>%
  mutate(randbmi6Cats2018 = factor(
    cut(randBMI2018, breaks = c(0, 18.5, 25, 30, 35, 40, Inf)),
    labels = bmi_cats))


#---------------------------
# RAND CES-D score category

summary(as.factor(df$randCESD1998))
summary(as.factor(df$randCESD2000))
summary(as.factor(df$randCESD2002))
summary(as.factor(df$randCESD2004))
summary(as.factor(df$randCESD2006))
summary(as.factor(df$randCESD2008))
summary(as.factor(df$randCESD2010))
summary(as.factor(df$randCESD2012))
summary(as.factor(df$randCESD2014))
summary(as.factor(df$randCESD2016))
summary(as.factor(df$randCESD2018))
# don't need cleaning


#------------------------------------------------------------------------------
# Lifestyle factor variables


#-----------------------------------
# Smoking

# Current smoker
df <- df %>%
  mutate(randSmokeNow1998 = factor(randSmokeNow1998,
                                   levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeNow2000 = factor(randSmokeNow2000,
                                   levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeNow2002 = factor(randSmokeNow2002,
                                   levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeNow2004 = factor(randSmokeNow2004,
                                   levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeNow2006 = factor(randSmokeNow2006,
                                   levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeNow2008 = factor(randSmokeNow2008,
                                   levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeNow2010 = factor(randSmokeNow2010,
                                   levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeNow2012 = factor(randSmokeNow2012,
                                   levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeNow2014 = factor(randSmokeNow2014,
                                   levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeNow2016 = factor(randSmokeNow2016,
                                   levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeNow2018 = factor(randSmokeNow2018,
                                   levels = c(0,1), labels = c("no","yes")))

# ever smoked
df <- df %>%
  mutate(randSmokeEver1998 = factor(randSmokeEver1998,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeEver2000 = factor(randSmokeEver2000,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeEver2002 = factor(randSmokeEver2002,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeEver2004 = factor(randSmokeEver2004,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeEver2006 = factor(randSmokeEver2006,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeEver2008 = factor(randSmokeEver2008,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeEver2010 = factor(randSmokeEver2010,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeEver2012 = factor(randSmokeEver2012,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeEver2014 = factor(randSmokeEver2014,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeEver2016 = factor(randSmokeEver2016,
                                    levels = c(0,1), labels = c("no","yes"))) %>%
  mutate(randSmokeEver2018 = factor(randSmokeEver2018,
                                    levels = c(0,1), labels = c("no","yes")))

# create vector of labels for smoker status variable
smoke_status <- c("Never smoker", "Former smoker", "Current smoker")

# create smoker status variables
df$smokeStatus1998 <- NA
df$smokeStatus1998[which(df$randSmokeEver1998 == "no")] <- 0
df$smokeStatus1998[which(df$randSmokeEver1998 == "yes" &
                           df$randSmokeNow1998 == "no")] <- 1
df$smokeStatus1998[which(df$randSmokeEver1998 == "yes" &
                           df$randSmokeNow1998 == "yes")] <- 2
df$smokeStatus2000 <- NA
df$smokeStatus2000[which(df$randSmokeEver2000 == "no")] <- 0
df$smokeStatus2000[which(df$randSmokeEver2000 == "yes" &
                           df$randSmokeNow2000 == "no")] <- 1
df$smokeStatus2000[which(df$randSmokeEver2000 == "yes" &
                           df$randSmokeNow2000 == "yes")] <- 2
df$smokeStatus2002 <- NA
df$smokeStatus2002[which(df$randSmokeEver2002 == "no")] <- 0
df$smokeStatus2002[which(df$randSmokeEver2002 == "yes" &
                           df$randSmokeNow2002 == "no")] <- 1
df$smokeStatus2002[which(df$randSmokeEver2002 == "yes" &
                           df$randSmokeNow2002 == "yes")] <- 2
df$smokeStatus2004 <- NA
df$smokeStatus2004[which(df$randSmokeEver2004 == "no")] <- 0
df$smokeStatus2004[which(df$randSmokeEver2004 == "yes" &
                           df$randSmokeNow2004 == "no")] <- 1
df$smokeStatus2004[which(df$randSmokeEver2004 == "yes" &
                           df$randSmokeNow2004 == "yes")] <- 2
df$smokeStatus2006 <- NA
df$smokeStatus2006[which(df$randSmokeEver2006 == "no")] <- 0
df$smokeStatus2006[which(df$randSmokeEver2006 == "yes" &
                           df$randSmokeNow2006 == "no")] <- 1
df$smokeStatus2006[which(df$randSmokeEver2006 == "yes" &
                           df$randSmokeNow2006 == "yes")] <- 2
df$smokeStatus2008 <- NA
df$smokeStatus2008[which(df$randSmokeEver2008 == "no")] <- 0
df$smokeStatus2008[which(df$randSmokeEver2008 == "yes" &
                           df$randSmokeNow2008 == "no")] <- 1
df$smokeStatus2008[which(df$randSmokeEver2008 == "yes" &
                           df$randSmokeNow2008 == "yes")] <- 2
df$smokeStatus2010 <- NA
df$smokeStatus2010[which(df$randSmokeEver2010 == "no")] <- 0
df$smokeStatus2010[which(df$randSmokeEver2010 == "yes" &
                           df$randSmokeNow2010 == "no")] <- 1
df$smokeStatus2010[which(df$randSmokeEver2010 == "yes" &
                           df$randSmokeNow2010 == "yes")] <- 2
df$smokeStatus2012 <- NA
df$smokeStatus2012[which(df$randSmokeEver2012 == "no")] <- 0
df$smokeStatus2012[which(df$randSmokeEver2012 == "yes" &
                           df$randSmokeNow2012 == "no")] <- 1
df$smokeStatus2012[which(df$randSmokeEver2012 == "yes" &
                           df$randSmokeNow2012 == "yes")] <- 2
df$smokeStatus2014 <- NA
df$smokeStatus2014[which(df$randSmokeEver2014 == "no")] <- 0
df$smokeStatus2014[which(df$randSmokeEver2014 == "yes" &
                           df$randSmokeNow2014 == "no")] <- 1
df$smokeStatus2014[which(df$randSmokeEver2014 == "yes" &
                           df$randSmokeNow2014 == "yes")] <- 2
df$smokeStatus2016 <- NA
df$smokeStatus2016[which(df$randSmokeEver2016 == "no")] <- 0
df$smokeStatus2016[which(df$randSmokeEver2016 == "yes" &
                           df$randSmokeNow2016 == "no")] <- 1
df$smokeStatus2016[which(df$randSmokeEver2016 == "yes" &
                           df$randSmokeNow2016 == "yes")] <- 2
df$smokeStatus2018 <- NA
df$smokeStatus2018[which(df$randSmokeEver2018 == "no")] <- 0
df$smokeStatus2018[which(df$randSmokeEver2018 == "yes" &
                           df$randSmokeNow2018 == "no")] <- 1
df$smokeStatus2018[which(df$randSmokeEver2018 == "yes" &
                           df$randSmokeNow2018 == "yes")] <- 2

# recode to factor variables
df <- df %>%
  mutate(smokeStatus1998 = factor(smokeStatus1998, levels = c(0,1,2),
                                  labels = smoke_status)) %>%
  mutate(smokeStatus2000 = factor(smokeStatus2000, levels = c(0,1,2),
                                  labels = smoke_status)) %>%
  mutate(smokeStatus2002 = factor(smokeStatus2002, levels = c(0,1,2),
                                  labels = smoke_status)) %>%
  mutate(smokeStatus2004 = factor(smokeStatus2004, levels = c(0,1,2),
                                  labels = smoke_status)) %>%
  mutate(smokeStatus2006 = factor(smokeStatus2006, levels = c(0,1,2),
                                  labels = smoke_status)) %>%
  mutate(smokeStatus2008 = factor(smokeStatus2008, levels = c(0,1,2),
                                  labels = smoke_status)) %>%
  mutate(smokeStatus2010 = factor(smokeStatus2010, levels = c(0,1,2),
                                  labels = smoke_status)) %>%
  mutate(smokeStatus2012 = factor(smokeStatus2012, levels = c(0,1,2),
                                  labels = smoke_status)) %>%
  mutate(smokeStatus2014 = factor(smokeStatus2014, levels = c(0,1,2),
                                  labels = smoke_status)) %>%
  mutate(smokeStatus2016 = factor(smokeStatus2016, levels = c(0,1,2),
                                  labels = smoke_status)) %>%
  mutate(smokeStatus2018 = factor(smokeStatus2018, levels = c(0,1,2),
                                  labels = smoke_status))


#-----------------------------------
# Health insurance type

# goverment insurance plan variable is coded 0 = no, 1 = yes
# private health insurance variable is a count of how many private insurance
# plans the participant has - so a value >= 1 indicates private health
# insurance

df$insurance1998 <- NA
df$insurance1998[which(df$govHealthIns1998 == 0 & df$private1998 == 0)] <- 0 # no insurance
df$insurance1998[which(df$private1998 > 0)] <- 1 # some private insurance
df$insurance1998[which(df$govHealthIns1998 == 1 & df$private1998 == 0)] <- 2 # gov insurance only
df$insurance2000 <- NA
df$insurance2000[which(df$govHealthIns2000 == 0 & df$private2000 == 0)] <- 0 # no insurance
df$insurance2000[which(df$private2000 > 0)] <- 1 # some private insurance
df$insurance2000[which(df$govHealthIns2000 == 1 & df$private2000 == 0)] <- 2 # gov insurance only
df$insurance2002 <- NA
df$insurance2002[which(df$govHealthIns2002 == 0 & df$private2002 == 0)] <- 0 # no insurance
df$insurance2002[which(df$private2002 > 0)] <- 1 # some private insurance
df$insurance2002[which(df$govHealthIns2002 == 1 & df$private2002 == 0)] <- 2 # gov insurance only
df$insurance2004 <- NA
df$insurance2004[which(df$govHealthIns2004 == 0 & df$private2004 == 0)] <- 0 # no insurance
df$insurance2004[which(df$private2004 > 0)] <- 1 # some private insurance
df$insurance2004[which(df$govHealthIns2004 == 1 & df$private2004 == 0)] <- 2 # gov insurance only
df$insurance2006 <- NA
df$insurance2006[which(df$govHealthIns2006 == 0 & df$private2006 == 0)] <- 0 # no insurance
df$insurance2006[which(df$private2006 > 0)] <- 1 # some private insurance
df$insurance2006[which(df$govHealthIns2006 == 1 & df$private2006 == 0)] <- 2 # gov insurance only
df$insurance2008 <- NA
df$insurance2008[which(df$govHealthIns2008 == 0 & df$private2008 == 0)] <- 0 # no insurance
df$insurance2008[which(df$private2008 > 0)] <- 1 # some private insurance
df$insurance2008[which(df$govHealthIns2008 == 1 & df$private2008 == 0)] <- 2 # gov insurance only
df$insurance2010 <- NA
df$insurance2010[which(df$govHealthIns2010 == 0 & df$private2010 == 0)] <- 0 # no insurance
df$insurance2010[which(df$private2010 > 0)] <- 1 # some private insurance
df$insurance2010[which(df$govHealthIns2010 == 1 & df$private2010 == 0)] <- 2 # gov insurance only
df$insurance2012 <- NA
df$insurance2012[which(df$govHealthIns2012 == 0 & df$private2012 == 0)] <- 0 # no insurance
df$insurance2012[which(df$private2012 > 0)] <- 1 # some private insurance
df$insurance2012[which(df$govHealthIns2012 == 1 & df$private2012 == 0)] <- 2 # gov insurance only
df$insurance2014 <- NA
df$insurance2014[which(df$govHealthIns2014 == 0 & df$private2014 == 0)] <- 0 # no insurance
df$insurance2014[which(df$private2014 > 0)] <- 1 # some private insurance
df$insurance2014[which(df$govHealthIns2014 == 1 & df$private2014 == 0)] <- 2 # gov insurance only
df$insurance2016 <- NA
df$insurance2016[which(df$govHealthIns2016 == 0 & df$private2016 == 0)] <- 0 # no insurance
df$insurance2016[which(df$private2016 > 0)] <- 1 # some private insurance
df$insurance2016[which(df$govHealthIns2016 == 1 & df$private2016 == 0)] <- 2 # gov insurance only
df$insurance2018 <- NA
df$insurance2018[which(df$govHealthIns2018 == 0 & df$private2018 == 0)] <- 0 # no insurance
df$insurance2018[which(df$private2018 > 0)] <- 1 # some private insurance
df$insurance2018[which(df$govHealthIns2018 == 1 & df$private2018 == 0)] <- 2 # gov insurance only

# create vector of labels for insurance status variable
ins_status <- c("Uninsured", "Any private insurance", "Public insurance only")

# convert to factor variable
df <- df %>%
  mutate(insurance1998 = factor(insurance1998, levels = c(0,1,2),
                                labels = ins_status)) %>%
  mutate(insurance2000 = factor(insurance2000, levels = c(0,1,2),
                                labels = ins_status)) %>%
  mutate(insurance2002 = factor(insurance2002, levels = c(0,1,2),
                                labels = ins_status)) %>%
  mutate(insurance2004 = factor(insurance2004, levels = c(0,1,2),
                                labels = ins_status)) %>%
  mutate(insurance2006 = factor(insurance2006, levels = c(0,1,2),
                                labels = ins_status)) %>%
  mutate(insurance2008 = factor(insurance2008, levels = c(0,1,2),
                                labels = ins_status)) %>%
  mutate(insurance2010 = factor(insurance2010, levels = c(0,1,2),
                                labels = ins_status)) %>%
  mutate(insurance2012 = factor(insurance2012, levels = c(0,1,2),
                                labels = ins_status)) %>%
  mutate(insurance2014 = factor(insurance2014, levels = c(0,1,2),
                                labels = ins_status)) %>%
  mutate(insurance2016 = factor(insurance2016, levels = c(0,1,2),
                                labels = ins_status)) %>%
  mutate(insurance2018 = factor(insurance2018, levels = c(0,1,2),
                                labels = ins_status))


#------------------------------------------------------------------------------
# save cleaned dataset
write_csv(df, paste0(directory, "/HRS_data_clean.csv"))
