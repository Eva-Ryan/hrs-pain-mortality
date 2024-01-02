# R code to load HRS files, select the required variables from each file, and
# merge the longitudinal data into a single csv file.
#------------------------------------------------------------------------------

# load packages
library(tidyverse)
library(haven)

#------------------------------------------------------------------------------
# LOAD DATA FOR 1998 - 2018 (WAVESS 4 - 14), SELECT REQUIRED VARIABLES ONLY,
# AND MERGE ALL WAVES INTO ONE DATASET

# get location of project directory
directory <- getwd()

#-----------------------------------
# 1. Combined Longitudinal RAND file

# read in combined Longitudinal RAND file (1992 - 2020)
long_rand <- read_dta(paste0(directory, "/RAND data/randhrs1992_2020v1.dta"))

# prepare data
long_rand <- long_rand %>%
  # select just the relevant variables (id, income and wealth)
  select(hhid, pn, h4atotb, h5atotb, h6atotb, h7atotb, h8atotb, h9atotb,
         h10atotb, h11atotb, h12atotb, h13atotb, h14atotb, h4itot, h5itot,
         h6itot, h7itot, h8itot, h9itot, h10itot, h11itot, h12itot, h13itot,
         h14itot, r4bmi, r5bmi, r6bmi, r7bmi, r8bmi, r9bmi, r10bmi, r11bmi,
         r12bmi, r13bmi,	r14bmi, h4hhres, h5hhres, h6hhres, h7hhres, h8hhres,
         h9hhres, h10hhres, h11hhres, h12hhres, h13hhres, h14hhres, h4child,
         h5child, h6child, h7child, h8child, h9child, h10child, h11child,
         h12child, h13child, h14child, r4higov, r5higov, r6higov, r7higov,
         r8higov, r9higov, r10higov, r11higov, r12higov, r13higov, r14higov,
         r4prpcnt, r5prpcnt, r6prpcnt, r7prpcnt, r8prpcnt, r9prpcnt, r10prpcnt,
         r11prpcnt, r12prpcnt, r13prpcnt, r14prpcnt, r1jlocc, r2jlocc, r3jlocc,
         r4jlocc, r5jlocc, r6jlocc, r7jlocc, r8jlocc, r9jlocc, r10jlocc,
         r11jlocc, r12jlocc, r13jlocc, r14jlocc, r1jjobs, r2jjobs, r3jjobs,
         r4jjobs, r5jjobs, r6jjobs, r7jjobs, r8jjobs, r9jjobs, r10jjobs,
         r11jjobs, r12jjobs, r13jjobs, r14jjobs, ravetrn,
         r4cesd, r5cesd, r6cesd, r7cesd, r8cesd, r9cesd,
         r10cesd, r11cesd, r12cesd, r13cesd, r14cesd, r4jcocc, r5jcocc, r6jcocc,
         r7jcocc, r8jcocc, r9jcocc, r10jcocc, r11jcocc, r12jcocc, r13jcocc,
         r14jcocc, r4smokev, r5smokev, r6smokev, r7smokev, r8smokev, r9smokev,
         r10smokev, r11smokev, r12smokev, r13smokev, r14smokev, r4smoken,
         r5smoken, r6smoken, r7smoken, r8smoken, r9smoken, r10smoken, r11smoken,
         r12smoken, r13smoken, r14smoken
  ) %>%
  # create unique id hhidpn and remove household and person numbers
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

# rename
names(long_rand) <- c("wealth1998", "wealth2000", "wealth2002", "wealth2004",
                      "wealth2006", "wealth2008", "wealth2010", "wealth2012",
                      "wealth2014", "wealth2016", "hhInc2018",
                      "hhInc1998", "hhInc2000", "hhInc2002", "hhInc2004",
                      "hhInc2006", "hhInc2008", "hhInc2010", "hhInc2012",
                      "hhInc2014", "hhInc2016", "wealth2018",
                      "randBMI1998", "randBMI2000",
                      "randBMI2002", "randBMI2004", "randBMI2006", "randBMI2008",
                      "randBMI2010", "randBMI2012", "randBMI2014", "randBMI2016",
                      "randBMI2018", "householdSize1998", "householdSize2000",
                      "householdSize2002", "householdSize2004",
                      "householdSize2006", "householdSize2008",
                      "householdSize2010", "householdSize2012",
                      "householdSize2014", "householdSize2016",
                      "householdSize2018", "numChildren1998",
                      "numChildren2000", "numChildren2002", "numChildren2004",
                      "numChildren2006", "numChildren2008", "numChildren2010",
                      "numChildren2012", "numChildren2014", "numChildren2016",
                      "numChildren2018", "govHealthIns1998", "govHealthIns2000",
                      "govHealthIns2002", "govHealthIns2004", "govHealthIns2006",
                      "govHealthIns2008", "govHealthIns2010", "govHealthIns2012",
                      "govHealthIns2014", "govHealthIns2016", "govHealthIns2018",
                      "private1998", "private2000", "private2002", "private2004",
                      "private2006", "private2008", "private2010", "private2012",
                      "private2014", "private2016", "private2018",
                      "longestJob1992", "longestJob1994", "longestJob1996",
                      "longestJob1998", "longestJob2000", "longestJob2002",
                      "longestJob2004", "longestJob2006", "longestJob2008",
                      "longestJob2010", "longestJob2012", "longestJob2014",
                      "longestJob2016", "longestJob2018", "jobHistory1992",
                      "jobHistory1994", "jobHistory1996", "jobHistory1998",
                      "jobHistory2000", "jobHistory2002", "jobHistory2004",
                      "jobHistory2006", "jobHistory2008", "jobHistory2010",
                      "jobHistory2012", "jobHistory2014", "jobHistory2016",
                      "jobHistory2018", "veteranStatus", "randCESD1998",
                      "randCESD2000", "randCESD2002", "randCESD2004",
                      "randCESD2006", "randCESD2008", "randCESD2010",
                      "randCESD2012", "randCESD2014", "randCESD2016",
                      "randCESD2018", "randJobStatus1998", "randJobStatus2000",
                      "randJobStatus2002", "randJobStatus2004",
                      "randJobStatus2006", "randJobStatus2008",
                      "randJobStatus2010", "randJobStatus2012",
                      "randJobStatus2014", "randJobStatus2016",
                      "randJobStatus2018", "randSmokeEver1998",
                      "randSmokeEver2000", "randSmokeEver2002",
                      "randSmokeEver2004", "randSmokeEver2006",
                      "randSmokeEver2008", "randSmokeEver2010",
                      "randSmokeEver2012", "randSmokeEver2014",
                      "randSmokeEver2016", "randSmokeEver2018",
                      "randSmokeNow1998", "randSmokeNow2000", "randSmokeNow2002",
                      "randSmokeNow2004", "randSmokeNow2006", "randSmokeNow2008",
                      "randSmokeNow2010", "randSmokeNow2012", "randSmokeNow2014",
                      "randSmokeNow2016", "randSmokeNow2018",
                      "hhidpn")

#---------------------------------
# 2. Tracker file

# read in the HRS Tracker file
tracker <- read_dta(paste0(directory, "/Tracker file/trk2020tr_r.dta"))

# prepare data
tracker <- tracker %>%
  # select just the relevant variables (id, main status, sociodemographics,
  # first interview and death month/year)
  select(HHID, PN, FRESCODE, GRESCODE, HRESCODE, JRESCODE, KRESCODE,
         LRESCODE, MRESCODE, NRESCODE, ORESCODE, PRESCODE, QRESCODE,
         FWGTR, GWGTR, HWGTR, JWGTR, KWGTR, LWGTR, MWGTR, NWGTR, OWGTR,
         PWGTR, QWGTR, FALIVE, GALIVE, HALIVE, JALIVE, KALIVE, LALIVE, MALIVE,
         NALIVE, OALIVE, PALIVE, QALIVE, GENDER, BIRTHYR, HISPANIC, RACE,
         DEGREE, EXDEATHMO, EXDEATHYR, FIRSTIW, FWHY0WGT) %>%
  # create unique id hhidpn and remove household and person numbers
  mutate(hhidpn = paste0(HHID, PN)) %>%
  select(-HHID, -PN)

# rename
names(tracker) <- c("frescode", "grescode", "hrescode", "jrescode", "krescode",
                    "lrescode", "mrescode", "nrescode", "orescode", "prescode",
                    "qrescode", "fwgtr", "gwgtr", "hwgtr", "jwgtr", "kwgtr",
                    "lwgtr", "mwgtr", "nwgtr", "owgtr", "pwgtr", "qwgtr",
                    "falive", "galive", "halive", "jalive", "kalive", "lalive",
                    "malive", "nalive", "oalive", "palive", "qalive", "gender",
                    "birthYear", "hispanic", "race", "degree", "monthOfDeath",
                    "yearOfDeath", "yrOf1stIntvw", "fwhy0wgt", "hhidpn")

#---------------------------------
# 3. Census region file

# read in the HRS Census region file
region <- read_dta(paste0(directory, "/Census region file/built/stata/HRSXREGION18.dta"))

# prepare data
region <- region %>%
  # select just the relevant variables
  select(region98, region00, region02, region04, region06, region08, region10,
         region12, region14, region16, region18, beale2003_98, beale2003_00,
         beale2003_02, beale2003_04, beale2003_06, beale2003_08, beale2003_10,
         beale2003_12, beale2003_14, beale2003_16, beale2003_18, hhid, pn
  ) %>%
  # create unique id hhidpn and remove household and person numbers
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

# rename for consistency with Hanna's code
names(region) <- c("region1998", "region2000", "region2002", "region2004",
                   "region2006", "region2008", "region2010", "region2012",
                   "region2014", "region2016", "region2018", "urbanicity1998",
                   "urbanicity2000", "urbanicity2002", "urbanicity2004",
                   "urbanicity2006", "urbanicity2008", "urbanicity2010",
                   "urbanicity2012", "urbanicity2014", "urbanicity2016",
                   "urbanicity2018", "hhidpn")

#---------------------------------
# 4. Individual wave RAND fatfiles

# create function to read in the individual wave RAND fatfiles and reduce to just
# relevant variables
rand_fun <- function(file_path_name, alphabet, wave){
  # Inputs: file_path_name = path to file to RAND file to be loaded in
  #         alphabet = letter that the pain variable names start with
  #         wave = wave year (numeric)
  # Output: df = reduced dataframe containing just ID and pain variables for the
  #              wave, with wave specific suffixes

  # read in RAND data
  df_full <- read_dta(file_path_name)

  # create vector of descriptive variable names to apply to all waves
  var_names <- c("troubledWithPainP", "painUsualSeverity", "painPreventActivityP",
                 "cancerEverP", "cancerTrtmtLast2YrsP", "cancerSeenDocLast2YrsP",
                 "cancerBetterSameWorse", "cancerNewSinceLastWaveP",
                 "cancerYearMostRecent", "diabetesP", "lungDisP", "hrtCondP",
                 "anginaP", "strokeP", "arthritisP", "psychP", "sad2wksP",
                 "currentSmokeP", "everSmokeP", "daysDrinkWkP", "srh", "weight",
                 "htFeet", "htInches", "feltDepressed", "everythingEffort",
                 "restlessSleep", "feltHappy", "feltLonely", "enjoyedLife",
                 "feltSad", "getGoing", "maritalStatus", "jobStatus",
                 "foodSecurity", "debt")

  # select relevant variables only (unique ID and pain variables)

  # Wave 6-11 (with religious importance question)
  if(wave > 2002 & wave < 2014){

    # add measured weight/height and importance of religion to list of variable
    # names
    var_names_plus <- c(var_names, "measuredWgt", "measuredHgt",
                        "religionImportance")

    # prepare data
    df <- df_full %>%
      # select relevant variables (NOTE: in same order as names in var_names)
      select("hhid", "pn",
             paste0(alphabet, "c104"), paste0(alphabet, "c105"),
             paste0(alphabet, "c106"), paste0(alphabet, "c018"),
             paste0(alphabet, "c020"), paste0(alphabet, "c019"),
             paste0(alphabet, "c023"), paste0(alphabet, "c024"),
             paste0(alphabet, "c028"), paste0(alphabet, "c010"),
             paste0(alphabet, "c030"), paste0(alphabet, "c036"),
             paste0(alphabet, "c045"), paste0(alphabet, "c053"),
             paste0(alphabet, "c070"), paste0(alphabet, "c065"),
             paste0(alphabet, "c150"), paste0(alphabet, "c117"),
             paste0(alphabet, "c116"),
             paste0(alphabet, "c129"), paste0(alphabet, "c001"),
             paste0(alphabet, "c139"), paste0(alphabet, "c141"),
             paste0(alphabet, "c142"), paste0(alphabet, "d110"),
             paste0(alphabet, "d111"), paste0(alphabet, "d112"),
             paste0(alphabet, "d113"), paste0(alphabet, "d114"),
             paste0(alphabet, "d115"), paste0(alphabet, "d116"),
             paste0(alphabet, "d117"), paste0(alphabet, "b063"),
             paste0(alphabet, "j005m1"), paste0(alphabet, "q415"),
             paste0(alphabet, "q477"),
             paste0(alphabet, "i841"), paste0(alphabet, "i834"),
             paste0(alphabet, "b053")) %>%
      # create unique id hhidpn and remove household and person numbers
      mutate(hhidpn = paste0(hhid, pn)) %>%
      select(-hhid, -pn)

    # make variable names more descriptive and add wave year (except for id)
    names(df)[-ncol(df)] <- paste0(var_names_plus, wave)

  }else{
    if(wave >= 2014){

      # add measured weight and height to list of variable names
      var_names_plus <- c(var_names, "measuredWgt", "measuredHgt")

      # prepare data
      df <- df_full %>%
        # select relevant variables (NOTE: in same order as names in var_names)
        select("hhid", "pn",
               paste0(alphabet, "c104"), paste0(alphabet, "c105"),
               paste0(alphabet, "c106"), paste0(alphabet, "c018"),
               paste0(alphabet, "c020"), paste0(alphabet, "c019"),
               paste0(alphabet, "c023"), paste0(alphabet, "c024"),
               paste0(alphabet, "c028"), paste0(alphabet, "c010"),
               paste0(alphabet, "c030"), paste0(alphabet, "c036"),
               paste0(alphabet, "c045"), paste0(alphabet, "c053"),
               paste0(alphabet, "c070"), paste0(alphabet, "c065"),
               paste0(alphabet, "c150"), paste0(alphabet, "c117"),
               paste0(alphabet, "c116"),
               paste0(alphabet, "c129"), paste0(alphabet, "c001"),
               paste0(alphabet, "c139"), paste0(alphabet, "c141"),
               paste0(alphabet, "c142"), paste0(alphabet, "d110"),
               paste0(alphabet, "d111"), paste0(alphabet, "d112"),
               paste0(alphabet, "d113"), paste0(alphabet, "d114"),
               paste0(alphabet, "d115"), paste0(alphabet, "d116"),
               paste0(alphabet, "d117"), paste0(alphabet, "b063"),
               paste0(alphabet, "j005m1"), paste0(alphabet, "q415"),
               paste0(alphabet, "q477"),
               paste0(alphabet, "i841"), paste0(alphabet, "i834")) %>%
        # create unique id hhidpn and remove household and person numbers
        mutate(hhidpn = paste0(hhid, pn)) %>%
        select(-hhid, -pn)

      # make variable names more descriptive and add wave year (except for id)
      names(df)[-ncol(df)] <- paste0(var_names_plus, wave)

    }else{
      if(wave == 2002){

        # add importance of religion to list of variable names
        var_names_plus <- c(var_names, "religionImportance")

        # prepare data
        df <- df_full %>%
          # select relevant variables (NOTE: in same order as names in var_names)
          select("hhid", "pn",
                 paste0(alphabet, "c104"), paste0(alphabet, "c105"),
                 paste0(alphabet, "c106"), paste0(alphabet, "c018"),
                 paste0(alphabet, "c020"), paste0(alphabet, "c019"),
                 paste0(alphabet, "c023"), paste0(alphabet, "c024"),
                 paste0(alphabet, "c028"), paste0(alphabet, "c010"),
                 paste0(alphabet, "c030"), paste0(alphabet, "c036"),
                 paste0(alphabet, "c045"), paste0(alphabet, "c053"),
                 paste0(alphabet, "c070"), paste0(alphabet, "c065"),
                 paste0(alphabet, "c150"), paste0(alphabet, "c117"),
                 paste0(alphabet, "c116"),
                 paste0(alphabet, "c129"), paste0(alphabet, "c001"),
                 paste0(alphabet, "c139"), paste0(alphabet, "c141"),
                 paste0(alphabet, "c142"), paste0(alphabet, "d110"),
                 paste0(alphabet, "d111"), paste0(alphabet, "d112"),
                 paste0(alphabet, "d113"), paste0(alphabet, "d114"),
                 paste0(alphabet, "d115"), paste0(alphabet, "d116"),
                 paste0(alphabet, "d117"), "hmarital",
                 paste0(alphabet, "j005m1"), paste0(alphabet, "q415"),
                 paste0(alphabet, "q477"), paste0(alphabet, "b053")
          ) %>%
          # create unique id hhidpn and remove household and person numbers
          mutate(hhidpn = paste0(hhid, pn)) %>%
          select(-hhid, -pn)

        # make variable names more descriptive and add wave year (except for id)
        names(df)[-ncol(df)] <- paste0(var_names_plus, wave)

      }else{ # Wave 5
        if(wave == 2000){

          # add importance of religion to list of variable names
          var_names_plus <- c(var_names, "religionImportance")

          # prepare data
          df <- df_full %>%
            # select relevant variables (NOTE: in same order as names in var_names)
            select("hhid", "pn", "g1372", "g1374", "g1375", "g1262", "g1264",
                   "g1263", "g1266", "g1267", "g1274", "g1245", "g1279",
                   "g1289", "g1301", "g1309", "g1327", "g1322", "g1456",
                   "g1400", "g1399", "g1416", "g1226", "g1425", "g1428", "g1429",
                   "g1669", "g1670", "g1671", "g1672", "g1673", "g1674", "g1675",
                   "g1676", "g1158", "g3365m1", "g5747", "g5846", "g1142") %>%
            # create unique id hhidpn and remove household and person numbers
            mutate(hhidpn = paste0(hhid, pn)) %>%
            select(-hhid, -pn)

          # make variable names more descriptive and add wave year (except for id)
          names(df)[-ncol(df)] <- paste0(var_names_plus, wave)

        }else{ # Wave 4
          if(wave == 1998){

            # add importance of religion to list of variable names
            var_names_plus <- c(var_names, "religionImportance")

            # prepare data
            df <- df_full %>%
              # select relevant variables (NOTE: in same order as names in var_names)
              select("hhid", "pn", "f1239", "f1241", "f1242", "f1129", "f1131",
                     "f1130", "f1133", "f1134", "f1141", "f1116", "f1146",
                     "f1156", "f1168", "f1176", "f1194", "f1189", "f1323", "f1267",
                     "f1266", "f1283", "f1097", "f1291", "f1295", "f1296",
                     "f1493", "f1494", "f1495", "f1496", "f1497", "f1498", "f1499",
                     "f1500", "f1071", "f3115m1", "f5360", "f5486", "f1055") %>%
              # create unique id hhidpn and remove household and person numbers
              mutate(hhidpn = paste0(hhid, pn)) %>%
              select(-hhid, -pn)

            # make variable names more descriptive and add wave year (except for id)
            names(df)[-ncol(df)] <- paste0(var_names_plus, wave)

          }
        }
      }
    }
  }

  # return reduced dataframe
  return(df)

}

# apply the function to load and reduce the 1998 - 2018 datasets (excluding
# 2006, as additional race variables are required so it is processed separately)

# 2018 - Wave 14
w2018 <- rand_fun(paste0(directory, "/RAND data/h18f2b.dta"), "q", 2018)
# 2016 - Wave 13
w2016 <- rand_fun(paste0(directory, "/RAND data/h16f2c.dta"), "p", 2016)
# 2014 - Wave 12
w2014 <- rand_fun(paste0(directory, "/RAND data/h14f2b.dta"), "o", 2014)
# 2012 - Wave 11
w2012 <- rand_fun(paste0(directory, "/RAND data/h12f3a.dta"), "n", 2012)
# 2010 - Wave 10
w2010 <- rand_fun(paste0(directory, "/RAND data/hd10f6a.dta"), "m", 2010)
# 2008 - Wave 9
w2008 <- rand_fun(paste0(directory, "/RAND data/h08f3a.dta"), "l", 2008)
# 2006 - Wave 8 (see below)
# 2004 - Wave 7
w2004 <- rand_fun(paste0(directory, "/RAND data/h04f1c.dta"), "j", 2004)
# 2002 - Wave 6
w2002 <- rand_fun(paste0(directory, "/RAND data/h02f2c.dta"), "h", 2002)
# 2000 - Wave 5
w2000 <- rand_fun(paste0(directory, "/RAND data/h00f1d.dta"), NA, 2000)
# 1998 - Wave 4
w1998 <- rand_fun(paste0(directory, "/RAND data/hd98f2c.dta"), NA, 1998)

# load 2006 fatfile and reduce to just required variables, including the added
# race variables

# read in RAND data
w2006 <- read_dta(paste0(directory, "/RAND data/h06f4a.dta"))

# create vector of descriptive variable names to apply to all waves
var_names <- c("troubledWithPainP", "painUsualSeverity", "painPreventActivityP",
               "cancerEverP", "cancerTrtmtLast2YrsP", "cancerSeenDocLast2YrsP",
               "cancerBetterSameWorse", "cancerNewSinceLastWaveP",
               "cancerYearMostRecent", "diabetesP", "lungDisP", "hrtCondP",
               "anginaP", "strokeP", "arthritisP", "psychP", "sad2wksP",
               "currentSmokeP", "everSmokeP", "daysDrinkWkP", "srh", "kb091m",
               "kb028", "weight", "htFeet", "htInches", "feltDepressed",
               "everythingEffort", "restlessSleep", "feltHappy", "feltLonely",
               "enjoyedLife", "feltSad", "getGoing", "maritalStatus", "jobStatus",
               "foodSecurity", "debt", "measuredWgt", "measuredHgt",
               "religionImportance")

# prepare data
alphabet <- "k"
w2006 <- w2006 %>%
  # select relevant variables (NOTE: in same order as names in var_names)
  select("hhid", "pn",
         paste0(alphabet, "c104"), paste0(alphabet, "c105"),
         paste0(alphabet, "c106"), paste0(alphabet, "c018"),
         paste0(alphabet, "c020"), paste0(alphabet, "c019"),
         paste0(alphabet, "c023"), paste0(alphabet, "c024"),
         paste0(alphabet, "c028"), paste0(alphabet, "c010"),
         paste0(alphabet, "c030"), paste0(alphabet, "c036"),
         paste0(alphabet, "c045"), paste0(alphabet, "c053"),
         paste0(alphabet, "c070"), paste0(alphabet, "c065"),
         paste0(alphabet, "c150"), paste0(alphabet, "c117"),
         paste0(alphabet, "c116"),
         paste0(alphabet, "c129"), paste0(alphabet, "c001"),
         "kb091m", "kb028",
         paste0(alphabet, "c139"), paste0(alphabet, "c141"),
         paste0(alphabet, "c142"),  paste0(alphabet, "d110"),
         paste0(alphabet, "d111"), paste0(alphabet, "d112"),
         paste0(alphabet, "d113"), paste0(alphabet, "d114"),
         paste0(alphabet, "d115"), paste0(alphabet, "d116"),
         paste0(alphabet, "d117"), paste0(alphabet, "b063"),
         paste0(alphabet, "j005m1"), paste0(alphabet, "q415"),
         paste0(alphabet, "q477"),
         paste0(alphabet, "i841"), paste0(alphabet, "i834"),
         paste0(alphabet, "b053")) %>%
  # create unique id hhidpn and remove household and person numbers
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

# make variable names more descriptive and add wave year (except for id)
names(w2006)[-ncol(w2006)] <- paste0(var_names, 2006)

# load in 1992-1996 waves solely to get height and weight data
w1996 <- read_dta(paste0(directory, "/RAND data/h96f4a.dta")) %>%
  select(hhid, pn, e954, e958, e959) %>%
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

w1995 <- read_dta(paste0(directory, "/RAND data/ad95f2b.dta")) %>%
  select(hhid, pn, d954, d958, d959) %>%
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

w1994 <- read_dta(paste0(directory, "/RAND data/h94f1a.dta")) %>%
  select(hhid, pn, w462, w463, w464) %>%
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

w1993 <- read_dta(paste0(directory, "/RAND data/ad93f2a.dta")) %>%
  select(hhid, pn, b304, b306) %>% # height only measured in inches
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

w1992 <- read_dta(paste0(directory, "/RAND data/hd92f1b.dta")) %>%
  select(hhid, pn, v515, v517, v518) %>%
  mutate(hhidpn = paste0(hhid, pn)) %>%
  select(-hhid, -pn)

# align with naming convention
var_names <- c("weight", "htFeet", "htInches")
names(w1996)[-ncol(w1996)] <- paste0(var_names, 1996)
names(w1995)[-ncol(w1995)] <- paste0(var_names, 1995)
names(w1994)[-ncol(w1994)] <- paste0(var_names, 1994)
names(w1993)[-ncol(w1993)] <- c("weight1993", "htInches1993")
names(w1992)[-ncol(w1992)] <- paste0(var_names, 1992)

#---------------------------------
# Merge all datasets together

# add pre-1998 height and weight to 1998 dataset
w1998 <- left_join(w1998,
                   w1996,
                   by = "hhidpn")
w1998 <- left_join(w1998,
                   w1995,
                   by = "hhidpn")
w1998 <- left_join(w1998,
                   w1994,
                   by = "hhidpn")
w1998 <- left_join(w1998,
                   w1993,
                   by = "hhidpn")
w1998 <- left_join(w1998,
                   w1992,
                   by = "hhidpn")

# left join (i.e., closed cohort, take 1998 as first year and only include
# those who participated in the 1998 wave)

# merge 1998 and 2000
df1 <- left_join(w1998,
                 w2000,
                 by = "hhidpn")
# add 2002
df2 <- left_join(df1,
                 w2002,
                 by = "hhidpn")
rm(df1)
# add 2004
df3 <- left_join(df2,
                 w2004,
                 by = "hhidpn")
rm(df2)
# add 2006
df4 <- left_join(df3,
                 w2006,
                 by = "hhidpn")
rm(df3)
# add 2008
df5 <- left_join(df4,
                 w2008,
                 by = "hhidpn")
rm(df4)
# add 2010
df6 <- left_join(df5,
                 w2010,
                 by = "hhidpn")
rm(df5)
# add 2012
df7 <- left_join(df6,
                 w2012,
                 by = "hhidpn")
rm(df6)
# add 2014
df8 <- left_join(df7,
                 w2014,
                 by = "hhidpn")
rm(df7)
# add 2016
df9 <- left_join(df8,
                 w2016,
                 by = "hhidpn")
rm(df8)
# add 2018
df10 <- left_join(df9,
                  w2018,
                  by = "hhidpn")
rm(df9)
# add variables from tracker file
df11 <- left_join(df10,
                  tracker,
                  by = "hhidpn")
rm(df10)
# add variables from census region file
df12 <- left_join(df11,
                  region,
                  by = "hhidpn")
rm(df11)
# add variables from Longitudinal RAND file to create final merged dataset with
# all required variables included
df_left <- left_join(df12,
                     long_rand,
                     by = "hhidpn")
rm(df12)

#----------------------------------------------------------------------------
# Save datasets

write_csv(df_left, paste0(directory, "/HRS_1998_to_2018.csv"))
