# I-690, EXPO CENTER ANALYSIS & PROJECT COMPARISONS

## RStudio Version: 1.1.456
## R Version: 3.5.1
## Windows 10

## Script Version: 1.0
## Updated: 2019-01-22


# CLEAR WORKSPACE; INSTALL/LOAD PACKAGES

rm(list = ls())

if(!require(zoo)){install.packages("zoo")}
if(!require(readr)){install.packages("readr")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(scales)){install.packages("scales")}
if(!require(readxl)){install.packages("readxl")}
if(!require(stringr)){install.packages("stringr")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(zipcode)){install.packages("zipcode")}

library(zoo)
library(readr)
library(tidyr)
library(dplyr)
library(scales)
library(readxl)
library(stringr)
library(lubridate)
library(zipcode)


# SET WORKING DIRECTORY; RETRIEVE DATA

setwd("~/Projects/REIS/Master")

urls <- c("https://raw.githubusercontent.com/jamisoncrawford/reis/master/Datasets/690_workforce_summary.csv",
          "https://raw.githubusercontent.com/jamisoncrawford/reis/master/Datasets/690_util_tidy.csv",
          "https://raw.githubusercontent.com/jamisoncrawford/reis/master/Datasets/expo_tidy.csv",
          "https://raw.githubusercontent.com/jamisoncrawford/reis/master/Datasets/tblr_master.csv")
names <- c("wsum", "util", "expo", "lvhc")

do.call("read_csv", mget(names[1]))

for (i in 1:length(names)){
  assign(names[i], read_csv(urls[i]))
}

rm(i, names, urls)


# AGGREGATE DATASETS TO I-690 WORKER SUMMARY LEVEL: "GROSS" & "HOURS"

wrk_sex_690 <- wsum %>%
  select(project:name, sex:race) %>%
  group_by(sex) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count, na.rm = TRUE),
         percent = count / total,
         project = "I-690")                             # Expo sex

wrk_race_690 <- wsum %>%
  select(project:name, sex:race) %>%
  group_by(race) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count, na.rm = TRUE),
         percent = count / total,
         project = "I-690")                             # Expo race

wrk_sxrc_690 <- wsum %>%
  select(project:name, sex:race) %>%
  group_by(sex, race) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count, na.rm = TRUE),
         percent = count / total,
         project = "I-690")                             # Expo sx/rc



grs_race_690 <- wsum %>%                                # Gross by race
  group_by(race) %>%
  summarize(total = sum(gross),
            mean = mean(gross),
            median = median(gross),
            iqr = IQR(gross),
            variance = var(gross))

grs_sex_690 <- wsum %>%                                 # Gross by sex
  group_by(sex) %>%
  summarize(total = sum(gross),
            mean = mean(gross),
            median = median(gross),
            iqr = IQR(gross),
            variance = var(gross))

grs_rcsx_690 <- wsum %>%                                # Gross by race/sex
  group_by(sex, race) %>%
  summarize(total = sum(gross),
            mean = mean(gross),
            median = median(gross),
            iqr = IQR(gross),
            variance = var(gross))

hrs_race_690 <- wsum %>%                                # Hours by race
  group_by(race) %>%
  summarize(total = sum(hours),
            mean = mean(hours),
            median = median(hours))

hrs_sex_690 <- wsum %>%                                 # Hours by sex
  group_by(sex) %>%
  summarize(total = sum(gross),
            mean = mean(gross),
            median = median(gross),
            iqr = IQR(gross),
            variance = var(gross))

hrs_rcsx_690 <- wsum %>%                                # Hours by race/sex
  group_by(sex, race) %>%
  summarize(total = sum(gross),
            mean = mean(gross),
            median = median(gross),
            iqr = IQR(gross),
            variance = var(gross))


# AGGREGATE DATASETS TO I-690 WORKER UTILIZATION REPORT: "GROSS" & "HOURS"

hrs_sx_690 <- util %>%
  mutate(total = total_m + total_f) %>%
  gather(key = sex, value = total, total_m, total_f) %>%
  group_by(sex) %>%
  summarize(hours = sum(total)) %>%
  mutate(percent = hours / sum(hours))                  # Hours by sex

hrs_sx_690$sex[1:2] <- c("Female", "Male")

hrs_rc_690 <- util %>%
  group_by(race) %>%
  summarize(hours = sum(race_hours))                    # Hours by race

tot_hrs <- sum(util$total_m, util$total_f)
wht_hrs <- data_frame(race = "White", hours = tot_hrs - sum(hrs_rc_690$hours))

all_hours <- bind_rows(hrs_rc_690, wht_hrs)

rm(tot_hrs)


# IMPORT ALTERNATIVE DATA STRUCTURE: I-690, UNTIDIED

url <- "https://raw.githubusercontent.com/jamisoncrawford/reis/master/Datasets/690_utilization.csv"
util_untidy <- read_csv(url); rm(url)

hrs_sx_690 <- util_untidy %>%
  mutate(total_a = total_m + total_f) %>%
  select(trade, total_a, total_m:total_min_f) %>%
  gather(key = tot_sex, value = total_g, total_m, total_f) %>%
  group_by(tot_sex) %>%
  summarize(tot_hrs = sum(total_g)) %>%
  rename("sex" = tot_sex,
         "hours" = tot_hrs) %>%
  mutate(percent = hours / sum(hours))                           # Hours, proportion by sex

hrs_rc_690 <- util_untidy %>%
  mutate(total = total_m + total_f,
         percent_m = round(total_m / total, 3),
         percent_f = round(total_f / total, 3),
         fem_goal = fem_goal,
         fem_actual = fem_actual,
         goal_diff = fem_actual - fem_goal) %>%
  select(trade, total_m:total_f, total, 
         percent_m:percent_f, fem_goal:fem_actual,
         goal_diff)                                              # Hours by sex, trade, goals


hrs_rcsx_690_ut <- util_untidy %>%
  mutate(total = total_m + total_f) %>%
  gather(key = race_gen, value = hrs, black_m:native_f)

hrs_rcsx_690_ut$race <- NA

for (i in seq_along(hrs_rcsx_690_ut$race_gen)){
  if (str_detect(hrs_rcsx_690_ut$race_gen[i], "^black.*")){
    hrs_rcsx_690_ut$race[i] <- "Black"
  } else if (str_detect(hrs_rcsx_690_ut$race_gen[i], "^hispanic.*")){
    hrs_rcsx_690_ut$race[i] <- "Hispanic"
  } else if (str_detect(hrs_rcsx_690_ut$race_gen[i], "^asian.*")){
    hrs_rcsx_690_ut$race[i] <- "Asian"
  } else if (str_detect(hrs_rcsx_690_ut$race_gen[i], "^native.*")){
    hrs_rcsx_690_ut$race[i] <- "Native"
  }
}

hrs_rcsx_690_ut$sex <- NA

for (i in seq_along(hrs_rcsx_690_ut$sex)){
  if (str_detect(hrs_rcsx_690_ut$race_gen[i], "_f$")){
    hrs_rcsx_690_ut$sex[i] <- "Female"
  } else if (str_detect(hrs_rcsx_690_ut$race_gen[i], "_m$")){
    hrs_rcsx_690_ut$sex[i] <- "Male"
  }
}

hrs_rcsx_690_ut <- hrs_rcsx_690_ut %>%
  select(trade, total_m, total_f, total:sex) %>%
  group_by(trade, race, sex) %>%
  summarize(total_trade_hrs_m = total_m,
            total_trade_hrs_f = total_f,
            dem_hrs = as.numeric(dem_hrs),
            hrs_wht = total - hrs)

trades <- unique(hrs_rcsx_690_ut$trade)
genders <- c("Male", "Female")

wht_hrs <- data_frame("trade" = rep(trades, each = 2),
                      "race" = "White",
                      "sex" = rep(genders, times = length(trades)),
                      "dem_hours" = NA,
                      "hrs_wht" = NA)

rm(i, trades, genders)

hours <- bind_rows(hrs_rcsx_690_ut, wht_hrs) %>% 
  arrange(trade, race) %>%
  select(trade, race, sex, hrs, total_m, total_f)

index <- which(is.na(hours$hrs))
names <- unique(hours$trade)
tot_m <- unique(hours$total_m, na.rm = TRUE)

for (i in index){
  if (hours$sex[i] == "Male"){
    hours$hrs[i] <- hours$total_m[i - 1]
  } else if (hours$sex[i] == "Female"){
    hours$hrs[i] <- hours$total_f[i - 2]
  }
}

hours <- hours %>%
  select(trade:hrs)

hrs_rcsx_690_fnl <- hours %>%
  group_by(trade, race, sex) %>%
  summarize(tot_hrs = sum(hrs))

hrs_rcsx_690_fnl %>%
  group_by(trade) %>%
  summarize(hours = sum(tot_hrs))                              # Total hours by trade, sex, race

tot_rc_690 <- hrs_rcsx_690_fnl %>%
  group_by(race) %>%
  summarize(tot_hrs = sum(tot_hrs))                            # Total hours by race

tot_sxrc_690 <- hrs_rcsx_690_fnl %>%
  group_by(sex, race) %>%
  summarize(tot_hrs = sum(tot_hrs),
            percent = tot_hrs / sum(tot_sxrc_690$tot_hrs))     # Total hours by sex, race

hours2 <- hours

for (i in 1:nrow(hours2)){
  if(hours2$race[i] == "White" & hours2$sex[i] == "Male"){
    hours2$hrs[i] <- 0
  }
}

min_hrs <- hours2 %>% 
  filter(hrs > 0) %>%
  group_by(trade) %>%
  summarize(min_count = sum(hrs, na.rm = TRUE)) %>%
  ungroup()

for (i in 1:nrow(hours)){
  for (j in 1:nrow(min_hrs)){
    if (hours$race[i] == "White" & hours$sex[i] == "Male" & hours$trade[i] == min_hrs$trade[j]){
      hours$hrs[i] <- hours$hrs[i] - min_hrs$min_count[j]
    }
  }
}

hrs_sxrc_690 <- hours %>% filter(hrs > 0)                      # Total hours by trade, sex, race

tot_rc_690 <- hrs_sxrc_690 %>%
  group_by(race) %>%
  summarize(count = sum(hrs, na.rm = TRUE)) %>%
  ungroup()                                                    # Total hours by race

tot_sx_690 <- hrs_sxrc_690 %>%
  group_by(sex) %>%
  summarize(count = sum(hrs, na.rm = TRUE)) %>%
  ungroup()                                                    # Total hours by sex

tot_sxrc_690 <- hrs_sxrc_690 %>%
  group_by(sex, race) %>%
  summarize(count = sum(hrs, na.rm = TRUE)) %>%
  ungroup() 

workers <- util_untidy %>%
  select(trade, min_goal:total_min_f) %>%
  rename("mgoal" = min_goal,
         "mactual" = min_actual,
         "fgoal" = fem_goal,
         "factual" = fem_actual,
         "tot_m" = total_emp_m,
         "tot_f" = total_emp_f,
         "tot_mm" = total_min_m,
         "tot_mf" = total_min_f)
  
tot_workers <- workers %>% 
  gather(key = indicator, value = total, tot_m:tot_mf) %>%
  mutate(level = NA,
         sex = NA)

for (i in seq_along(tot_workers$total)){
  if (str_detect(tot_workers$indicator[i], "_m$|_f$")){
    tot_workers$level[i] <- "Total"
  } else if (str_detect(tot_workers$indicator[i], "_mf$|_mm$")){
    tot_workers$level[i] <- "Minority"
  } 
}

for (i in seq_along(tot_workers$total)){
  if (str_detect(tot_workers$indicator[i], "_m$|_mm$")){
    tot_workers$sex[i] <- "Male"
  } else if (str_detect(tot_workers$indicator[i], "_f$|_mf$")){
    tot_workers$sex[i] <- "Female"
  } 
}

emps <- sum(tot_workers$total, na.rm = TRUE)

tot_workers <- tot_workers %>% 
  select(-indicator) %>%
  group_by(trade, sex, level) %>%
  summarize(total = sum(total)) %>%
  as_data_frame() %>%                                           # Proportion of sex/race in trades
  mutate(percent = total / emps)
  

# AGGREGATION TO I-690 LEVELS: EXPO CENTER

tot_wage_expo <- sum(expo$wages, na.rm = TRUE)

expo_gross_sex <- expo %>%
  select(project:name, sex:race, wages) %>%
  group_by(sex) %>%
  summarize(count = sum(wages, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total gross by sex

expo_gross_rac <- expo %>%
  select(project:name, sex:race, wages) %>%
  group_by(race) %>%
  summarize(count = sum(wages, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total gross by race

expo_gross_sxrc <- expo %>%
  select(project:name, sex:race, wages) %>%
  group_by(sex, race) %>%
  summarize(count = sum(wages, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total gross by race, sex

expo_hours_sex <- expo %>%
  select(project:name, sex:race, hours) %>%
  group_by(sex) %>%
  summarize(count = sum(hours, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total hours by sex

expo_hours_rac <- expo %>%
  select(project:name, sex:race, hours) %>%
  group_by(race) %>%
  summarize(count = sum(hours, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total hours by race

expo_hours_sxrc <- expo %>%
  select(project:name, sex:race, hours) %>%
  group_by(sex, race) %>%
  summarize(count = sum(hours, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total hours by race, sex

employees <- sum(expo$employees)

expo_workers_sex <- expo %>%
  select(project:name, sex:race) %>%
  group_by(sex) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total employees by sex

expo_workers_rac <- expo %>%
  select(project:name, sex:race) %>%
  group_by(race) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total employees by race
  
expo_workers_rcsx <- expo %>%
  select(project:name, sex:race) %>%
  group_by(sex, race) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total employees by race, sex


# AGGREGATION TO I-690 LEVELS: LAKEVIEW

index <- which(lvhc$sex == "male")
lvhc$sex[index] <- "Male"
lvhc <- filter(lvhc, !is.na(project))

lvhc_gross_sex <- lvhc %>%
  filter(project == "Lakeview") %>%
  select(project:name, zip:ssn, sex:race, gross) %>%
  group_by(sex) %>%
  summarize(count = sum(gross, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total gross by sex

lvhc_gross_rac <- lvhc %>%
  filter(project == "Lakeview") %>%
  select(project:name, zip:ssn, sex:race, gross) %>%
  group_by(race) %>%
  summarize(count = sum(gross, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total gross by race

lvhc_gross_sxrc <- lvhc %>%
  filter(project == "Lakeview") %>%
  select(project:name, zip:ssn, sex:race, gross) %>%
  group_by(sex, race) %>%
  summarize(count = sum(gross, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total gross by race, sex

lvhc_hours_sex <- lvhc %>%
  filter(project == "Lakeview") %>%
  select(project:name, zip:ssn, sex:race, hours) %>%
  group_by(sex) %>%
  summarize(count = sum(hours, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total hours by sex

lvhc_hours_rac <- lvhc %>%
  filter(project == "Lakeview") %>%
  select(project:name, zip:ssn, sex:race, hours) %>%
  group_by(race) %>%
  summarize(count = sum(hours, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total hours by race

lvhc_hours_sxrc <- lvhc %>%
  filter(project == "Lakeview") %>%
  select(project:name, zip:ssn, sex:race, hours) %>%
  group_by(sex, race) %>%
  summarize(count = sum(hours, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total hours by race, sex

lvhc_workers_sex <- lvhc %>%
  filter(project == "Lakeview") %>%
  select(project:name, zip:ssn, sex:race) %>%
  unique() %>%
  group_by(sex) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total employees by sex

lvhc_workers_race <- lvhc %>%
  filter(project == "Lakeview") %>%
  select(project:name, zip:ssn, sex:race) %>%
  unique() %>%
  group_by(race) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total employees by race
                                 
lvhc_workers_sxrc <- lvhc %>%
  filter(project == "Lakeview") %>%
  select(project:name, zip:ssn, sex:race) %>%
  unique() %>%
  group_by(sex, race) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total employees by sex, race
  

# AGGREGATION TO I-690 LEVELS: HANCOCK

hc_gross_sex <- lvhc %>%
  filter(project == "Hancock") %>%
  select(project:name, zip:ssn, sex:race, gross) %>%
  group_by(sex) %>%
  summarize(count = sum(gross, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total gross by sex

hc_gross_rac <- lvhc %>%
  filter(project == "Hancock") %>%
  select(project:name, zip:ssn, sex:race, gross) %>%
  group_by(race) %>%
  summarize(count = sum(gross, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total gross by race

hc_gross_sxrc <- lvhc %>%
  filter(project == "Hancock") %>%
  select(project:name, zip:ssn, sex:race, gross) %>%
  group_by(sex, race) %>%
  summarize(count = sum(gross, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total gross by race, sex

tot_hours_hc <- sum(lvhc$hours, na.rm = TRUE)
index <- which(lvhc$sex == "male")
lvhc$sex[index] <- "Male"
lvhc <- filter(lvhc, !is.na(project))

hc_hours_sex <- lvhc %>%
  filter(project == "Hancock") %>%
  select(project:name, zip:ssn, sex:race, hours) %>%
  group_by(sex) %>%
  summarize(count = sum(hours, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total hours by sex

hc_hours_rac <- lvhc %>%
  filter(project == "Hancock") %>%
  select(project:name, zip:ssn, sex:race, hours) %>%
  group_by(race) %>%
  summarize(count = sum(hours, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total hours by race

hc_hours_sxrc <- lvhc %>%
  filter(project == "Hancock") %>%
  select(project:name, zip:ssn, sex:race, hours) %>%
  group_by(sex, race) %>%
  summarize(count = sum(hours, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total hours by race, sex

hc_workers_sex <- lvhc %>%
  filter(project == "Hancock") %>%
  select(project:name, zip:ssn, sex:race) %>%
  unique() %>%
  group_by(sex) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total employees by sex

hc_workers_race <- lvhc %>%
  filter(project == "Hancock") %>%
  select(project:name, zip:ssn, sex:race) %>%
  unique() %>%
  group_by(race) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total employees by race

hc_workers_sxrc <- lvhc %>%
  filter(project == "Hancock") %>%
  select(project:name, zip:ssn, sex:race) %>%
  unique() %>%
  group_by(sex, race) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)                                  # Total employees by sex, race


# AGGREGATE FINDINGS: TOTAL EMPLOYEES

hc <- hc_workers_race %>%
  select(race, count, total, perc) %>%
  rename("percent" = perc) %>%
  mutate(project = "Hancock")

lv <- lvhc_workers_race %>%
  mutate(project = "Lakeview") %>%
  rename("percent" = perc)

ex <- expo_workers_rac %>%
  select(race, count:perc) %>%
  mutate(project = "Expo Center") %>%
  rename("percent" = perc)

hw <- wrk_race_690 %>%
  mutate(total = sum(count, na.rm = TRUE),
         project = "I-690") %>%
  select(race, count, total, percent, project) 

names <- c(hc, lv, ex, hw)

all_races <- hc %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Race distribution by project

lv <- lvhc_workers_sex %>%
  rename(percent = perc) %>%
  mutate(project = "Lakeview")

hc <- hc_workers_sex %>%
  rename(percent = perc) %>%
  mutate(project = "Hancock") %>%
  select(sex, count, total, percent, project)

ex <- expo_workers_sex %>%
  rename(percent = perc) %>%
  mutate(project = "Expo Center")

hw <- wrk_sex_690

all_sexes <- hc %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Sex distribution by project

hw <- wrk_sxrc_690

hc <- hc_workers_sxrc %>%
  mutate(project = "Hancock") %>%
  rename(percent = perc)

ex <- expo_workers_rcsx %>%
  mutate(project = "Expo Center") %>%
  rename(percent = perc)

lv <- lvhc_workers_sxrc %>%
  rename(percent = perc) %>%
  mutate(project = "Lakeview")

all_sxrc <- hc %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Sex, race distribution by project


# AGGREGATE FINDINGS: HOURS DISTRIBUTION

hw <- tot_rc_690 %>%
  mutate(total = sum(count, na.rm = TRUE),
         percent = count / total,
         project = "I-690")

ex <- expo_hours_rac %>%
  rename(percent = perc) %>%
  mutate(total = sum(count),
         project = "Expo Center")

lv <- lvhc_hours_rac %>%
  rename(percent = perc) %>%
  mutate(project = "Lakeview")

hc <- hc_hours_rac %>%
  rename(percent = perc) %>%
  mutate(total = sum(count),
         project = "Hancock")

all_hours_race <- hc %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Hours by race

hw <- tot_sx_690 %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "I-690")

hc <- hc_hours_sex %>%
  rename(percent = perc) %>%
  mutate(project = "Hancock") %>%
  select(sex:count, total, percent:project)

lv <- lvhc_hours_sex %>%
  rename(percent = perc) %>%
  mutate(project = "Lakeview") %>%
  select(sex:count, total, percent:project)

ex <- expo_hours_sex %>%
  rename(percent = perc) %>%
  mutate(project = "Expo Center") %>%
  select(sex:count, total, percent:project)

all_hours_sex <- hc %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Hours by sex

options(scipen = 999)                                           # Disable sci. notation

hw <- as_data_frame(tot_sxrc_690) %>%
  mutate(total = sum(count, na.rm = TRUE),
         percent = count / total,
         project = "I-690")

ex <- as_data_frame(expo_hours_sxrc) %>%
  rename(percent = perc) %>%
  mutate(project = "Expo Center")

hc <- as_data_frame(hc_hours_sxrc) %>%
  rename(percent = perc) %>%
  mutate(project = "Hancock")

lv <- as_data_frame(lvhc_hours_sxrc) %>%
  rename(percent = perc) %>%
  mutate(project = "Lakeview")

all_hours_sxrc <- hc %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Hours by sex, race, project


# AGGREGATE FINDINGS: GROSS DISTRIBUTION

hw <- as_data_frame(grs_race_690) %>%
  rename(count = total) %>%
  select(race, count) %>%
  mutate(total = sum(count, na.rm = TRUE),
         percent = count / total,
         project = "I-690")

ex <- as_data_frame(expo_gross_rac) %>%
  rename(percent = perc) %>%
  mutate(project = "Expo Center")

hc <- as_data_frame(hc_gross_rac) %>%
  rename(percent = perc) %>%
  mutate(project = "Hancock")

lv <- as_data_frame(lvhc_gross_rac) %>%
  rename(percent = perc) %>%
  mutate(project = "Lakeview")

hw <- grs_race_690 %>%
  select(race:total) %>%
  rename(count = total) %>%
  mutate(total = sum(count, na.rm = TRUE),
         percent = count / total,
         project = "I-690")
  
ex <- expo_gross_rac %>%
  rename(percent = perc) %>%
  mutate(project = "Expo Center")

all_gross_race <- hc %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Gross by race, project

hw <- as_data_frame(grs_sex_690) %>%
  select(sex:total) %>%
  rename(count = total) %>%
  mutate(total = sum(count, na.rm = TRUE),
         percent = count / total,
         project = "I-690")

ex <- as_data_frame(expo_gross_sex) %>%
  rename(percent = perc) %>%
  mutate(project = "Expo Center")

hc <- as_data_frame(hc_gross_sex) %>%
  rename(percent = perc) %>%
  mutate(project = "Hancock")
  
lv <- as_data_frame(lvhc_gross_sex) %>%
  rename(percent = perc) %>%
  mutate(project = "Lakeview")

all_gross_sex <- hc %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Gross by sex, project

hw <- as_data_frame(grs_rcsx_690)
ex <- as_data_frame(expo_gross_sxrc)
hc <- as_data_frame(hc_gross_sxrc)
lv <- as_data_frame(lvhc_gross_sxrc)

hw <- hw %>%
  select(sex:total) %>%
  rename("count" = total) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "I-690") %>%
  select(sex, race, count, total, percent, project) %>%
  arrange(desc(percent))

ex <- ex %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "Expo Center") %>%
  select(sex, race, count, total, percent, project) %>%
  arrange(desc(percent))

hc <- hc %>%
  rename("percent" = perc) %>%
  mutate(project = "Hancock") %>%
  arrange(desc(percent))

lv <- lv %>%
  rename(percent = perc) %>%
  mutate(project = "Lakeview") %>%
  arrange(desc(percent))

all_gross_sxrc <- hc %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Gross by race, sex, project


# ADDITIONAL FINDINGS: EXPO CENTER

#expo_sex_class <- expo %>%
#  filter(!is.na(title),
#         !is.na(sex)) %>%
#  group_by(sex, title) %>%
#  summarize(count = n()) %>%
#  as_data_frame() %>%
#  mutate(total = sum(count),
#         percent = count / total,
#         name = "Expo Center")                                  # Class by sex, Expo

#expo_race_class <- expo %>%
#  filter(!is.na(title),
#         !is.na(race)) %>%
#  group_by(race, title) %>%
#  summarize(count = n()) %>%
#  as_data_frame() %>%
#  mutate(total = sum(count),
#         percent = count / total,
#         name = "Expo Center")                                  # Class by race, Expo
  
#expo_rcsx_class <- expo %>%
#  filter(!is.na(title),
#         !is.na(race),
#         !is.na(sex)) %>%
#  group_by(sex, race, title) %>%
#  summarize(count = n()) %>%
#  as_data_frame() %>%
#  mutate(total = sum(count),
#         percent = count / total,
#         name = "Expo Center")                                  # Class by sex and race, Expo


# WRITE TO CSV

names <- c("all_gross_race", "all_gross_sex",   "all_gross_sxrc", 
           "all_hours_race", "all_hours_sex",   "all_hours_sxrc", 
           "all_races",      "all_sexes",       "all_sxrc")

files <- paste0(names, ".csv"); rm(names)

setwd("~/Projects/REIS/Master")

write_csv(all_gross_race, files[1])
write_csv(all_gross_sex, files[2])
write_csv(all_gross_sxrc, files[3])

write_csv(all_hours_race, files[4])
write_csv(all_hours_sex, files[5])
write_csv(all_hours_sxrc, files[6])

write_csv(all_races, files[7])
write_csv(all_sexes, files[8])
write_csv(all_sxrc, files[9])


# AGGREGATION TO UNIQUE WORKER TOTALS: I-690, HANCOCK, LAKEVIEW

ex_tots <- wsum %>%
  select(project:name, ssn, hours, gross, sex, race)
  
ex_tots$ssn <- as.character(1:length(ex_tots$ssn))     # Unique IDs in lieu of SSN, I-690

hc_tots <- lvhc %>%
  filter(project == "Hancock")

index <- which(is.na(hc_tots$ssn))

hc_tots[index[c(1:9, 11)], "ssn"] <- "401"
hc_tots[index[c(10, 12)], "ssn"] <- "402"
hc_tots[index[c(13:21)], "ssn"] <- "403"

hc_tots <- hc_tots %>%
  select(project:name, zip:ssn, hours, gross, sex:race) %>%
  group_by(project, name, ssn, sex, race) %>%
  summarize(tot_hrs = sum(hours),
            tot_grs = sum(gross))                      # Unique totals, Hancock


url <- "https://raw.githubusercontent.com/jamisoncrawford/reis/master/Datasets/tblr_master.csv"
lv <- read_csv(url); rm(url)


lv_tots <- lv %>%
  filter(project == "Lakeview") %>%
  select(project:ssn, hours, gross, sex:race) %>%
  group_by(project, name, ssn, sex, race) %>%
  summarize(tot_hrs = sum(hours),
            tot_grs = sum(gross))                      # Unique totals, Lakeview

ex_tots <- ex_tots %>% select(project, name, ssn, sex, race, hours, gross)
hc_tots <- hc_tots %>% rename("hours" = tot_hrs, "gross" = tot_grs)
lv_tots <- lv_tots %>% rename("hours" = tot_hrs, "gross" = tot_grs)

all_tots <- ex_tots %>%
  bind_rows(hc_tots) %>%
  bind_rows(lv_tots)


# WRITE TO CSV

setwd("~/Projects/REIS/Master")

write_csv(all_tots, "all_tots.csv")


# HANCOCK & LAKEVIEW ANALYSIS


# RECORDS

url <- "https://raw.githubusercontent.com/jamisoncrawford/reis/master/Datasets/tblr_master.csv"
lvhc <- read_csv(url); rm(url)

index <- which(lvhc$sex == "male")
lvhc$sex[index] <- "Male"
lvhc <- filter(lvhc, !is.na(project))

lvhc_recs_uniq <- lvhc %>%
  select(project:name, zip:ssn, sex:race) %>%
  unique() %>%
  group_by(project) %>%
  summarize(unique = n(),
            race_recs = sum(!is.na(race)),
            race_perc = race_recs / unique,
            sex_recs = sum(!is.na(sex)),
            sex_perc = sex_recs / unique) %>%    # Note: Uniqueness determined by project, ssn, ignores zip
  ungroup() %>% 
  mutate(unique = number(unique, big.mark = ","),
         race_recs = number(race_recs, big.mark = ","),
         race_perc = percent(race_perc, accuracy = 0.01),
         sex_recs = number(sex_recs, big.mark = ","),
         sex_perc = percent(sex_perc, accuracy = 0.01)) %>%
  rename("Project" = project,
         "Total Workers" = unique,
         "Race Disclosed" = race_recs,
         "Race Disclosed (%)" = race_perc,
         "Gender Disclosed" = sex_recs,
         "Gender Disclosed (%)" = sex_perc)
  
  
lvhc_recs <- lvhc %>%
  group_by(project) %>%
  summarize(unique = n(),
            race_recs = sum(!is.na(race)),
            race_perc = race_recs / unique,
            sex_recs = sum(!is.na(sex)),
            sex_perc = sex_recs / unique,
            zip_recs = sum(!is.na(zip)),
            zip_perc = sex_recs / unique,
            grs_recs = sum(!is.na(gross)),
            grs_perc = grs_recs / unique) %>%     # Note: Revealed Records Breakdown
  ungroup() %>% 
  mutate(unique = number(unique, big.mark = ","),
         race_recs = number(race_recs, big.mark = ","),
         race_perc = percent(race_perc, accuracy = 0.01),
         sex_recs = number(sex_recs, big.mark = ","),
         sex_perc = percent(sex_perc, accuracy = 0.01),
         zip_recs = number(zip_recs, big.mark = ","),
         zip_perc = percent(zip_perc, accuracy = 0.01),
         grs_recs = number(grs_recs, big.mark = ","),
         grs_perc = percent(grs_perc, accuracy = 0.01)) %>%
  rename("Project" = project,
         "Total Records" = unique,
         "Race Disclosed" = race_recs,
         "Race Disclosed (%)" = race_perc,
         "Gender Disclosed" = sex_recs,
         "Gender Disclosed (%)" = sex_perc,
         "ZIP Disclosed" = zip_recs,
         "ZIP Disclosed (%)" = zip_perc,
         "Gross Disclosed" = grs_recs,
         "Gross Disclosed (%)" = grs_perc)
  
lvhc_rec_cons <- lvhc %>%
  group_by(project, name) %>%
  summarize(`Unique Records` = n()) %>%
  ungroup()

tmp_tots <- lvhc_rec_cons %>%
  group_by(project) %>%
  summarize(Total = sum(`Unique Records`)) %>%
  ungroup()

lvhc_rec_cons <- left_join(lvhc_rec_cons, 
                           tmp_tots, 
                           by = "project") %>%
  rename("Project" = project,
         "Contractor" = name) %>%
  mutate(Percent = `Unique Records` / Total) %>%
  arrange(Project, desc(Percent)) %>%
  mutate(`Unique Records` = number(`Unique Records`, big.mark = ","),
         Total = number (Total, big.mark = ","),
         Percent = percent(Percent, accuracy = 0.01)) %>%
  rename(`Project Total` = Total)

# Race & Gender by Contractor

con_race <- lvhc %>%
  select(project:name, zip:ssn, sex:race) %>%
  unique() %>%
  group_by(project, name, race) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  arrange(project, reorder(name, desc(total)), reorder(race, desc(total)))

proj_tots <- lvhc %>%
  select(project:name, zip:ssn, sex:race) %>%
  unique() %>%
  group_by(project) %>%
  summarize(proj_total = sum(n())) %>%
  ungroup()

name_tots <- lvhc %>%
  select(project:name, zip:ssn, sex:race) %>%
  unique() %>%
  group_by(project, name) %>%
  summarize(name_total = sum(n())) %>%
  ungroup()

con_race_viz <- con_race %>%
  left_join(name_tots, by = c("project", "name")) %>%
  left_join(proj_tots, by = "project") %>%
  mutate(name_perc = total / name_total,
         proj_perc = total / proj_total) %>%
  select(project:name_total, name_perc, proj_total, proj_perc)

con_race_tbl <- con_race_viz %>%
  mutate(name_perc = percent(name_perc, accuracy = 0.01),
         proj_perc = percent(proj_perc, accuracy = 0.01)) %>%
  rename("Project" = project,
         "Company" = name,
         "Race" = race,
         "Workers of Race" = total,
         "Total Company Workers" = name_total,
         "Company Workers (%)" = name_perc,
         "Total Project Workers" = proj_total,
         "Project Workers (%)" = proj_perc)                        # Workers by Company & Race

con_sex <- lvhc %>%
  select(project:name, zip:ssn, sex:race) %>%
  unique() %>%
  group_by(project, name, sex) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  arrange(project, reorder(name, desc(total)), reorder(sex, desc(total)))

con_sex_viz <- con_sex %>%
  left_join(name_tots, by = c("project", "name")) %>%
  left_join(proj_tots, by = "project") %>%
  mutate(name_perc = total / name_total,
         proj_perc = total / proj_total) %>%
  select(project:name_total, name_perc, proj_total, proj_perc)

con_sex_tbl <- con_sex_viz %>%
  mutate(name_perc = percent(name_perc, accuracy = 0.01),
         proj_perc = percent(proj_perc, accuracy = 0.01)) %>%
  rename("Project" = project,
         "Company" = name,
         "Gender" = sex,
         "Workers of Gender" = total,
         "Total Company Workers" = name_total,
         "Company Workers (%)" = name_perc,
         "Total Project Workers" = proj_total,
         "Project Workers (%)" = proj_perc)                        # Workers by Company & Sex

con_sxrc <- lvhc %>%
  filter(!is.na(project),
         !is.na(ssn)) %>%
  select(project:name, ssn, sex:race) %>%
  unique() %>%
  group_by(project, name, sex, race) %>%
  summarize(total = n()) %>%
  ungroup() %>%
  arrange(project, reorder(name, desc(total)), reorder(sex, desc(total)), reorder(race, desc(total)))

con_sxrc_viz <- con_sxrc %>%
  left_join(name_tots, by = c("project", "name")) %>%
  left_join(proj_tots, by = "project") %>%
  mutate(name_perc = total / name_total,
         proj_perc = total / proj_total) %>%
  select(project:name_total, name_perc, proj_total, proj_perc)

con_sxrc_tbl <- con_sxrc_viz %>%
  mutate(name_perc = percent(name_perc, accuracy = 0.01),
         proj_perc = percent(proj_perc, accuracy = 0.01)) %>%
  rename("Project" = project,
         "Company" = name,
         "Gender" = sex,
         "Race" = race,
         "Workers of Gender & Race" = total,
         "Total Company Workers" = name_total,
         "Company Workers (%)" = name_perc,
         "Total Project Workers" = proj_total,
         "Project Workers (%)" = proj_perc)                        # Workers by Company & Sex/Race

# Pay & Hours by Gender & Race

tot_hours <- lvhc %>%
  select(project, name, zip:ssn, sex, race, hours) %>%
  group_by(project, name, ssn, sex, race) %>%
  summarize(total = sum(hours)) %>%
  ungroup()

proj_tots <- lvhc %>%
  select(project:name, zip:ssn, sex:race, hours) %>%
  group_by(project) %>%
  summarize(proj_total = sum(hours, na.rm = TRUE)) %>%
  ungroup()

name_tots <- lvhc %>%
  select(project:name, zip:ssn, sex:race, hours) %>%
  group_by(project, name) %>%
  summarize(name_total = sum(n())) %>%
  ungroup()

tot_hrs_sx_viz <- tot_hours %>%
  group_by(project, sex) %>%
  summarize(tot_hrs = sum(total)) %>%
  arrange(project, desc(`tot_hrs`)) %>%
  ungroup() %>%
  left_join(proj_tots, by = "project") %>%
  mutate(proj_perc = tot_hrs / proj_total)

tot_hrs_sx_tbl <- tot_hrs_sx_viz %>%
  mutate(tot_hrs = number(tot_hrs, big.mark = ","),
         proj_total = number(proj_total, big.mark = ","),
         proj_perc = percent(proj_perc, accuracy = 0.01)) %>%
  rename("Project" = project,
         "Gender" = sex,
         "Hours" = tot_hrs, 
         "Project Hours" = proj_total,
         "Project Hours (%)" = proj_perc)                           # Hours by project and gender

tot_hrs_rc_viz <- tot_hours %>%
  group_by(project, race) %>%
  summarize(tot_hrs = sum(total)) %>%
  arrange(project, desc(`tot_hrs`)) %>%
  ungroup() %>%
  left_join(proj_tots, by = "project") %>%
  mutate(proj_perc = tot_hrs / proj_total)

tot_hrs_rc_tbl <- tot_hrs_rc_viz %>%
  mutate(tot_hrs = number(tot_hrs, big.mark = ","),
         proj_total = number(proj_total, big.mark = ","),
         proj_perc = percent(proj_perc, accuracy = 0.01)) %>%
  rename("Project" = project,
         "Race" = race,
         "Hours" = tot_hrs, 
         "Project Hours" = proj_total,
         "Project Hours (%)" = proj_perc)                           # Hours by project and gender

tot_hrs_sxrc_viz <- tot_hours %>%
  group_by(project, sex, race) %>%
  summarize(tot_hrs = sum(total)) %>%
  arrange(project, desc(`tot_hrs`)) %>%
  ungroup() %>%
  left_join(proj_tots, by = "project") %>%
  mutate(proj_perc = tot_hrs / proj_total)

tot_hrs_sxrc_tbl <- tot_hrs_sxrc_viz %>%
  mutate(tot_hrs = number(tot_hrs, big.mark = ","),
         proj_total = number(proj_total, big.mark = ","),
         proj_perc = percent(proj_perc, accuracy = 0.01)) %>%
  rename("Project" = project,
         "Gender" = sex,
         "Race" = race,
         "Hours" = tot_hrs, 
         "Project Hours" = proj_total,
         "Project Hours (%)" = proj_perc)                           # Hours by project and gender/race

# Worker Location

library(zipcode)
library(noncensus)

data("zip_codes")
data("counties")

lvhc_loc <- lvhc %>%
  select(project:name, zip, ssn, hours, gross, sex:race) %>%
  filter(!is.na(project),
         !is.na(ssn)) %>%
  mutate(zip = as.character(zip),
         zip = str_pad(zip, width = 5, side = "left", pad = "0")) %>%
  left_join(zip_codes, by = "zip") %>%
  mutate(state_fips = str_extract(fips, "^.{2}"),
         county_fips = str_extract(fips, ".{3}$")) %>%
  left_join(counties, by = c("state", "state_fips", "county_fips")) %>%
  select(project:state, county_name, latitude:fips)

lvhc_loc_tots <- lvhc_loc %>%
  select(project, name, ssn, zip, gross, hours, county_name, state) %>%
  filter(!is.na(project)) %>%
  group_by(project, name, zip, ssn, state, county_name) %>%
  summarize(tot_gross = sum(gross),
            tot_hours = sum(hours)) %>%
  ungroup()
  
county_tots <- lvhc_loc_tots %>%
  group_by(state, county_name) %>%
  summarize(workers = n(),
            tot_gross = sum(tot_gross, na.rm = TRUE),
            tot_hours = sum(tot_hours, na.rm = TRUE)) %>%
  arrange(reorder(county_name, desc(tot_gross))) %>%
  ungroup()

all_tots <- lvhc_loc_tots %>%
  mutate(all_gross = sum(tot_gross, na.rm = TRUE),
         all_hours = sum(tot_hours, na.rm = TRUE)) %>%
  select(county_name, all_gross, all_hours)

all_wrks <- lvhc_loc_tots %>%
  unique() %>%
  group_by(state, county_name) %>%
  summarize(tot_workers = n()) %>%
  select(county_name, tot_workers) %>%
  ungroup() %>%
  mutate(tot_workers = sum(tot_workers, na.rm = TRUE))

county_tots_viz <- county_tots %>%
  left_join(all_tots, by = c("county_name")) %>%
  left_join(all_wrks, by = c("county_name", "state")) %>%
  mutate(gross_perc = tot_gross / all_gross,
         hours_perc = tot_hours / all_hours,
         works_perc = workers / tot_workers) %>%
  unique()

county_tots_tbl <- county_tots_viz %>%
  select(state:workers, works_perc, tot_gross, gross_perc, tot_hours, hours_perc) %>%
  mutate(works_perc = percent(works_perc, accuracy = 0.01),
         hours_perc = percent(hours_perc, accuracy = 0.01),
         gross_perc = percent(gross_perc, accuracy = 0.01),
         tot_gross = dollar(tot_gross, big.mark = ","),
         tot_hours = number(tot_hours, big.mark = ",", accuracy = 0.1)) %>%
  rename("State" = state,
         "County" = county_name,
         "Workers" = workers,
         "Workforce (%)" = works_perc,
         "Hours" = tot_hours,
         "Total Hours (%)" = hours_perc, 
         "Gross" = tot_gross,
         "Total Gross (%)" = gross_perc)                               # Workers, hours, gross by county; LV & HC

proj_loc_tots <- lvhc_loc %>%
  select(project, name, ssn, zip, gross, hours, county_name, state) %>%
  filter(!is.na(project)) %>%
  group_by(project, name, zip, ssn, state, county_name) %>%
  summarize(tot_gross = sum(gross),
            tot_hours = sum(hours)) %>%
  ungroup()

proj_county_tots <- proj_loc_tots %>%
  group_by(project, state, county_name) %>%
  summarize(workers = n(),
            tot_gross = sum(tot_gross, na.rm = TRUE),
            tot_hours = sum(tot_hours, na.rm = TRUE)) %>%
  arrange(project, reorder(county_name, desc(workers))) %>%
  ungroup()

all_tots <- proj_county_tots %>%
  group_by(project) %>%
  summarize(all_gross = sum(tot_gross, na.rm = TRUE),
            all_hours = sum(tot_hours, na.rm = TRUE)) %>%
  select(project, all_gross, all_hours)

all_wrks <- proj_county_tots %>%
  unique() %>%
  group_by(project) %>%
  summarize(tot_workers = sum(workers)) %>%
  ungroup()

proj_county_tots_viz <- proj_county_tots %>%
  left_join(all_tots, by = c("project")) %>%
  left_join(all_wrks, by = "project") %>%
  mutate(gross_perc = tot_gross / all_gross,
         hours_perc = tot_hours / all_hours,
         works_perc = workers / tot_workers) %>%
  unique()

proj_county_tots_tbl <- proj_county_tots_viz %>%
  select(project, state:workers, works_perc, tot_gross, gross_perc, tot_hours, hours_perc) %>%
  mutate(works_perc = percent(works_perc, accuracy = 0.01),
         hours_perc = percent(hours_perc, accuracy = 0.01),
         gross_perc = percent(gross_perc, accuracy = 0.01),
         tot_gross = dollar(tot_gross, big.mark = ","),
         tot_hours = number(tot_hours, big.mark = ",", accuracy = 0.1)) %>%
  rename("Project" = project,
         "State" = state,
         "County" = county_name,
         "Workers" = workers,
         "Project Workforce (%)" = works_perc,
         "Hours" = tot_hours,
         "Project Hours (%)" = hours_perc, 
         "Gross" = tot_gross,
         "Project Gross (%)" = gross_perc)                               # Workers, hours, gross by county; LV & HC

# Workers & Race in Syracuse

if(!require(sf)){install.packages("sf")}
if(!require(tmap)){installe.packages("tmap")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(readr)){install.packages("readr")}
if(!require(tigris)){install.packages("tigris")}

library(sf)
library(tmap)
library(dplyr)
library(readr)
library(tigris)

options(scipen = 999)

url <- "https://raw.githubusercontent.com/jamisoncrawford/Syracuse-Crime-Analysis/master/Data/fips_geoid.csv"

fips <- read_csv(url)
sf_tracts <- tracts(state = "NY", county = "Onondaga", year = 2016, class = "sf")

sf_tracts <- sf_tracts %>%
  filter(GEOID %in% fips$geoid)

syr_zips <- zctas(starts_with = "132", year = 2016, state = "NY", class = "sf")

save(syr_zips, file = "syr_zips.rds")

load("syr_zips.rds")

syr_zips <- syr_zips %>%
  filter(ZCTA5CE10 != "13212",
         ZCTA5CE10 != "13211",
         ZCTA5CE10 != "13209")

index <- which(syr_zips$ZCTA5CE10 == "13214")
syr_zips[index, "ZCTA5CE10"] <- ""

map <- tm_shape(syr_zips) +
  tm_fill(col = "skyblue",
          alpha = 1,
          title = "ZIP Codes in Syracuse") +
  tm_borders(alpha = 1,
             col = "white") +
  tm_text(text = "ZCTA5CE10", 
          size = 0.7,
          col = "black") +
  tm_credits(text = "Source:\nU.S. Census Bureau" , 
             align = "left",
             position = c("left", "bottom"),
             size = 0.7) +
  tm_layout(title = "ZIP Codes",
            title.size = 1,
            title.position = c("LEFT", "TOP"), 
            main.title.position = c("LEFT", "TOP"),
            frame = FALSE) +
    tm_shape(sf_tracts) +
    tm_fill(alpha = 0.3,
            col = "tomato") +
    tm_borders(alpha = 0,
               col = "white")                         # Rationale for Inclusion: Map of ZIPs & City Limits


city_zips <- c("13204", "13290", "13208", "13203", "13206", "13202", "13207", "13205", "13210", "13224")

lvhc_loc <- lvhc_loc %>%
  mutate(in_city = NA)

for (i in 1:nrow(lvhc_loc)){
  if (lvhc_loc$zip[i] %in% city_zips){
    lvhc_loc$in_city[i] <- "Within"
  } else {
    lvhc_loc$in_city[i] <- "Outside"
  }
}

lvhc_inds <- lvhc_loc %>%
  group_by(project, name, zip, ssn, sex, race, in_city) %>%
  summarize(tot_weeks = n(),
            tot_hours = sum(hours, na.rm = TRUE),
            tot_gross = sum(gross, na.rm = TRUE)) %>%
  ungroup()
  
city_lims <- lvhc_inds %>%
  group_by(project, race, in_city) %>%
  summarize(tots = n(),
            weeks = sum(tot_weeks),
            hours = sum(tot_hours),
            gross = sum(tot_gross)) %>%
  arrange(project, reorder(in_city, gross)) %>%
  ungroup()

proj_tots <- city_lims %>%
  group_by(project) %>%
  summarize(proj_tots = sum(tots),
            proj_weeks = sum(weeks),
            proj_hours = sum(hours),
            proj_gross = sum(gross))

city_lims_viz <- city_lims %>% 
  left_join(proj_tots, by = "project") %>%
  mutate(wrk_perc = tots / proj_tots,
         wks_perc = weeks / proj_weeks,
         hrs_perc = hours / proj_hours,
         grs_perc = gross / proj_gross)

city_lims_tbl <- city_lims_viz %>%
  select(project:tots, wrk_perc, weeks, wks_perc, hours, hrs_perc, gross, grs_perc) %>%
  mutate(weeks = number(weeks, big.mark = ","),
         hours = number(hours, big.mark = ","),
         gross = dollar(gross, big.mark = ","),
         wrk_perc = percent(wrk_perc, accuracy = 0.01),
         wks_perc = percent(wks_perc, accuracy = 0.01),
         hrs_perc = percent(hrs_perc, accuracy = 0.01),
         grs_perc = percent(grs_perc, accuracy = 0.01)) %>%
  rename("Project" = project,
         "Race" = race,
         "Syracuse" = in_city,
         "Workers" = tots,
         "Proj. Workers (%)" = wrk_perc,
         "Weeks" = weeks,
         "Proj. Weeks (%)" = wks_perc,
         "Hours" = hours,
         "Proj. Hours (%)" = hrs_perc,
         "Gross" = gross,
         "Proj. Gross (%)" = grs_perc)                              # City lims by race

city_lims_overview_viz <- city_lims_viz %>%
  group_by(project, in_city) %>%
  summarize(workers = sum(tots),
            weeks = sum(weeks),
            hours = sum(hours),
            gross = sum(gross)) %>%
  ungroup() %>%
  left_join(proj_tots, by = "project") %>%
  mutate(wrk_perc = workers / proj_tots,
         wks_perc = weeks / proj_weeks,
         hrs_perc = hours / proj_hours,
         grs_perc = gross / proj_gross)

city_lims_overview_tbl <- city_lims_overview_viz %>%
  select(project:workers, wrk_perc, weeks, wks_perc, hours, hrs_perc, gross, grs_perc) %>%
  mutate(weeks = number(weeks, big.mark = ","),
         hours = number(hours, big.mark = ","),
         gross = dollar(gross, big.mark = ","),
         wrk_perc = percent(wrk_perc, accuracy = 0.01),
         wks_perc = percent(wks_perc, accuracy = 0.01),
         hrs_perc = percent(hrs_perc, accuracy = 0.01),
         grs_perc = percent(grs_perc, accuracy = 0.01)) %>%
  rename("Project" = project,
         "Syracuse" = in_city,
         "Workers" = workers,
         "Proj. Workers (%)" = wrk_perc,
         "Weeks" = weeks,
         "Proj. Weeks (%)" = wks_perc,
         "Hours" = hours,
         "Proj. Hours (%)" = hrs_perc,
         "Gross" = gross,
         "Proj. Gross (%)" = grs_perc)

