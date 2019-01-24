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
if(!require(readxl)){install.packages("readxl")}
if(!require(stringr)){install.packages("stringr")}
if(!require(lubridate)){install.packages("lubridate")}

library(zoo)
library(readr)
library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(lubridate)


# SET WORKING DIRECTORY; RETRIEVE DATA

setwd("~/Projects/REIS/Master")

urls <- c("https://raw.githubusercontent.com/jamisoncrawford/reis/master/Datasets/690_workforce_summary.csv",
          "https://raw.githubusercontent.com/jamisoncrawford/reis/master/Datasets/690_util_tidy.csv",
          "https://raw.githubusercontent.com/jamisoncrawford/reis/master/Datasets/expo_tidy.csv",
          "https://raw.githubusercontent.com/jamisoncrawford/reis/master/Datasets/lakeview_hancock_merge.csv")
names <- c("wsum", "util", "expo", "lvhc")

do.call("read_csv", mget(names[1]))

for (i in 1:length(names)){
  assign(names[i], read_csv(urls[i]))
}

rm(i, names, urls)


# AGGREGATE DATASETS TO I-690 WORKER SUMMARY LEVEL: "GROSS" & "HOURS"

wrk_race_690 <- as_data_frame(table(wsum$race)) %>%     # Race distribution, I-690
  rename(race = Var1, count = n) %>%
  mutate(percent = count / sum(count))

wrk_sex_690 <- as_data_frame(table(wsum$sex)) %>%       # Sex distribution
  rename(sex = Var1, count = n) %>%
  mutate(percent = count / sum(count))

tot <- wsum %>%
  filter(!is.na(race),
         !is.na(sex)) %>%
  nrow()

workers_sxrc_690 <- wsum %>%
  filter(!is.na(race),
         !is.na(sex)) %>%
  group_by(sex, race) %>%
  summarize(count = n(),
            total = tot,
            percent = count / total) %>%
  mutate(project = "I-690")

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

hrs_sxtd_690$sex <- c("Female", "Male")

hrs_rc_690 <- util %>%
  group_by(race) %>%
  summarize(hours = sum(race_hours))                    # Hours by race

tot_hrs <- sum(util$total_m, util$total_f)
wht_hrs <- data_frame(race = "White", hours = tot_hrs - sum(hrs_rc_690$hours))

bind_rows(hrs_rc_690, wht_hrs)

tmp <- util %>%
  select(trade, total_m:total_f, race:tot_hrs) %>%
  group_by(trade, sex) %>%
  mutate(all_hrs = sum(total_m, total_f))

tmp %>%
  group_by(trade) %>%
  mutate(hours = tot_hrs / sum(all_hrs))

rm(tot_hrs, tot_wht)


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
            dem_hrs = dem_hrs,
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
  filter(!is.na(wages)) %>%
  group_by(sex) %>%
  summarize(earnings = sum(wages, na.rm = TRUE),
            percent = sum(wages) / tot_wage_expo)               # Total gross by sex

expo_gross_rac <- expo %>% 
  filter(!is.na(wages)) %>%
  group_by(race) %>%
  summarize(earnings = sum(wages, na.rm = TRUE),
            percent = sum(wages) / tot_wage_expo)               # Total gross by race

expo_gross_sxrc <- expo %>% 
  filter(!is.na(wages)) %>%
  group_by(sex, race) %>%
  summarize(earnings = sum(wages, na.rm = TRUE),
            percent = sum(wages) / tot_wage_expo)               # Total gross by race, sex

expo_hours_sex <- expo %>% 
  filter(!is.na(hours)) %>%
  group_by(sex) %>%
  summarize(tot_hours = sum(hours, na.rm = TRUE),
            percent = tot_hours / tot_wage_expo)                # Total hours by sex

expo_hours_rac <- expo %>% 
  filter(!is.na(hours)) %>%
  group_by(race) %>%
  summarize(tot_hours = sum(hours, na.rm = TRUE),
            percent = tot_hours / tot_wage_expo)                # Total hours by race

expo_hours_sxrc <- expo %>% 
  filter(!is.na(hours)) %>%
  group_by(sex, race) %>%
  summarize(tot_hours = sum(hours, na.rm = TRUE),
            percent = sum(hours) / tot_wage_expo)               # Total hours by race, sex

employees <- sum(expo$employees)

expo_workers_sex <- expo %>%
  filter(!is.na(employees),
         !is.na(sex)) %>%
  group_by(sex) %>%
  summarize(tot_emps = sum(employees)) %>%
  as_data_frame() %>%
  mutate(percent = tot_emps / employees)                        # Total employees by sex

expo_workers_rac <- expo %>%
  filter(!is.na(employees),
         !is.na(race)) %>%
  group_by(race) %>%
  summarize(tot_emps = sum(employees)) %>%
  as_data_frame() %>%
  mutate(percent = tot_emps / employees)                        # Total employees by race
  
expo_workers_rcsx <- expo %>%
  filter(!is.na(employees),
         !is.na(race),
         !is.na(sex)) %>%
  group_by(sex, race) %>%
  summarize(emps = sum(employees)) %>%
  as_data_frame() %>%
  mutate(percent = emps / employees)                            # Total employees by race, sex


# AGGREGATION TO I-690 LEVELS: LAKEVIEW

lvhc_dup <- lvhc
lvhc <- lvhc_dup %>% 
  filter(project == "Lakeview")

tot_wage_lvhc <- sum(lvhc$gross, na.rm = TRUE)

lvhc_gross_sex <- lvhc %>% 
  filter(!is.na(gross),
         !is.na(sex),
         !is.na(race)) %>%
  group_by(sex) %>%
  summarize(earnings = sum(gross, na.rm = TRUE),
            percent = sum(gross) / tot_wage_lvhc)               # Total gross by sex

lvhc_gross_rac <- lvhc %>% 
  filter(!is.na(gross),
         !is.na(sex),
         !is.na(race)) %>%
  group_by(race) %>%
  summarize(earnings = sum(gross, na.rm = TRUE),
            percent = sum(gross) / tot_wage_lvhc)               # Total gross by race

lvhc_gross_sxrc <- lvhc %>% 
  filter(!is.na(gross),
         !is.na(sex),
         !is.na(race)) %>%
  group_by(sex, race) %>%
  summarize(earnings = sum(gross, na.rm = TRUE),
            percent = sum(gross) / tot_wage_lvhc)               # Total gross by race, sex

tot_hours_lvhc <- sum(lvhc$hours, na.rm = TRUE)
index <- which(lvhc$sex == "male")
lvhc$sex[index] <- "Male"

lvhc_hours_sex <- lvhc %>% 
  filter(!is.na(hours),
         !is.na(sex)) %>%
  group_by(sex) %>%
  summarize(tot_hours = sum(hours, na.rm = TRUE),
            percent = tot_hours / tot_hours_lvhc)               # Total hours by sex

lvhc_hours_rac <- lvhc %>% 
  filter(!is.na(hours),
         !is.na(race)) %>%
  group_by(race) %>%
  summarize(tot_hours = sum(hours, na.rm = TRUE),
            percent = tot_hours / tot_hours_lvhc)               # Total hours by race

lvhc_hours_sxrc <- lvhc %>% 
  filter(!is.na(hours),
         !is.na(race),
         !is.na(sex)) %>%
  group_by(sex, race) %>%
  summarize(tot_hours = sum(hours, na.rm = TRUE),
            percent = sum(hours) / tot_hours_lvhc)              # Total hours by race, sex

lvhc_workers_sex <- lvhc %>%
  filter(!is.na(ssn),
         !is.na(sex),
         !is.na(name),
         !is.na(race),
         !is.na(zip)) %>%
  group_by(zip, ssn, race, sex) %>%
  summarize(occurrences = n()) %>%
  as_data_frame()

lvhc_workers_sex <- lvhc_workers_sex %>%
  group_by(sex) %>%
  summarize(count = n(),
            percent = count / count) %>%
  as_data_frame()                                              # Total employees by sex

lvhc_workers_race <- lvhc %>%
  filter(!is.na(ssn),
         !is.na(sex),
         !is.na(name),
         !is.na(race),
         !is.na(zip)) %>%
  group_by(zip, ssn, sex, race) %>%
  summarize(occurrences = n()) %>%
  as_data_frame()

occ <- sum(lvhc_workers_race$count, na.rm = TRUE)

lvhc_workers_race <- lvhc_workers_race %>%
  group_by(race) %>%
  summarize(count = n()) %>%
  as_data_frame() %>%
  mutate(total = sum(count),
         percent = count / total)                                 # Total employees by race
                                 
lvhc_workers_sxrc <- lvhc %>%
  filter(!is.na(ssn),
         !is.na(sex),
         !is.na(name),
         !is.na(race),
         !is.na(zip)) %>%
  group_by(zip, ssn, sex, race) %>%
  summarize(occurrences = n()) %>%
  as_data_frame() %>%
  group_by(sex, race) %>%
  summarize(count = n(),
            total = sum(lvhc_workers_race$count)) %>%
  as_data_frame() %>%
  mutate(percent = count / total)                               # Total employees by sex, race
  

# AGGREGATION TO I-690 LEVELS: HANCOCK

lvhc <- lvhc_dup %>% 
  filter(project == "Hancock")

tot_wage_hc <- sum(lvhc$gross, na.rm = TRUE)

hc_gross_sex <- lvhc %>% 
  filter(!is.na(gross),
         !is.na(sex),
         !is.na(race)) %>%
  group_by(sex) %>%
  summarize(earnings = sum(gross, na.rm = TRUE),
            percent = sum(gross) / tot_wage_hc)                 # Total gross by sex

hc_gross_rac <- lvhc %>% 
  filter(!is.na(gross),
         !is.na(sex),
         !is.na(race)) %>%
  group_by(race) %>%
  summarize(earnings = sum(gross, na.rm = TRUE),
            percent = sum(gross) / tot_wage_hc)                 # Total gross by race

hc_gross_sxrc <- lvhc %>% 
  filter(!is.na(gross),
         !is.na(sex),
         !is.na(race)) %>%
  group_by(sex, race) %>%
  summarize(earnings = sum(gross, na.rm = TRUE),
            percent = sum(gross) / tot_wage_hc)                 # Total gross by race, sex

tot_hours_hc <- sum(lvhc$hours, na.rm = TRUE)
index <- which(lvhc$sex == "male")
lvhc$sex[index] <- "Male"

hc_hours_sex <- lvhc %>% 
  filter(!is.na(hours),
         !is.na(sex)) %>%
  group_by(sex) %>%
  summarize(tot_hours = sum(hours, na.rm = TRUE),
            percent = tot_hours / tot_hours_hc)                 # Total hours by sex

hc_hours_rac <- lvhc %>% 
  filter(!is.na(hours),
         !is.na(race)) %>%
  group_by(race) %>%
  summarize(tot_hours = sum(hours, na.rm = TRUE),
            percent = tot_hours / tot_hours_hc)                 # Total hours by race

hc_hours_sxrc <- lvhc %>% 
  filter(!is.na(hours),
         !is.na(race),
         !is.na(sex)) %>%
  group_by(sex, race) %>%
  summarize(tot_hours = sum(hours, na.rm = TRUE),
            percent = sum(hours) / tot_hours_hc)                # Total hours by race, sex

hc_workers_sex <- lvhc %>%
  filter(!is.na(ssn),
         !is.na(sex),
         !is.na(name),
         !is.na(race),
         !is.na(zip)) %>%
  group_by(zip, ssn, race, sex) %>%
  summarize(occurrences = n()) %>%
  as_data_frame()

hc_workers_sex <- hc_workers_sex %>%
  group_by(sex) %>%
  summarize(count = n(),
            total = sum(hc_workers_sex$count)) %>%
  as_data_frame() %>%
  mutate(percent = count / total)                               # Total employees by sex

hc_workers_race <- lvhc %>%
  filter(!is.na(ssn),
         !is.na(sex),
         !is.na(name),
         !is.na(race),
         !is.na(zip)) %>%
  group_by(zip, ssn, sex, race) %>%
  summarize(occurrences = n()) %>%
  as_data_frame()

hc_workers_race <- hc_workers_race %>%
  group_by(race) %>%
  summarize(count = n()) %>%
  as_data_frame() %>%
  mutate(total = count / sum(count))                            # Total employees by race

hc_workers_sxrc <- lvhc %>%
  filter(!is.na(ssn),
         !is.na(sex),
         !is.na(name),
         !is.na(race),
         !is.na(zip)) %>%
  group_by(zip, ssn, sex, race) %>%
  summarize(occurrences = n()) %>%
  as_data_frame() %>%
  group_by(sex, race) %>%
  summarize(count = n(),
            total = sum(hc_workers_race$count)) %>%
  as_data_frame() %>%
  mutate(percent = count / total)                               # Total employees by sex, race


# AGGREGATE FINDINGS: TOTAL EMPLOYEES

hc <- hc_workers_race %>%
  select(race, count, total) %>%
  rename("percent" = total) %>%
  mutate(project = "Hancock")

lv <- lvhc_workers_race %>%
  select(-total) %>%
  mutate(project = "Lakeview")

ex <- expo_workers_rac %>%
  select(race:percent) %>%
  rename("count" = tot_emps) %>%
  mutate(project = "Expo Center")

hw <- wrk_race_690 %>%
  mutate(project = "I-690")

names <- c(hc, lv, ex, hw)

all_races <- hc %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Race distribution by project

lv <- lvhc_workers_sex %>%
  mutate(percent = count / sum(count),
         project = "Lakeview")

hc <- hc_workers_sex %>%
  select(-total) %>%
  mutate(project = "Hancock")

hc[1, "percent"] <- 1
nr <- data_frame(sex = "Female", 
                 count = 0, 
                 percent = 0, 
                 project = "Hancock")
hc <- hc %>% bind_rows(nr)

ex <- expo_workers_sex %>%
  rename("count" = tot_emps) %>%
  mutate(project = "Expo Center")

hw <- wrk_sex_690 %>%
  mutate(percent = as.numeric(percent)) %>%
  mutate(project = "I-690")

all_sexes <- hc %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Sex distribution by project

hw <- workers_sxrc_690

hc <- hc_workers_sxrc %>%
  mutate(project = "Hancock")

ex <- expo_workers_rcsx %>%
  rename("count" = emps) %>%
  mutate(total = sum(ex$emps),
         project = "Expo Center") %>%
  select(sex:count, total, percent:project)

lv <- lvhc_workers_sxrc %>%
  mutate(project = "Lakeview")

all_sxrc <- hc %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Sex, race distribution by project


# AGGREGATE FINDINGS: HOURS DISTRIBUTION

hw <- hrs_race_690 %>%
  rename("count" = total) %>%
  select(-mean, -median) %>%
  mutate(total = sum(hw$total),
         percent = count / total,
         project = "I-690")

ex <- expo_hours_rac %>%
  rename("count" = tot_hours) %>%
  mutate(total = sum(count),
         project = "Expo Center") %>%
  select(race:count, total, percent, project)

lv <- lvhc_hours_rac %>%
  rename("count" = tot_hours) %>%
  mutate(total = sum(count),
         project = "Lakeview")

hc <- hc_hours_rac %>%
  rename("count" = tot_hours) %>%
  mutate(total = sum(count),
         project = "Hancock")

all_hours_race <- hc %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Hours by race

hw <- hrs_sx_690 %>%
  rename("count" = hours) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "I-690") %>%
  select(sex:count, total, percent:project)

hw[1:2, 1] <- c("Female", "Male")

hc <- hc_hours_sex %>%
  rename("count" = tot_hours) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "Hancock") %>%
  select(sex:count, total, percent:project)

lv <- lvhc_hours_sex %>%
  rename("count" = tot_hours) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "Lakeview") %>%
  select(sex:count, total, percent:project)

ex <- expo_hours_sex %>%
  rename("count" = tot_hours) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "Expo Center") %>%
  select(sex:count, total, percent:project)

hc_fem <- data_frame(sex = "Female", 
                     count = 0.0, 
                     total = 27988.0, 
                     percent = 0.0, 
                     project = "Hancock")

all_hours_sex <- hc %>%
  bind_rows(hc_fem) %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Hours by sex

options(scipen = 999)                                           # Disable sci. notation

hw <- as_data_frame(hrs_rcsx_690_fnl)
ex <- as_data_frame(expo_hours_sxrc)
hc <- as_data_frame(hc_hours_sxrc)
lv <- as_data_frame(lvhc_hours_sxrc)

lv <- lv %>%
  rename("count" = tot_hours) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "Lakeview") %>%
  select(sex:race, count, total, percent:project)

ex <- ex %>%
  rename("count" = tot_hours) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "Expo Center") %>%
  select(sex:race, count, total, percent:project)

hc <- hc %>%
  rename("count" = tot_hours) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "Expo Center") %>%
  select(sex:count, total, percent:project)

hw <- hw %>% 
  select(sex, race, tot_hrs) %>%
  rename("count" = tot_hrs) %>%
  group_by(sex, race) %>%
  summarize(count = sum(count)) %>%
  as_tibble() %>%
  mutate(total = as.double(sum(count)),
         percent = count / total,
         project = "I-690")

all_hours_sxrc <- hc %>%
  bind_rows(hc_fem) %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Hours by sex, race, project


# AGGREGATE FINDINGS: GROSS DISTRIBUTION

hw <- as_data_frame(grs_race_690)
ex <- as_data_frame(expo_gross_rac)
hc <- as_data_frame(hc_gross_rac)
lv <- as_data_frame(lvhc_gross_rac)

hw <- hw %>%
  select(race:total) %>%
  rename("count" = total) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "I-690") %>%
  select(race, count, total, percent, project)
  
ex <- ex %>%
  rename("count" = earnings) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "Expo Center") %>%
  select(race, count, total, percent, project)

hc <- hc %>%
  rename("count" = earnings) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "Hancock") %>%
  select(race, count, total, percent, project)

lv <- lv %>%
  rename("count" = earnings) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "Lakeview") %>%
  select(race, count, total, percent, project)

all_gross_race <- hc %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Gross by race, project

hw <- as_data_frame(grs_sex_690)
ex <- as_data_frame(expo_gross_sex)
hc <- as_data_frame(hc_gross_sex)
lv <- as_data_frame(lvhc_gross_sex)

hw <- hw %>%
  select(sex:total) %>%
  rename("count" = total) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "I-690") %>%
  select(sex, count, total, percent, project)

ex <- ex %>%
  rename("count" = earnings) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "Expo Center") %>%
  select(sex, count, total, percent, project)

hc <- hc %>%
  rename("count" = earnings) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "Hancock") %>%
  select(sex, count, total, percent, project)

index <- which(lv$sex == "male")
lv[index, "sex"] <- "Male"

lv <- lv %>%
  group_by(sex) %>%
  summarize(count = sum(earnings)) %>%
  as_tibble() %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "Lakeview") %>%
  select(sex, count, total, percent, project)

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
  select(sex, race, count, total, percent, project)

ex <- ex %>%
  rename("count" = earnings) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "Expo Center") %>%
  select(sex, race, count, total, percent, project)

hc <- hc %>%
  rename("count" = earnings) %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "Hancock") %>%
  select(sex, race, count, total, percent, project)

index <- which(lv$sex == "male")
lv[index, "sex"] <- "Male"

lv <- lv %>%
  group_by(sex, race) %>%
  summarize(count = sum(earnings)) %>%
  as_tibble() %>%
  mutate(total = sum(count),
         percent = count / total,
         project = "Lakeview") %>%
  select(sex, race, count, total, percent, project)

all_gross_sxrc <- hc %>%
  bind_rows(lv) %>%
  bind_rows(ex) %>%
  bind_rows(hw)                                                 # Gross by race, sex, project


# ADDITIONAL FINDINGS: EXPO CENTER

expo_sex_class <- expo %>%
  filter(!is.na(title),
         !is.na(sex)) %>%
  group_by(sex, title) %>%
  summarize(count = n()) %>%
  as_data_frame() %>%
  mutate(total = sum(count),
         percent = count / total,
         name = "Expo Center")                                  # Class by sex, Expo

expo_race_class <- expo %>%
  filter(!is.na(title),
         !is.na(race)) %>%
  group_by(race, title) %>%
  summarize(count = n()) %>%
  as_data_frame() %>%
  mutate(total = sum(count),
         percent = count / total,
         name = "Expo Center")                                  # Class by race, Expo
  
expo_rcsx_class <- expo %>%
  filter(!is.na(title),
         !is.na(race),
         !is.na(sex)) %>%
  group_by(sex, race, title) %>%
  summarize(count = n()) %>%
  as_data_frame() %>%
  mutate(total = sum(count),
         percent = count / total,
         name = "Expo Center")                                  # Class by sex and race, Expo


# WRITE TO CSV

names <- c("all_gross_race", "all_gross_sex",   "all_gross_sxrc", 
           "all_hours_race", "all_hours_sex",   "all_hours_sxrc", 
           "all_races",      "all_sexes",       "all_sxrc", 
           "expo_sex_class", "expo_race_class", "expo_rcsx_class")

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

write_csv(expo_sex_class, files[10])
write_csv(expo_race_class, files[11])
write_csv(expo_rcsx_class, files[12])