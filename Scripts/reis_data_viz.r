# REIS Final Data Tables & Visualizations

## RStudio Version: 1.1.456
## R Version: 3.5.1
## Windows 10

## Script Version: 1.1
## Updated: 2019-02-13


# CLEAR WORKSPACE; INSTALL/LOAD PACKAGES

if(!require(zoo)){install.packages("zoo")}
if(!require(readr)){install.packages("readr")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(scales)){install.packages("scales")}
if(!require(readxl)){install.packages("readxl")}
if(!require(zipcode)){install.packages("zipcode")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(stringr)){install.packages("stringr")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(gridExtra)){install.packages("gridExtra")}
if(!require(kableExtra)){install.packages("kableExtra")}

library(zoo)
library(readr)
library(tidyr)
library(dplyr)
library(scales)
library(readxl)
library(zipcode)
library(ggplot2)
library(stringr)
library(lubridate)
library(gridExtra)
library(kableExtra)

# Web-Recommended Packages (Tables as Images)

    ## Source: https://haozhu233.github.io/kableExtra/save_kable_and_as_image.html
    ## Warning: Installs Software (http://phantomjs.org/)

# Note: Run the following script: "reis_findings.rmd"

# Script Location: https://github.com/jamisoncrawford/reis/tree/master/Scripts

# Graphics Punch List: https://docs.google.com/spreadsheets/d/1-K1Nw5DEOLmFOhkIc0CdZtOil9kEwEoYWvnF8wuAuDs/edit?usp=sharing

setwd("~/Projects/REIS/Master/Graphics & Tables")

### EXPO CENTER

## Visual: Total Hours Worked by Race, Expo Center

viz_expo_rc_hrs <- hrs_rc_viz %>%
  filter(Project == "Expo Center",
         !is.na(Race)) %>%
  select(-Total)

bar_expo_rc_hrs <- ggplot(viz_expo_rc_hrs, aes(x = reorder(Race, Percent), y = Count)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +
  labs(title = "Total hours worked by race",
       subtitle = "Expo Center",
       x = "Race", 
       y = "Hours", 
       caption = "Source: NYS Office of General Services")

ggsave(plot = bar_expo_rc_hrs, 
       filename = "expo_rc_hrs_bar.jpg",
       bg = "transparent")

## Table: Hours Worked by Race, Expo

tbl_expo_rc_hrs <- hrs_rc_viz %>%
  filter(Project == "Expo Center") %>%
  select(-Project, -Total) %>%
  mutate(Percent = percent(Percent, accuracy = 0.01),
         Count = number(Count, big.mark = ",")) %>%
  rename("Total Hours" = Count)

write_csv(tbl_expo_rc_hrs, "expo_rc_hrs_tbl.csv")

# Export Issues:: kableEXtra & MiKTeX conflict

## Visual: Hours Worked by Race & Sex, Expo

viz_exp_sxrc_hrs <- hrs_sxrc_viz %>%
  filter(Project == "Expo Center") %>%
  select(-Project) %>%
  arrange(desc(Gender), desc(Percent))

bar_exp_sxrc_hrs <- ggplot(viz_exp_sxrc_hrs, aes(x = reorder(Race, Percent), y = Count, fill = Gender)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +
  labs(title = "Total hours worked by gender and race",
       subtitle = "Expo Center",
       x = "Race", 
       y = "Hours", 
       caption = "Source: NYS Office of General Services")

ggsave(plot = bar_exp_sxrc_hrs, 
       filename = "expo_sxrc_hrs_bar.jpg",
       bg = "transparent")

## Table: Hours Worked by Race & Sex, Expo

tbl_expo_sxrc_hrs <- viz_exp_sxrc_hrs %>%
  mutate(Percent = percent(Percent, accuracy = 0.01),
         Count = number(Count, big.mark = ","))

write_csv(tbl_expo_sxrc_hrs, "expo_sxrc_hrs_tbl.csv")

## Gross Wages by Race, Expo

viz_expo_rc_grs <- gp_race_viz %>%
  filter(Project == "Expo Center") %>%
  select(-Project)

bar_expo_rc_grs <- ggplot(viz_expo_rc_grs, aes(x = reorder(Race, Percent), y = Count)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = dollar) +
  labs(title = "Total gross income by race",
       subtitle = "Expo Center",
       x = "Race", 
       y = "Gross", 
       caption = "Source: NYS Office of General Services")

ggsave(plot = bar_expo_rc_grs, 
       filename = "expo_rc_grs_bar.jpg",
       bg = "transparent")

## Visual: Gross Wages by Sex and Race, Expo

viz_expo_sxrc_grs <- gp_sxrc_viz %>%
  filter(Project == "Expo Center") %>%
  select(-Project)

bar_expo_sxrc_grs <- ggplot(viz_expo_sxrc_grs, aes(x = reorder(Race, Percent), y = Count, fill = reorder(Gender, Percent))) +
  coord_flip() +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = dollar) +
  labs(title = "Total gross income by gender and race",
       subtitle = "Expo Center",
       x = "Race", 
       y = "Gross", 
       fill = "Gender",
       caption = "Source: NYS Office of General Services")

ggsave(plot = bar_expo_sxrc_grs, 
       filename = "expo_sxrc_grs_bar.jpg",
       bg = "transparent")

## Table: Gross Wage by Race

tbl_expo_rc_grs <- viz_expo_rc_grs %>%
  arrange(desc(Percent)) %>%
  mutate(Percent = percent(Percent, accuracy = 0.01),
         Count = number(Count, big.mark = ","))

write_csv(tbl_expo_rc_grs, "expo_rc_grs_tbl.csv")

## Table: Gross Wage by Sex & Race

tbl_expo_sxrc_grs <- viz_expo_sxrc_grs %>%
  arrange(desc(Gender), desc(Percent)) %>%
  mutate(Percent = percent(Percent, accuracy = 0.01),
         Count = number(Count, big.mark = ","))

write_csv(tbl_expo_sxrc_grs, "expo_sxrc_grs_tbl.csv")

# Percentage of Wages, Hours v. Proportion of Workforce

tmp_expo_rc_grs <- viz_expo_rc_grs %>%
  mutate(Indicator = "Gross")

tmp_expo_rc_hrs <- viz_expo_rc_hrs %>%
  mutate(Indicator = "Hours") %>%
  select(-Project)

tmp_expo_rc_viz <- bind_rows(tmp_expo_rc_hrs, tmp_expo_rc_grs)

tmp_expo_rc <- ggplot(tmp_expo_rc_viz, aes(x = Race, y = Percent, fill = reorder(Indicator, Percent))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = percent) +
  labs(title = "Proportion of hours v. gross earnings by race",
       subtitle = "Scale: 100%",
       x = NULL,
       y = NULL,
       fill = "Proportion",
       caption = "") + 
  theme(legend.position = "none")

tmp_expo_rc2 <- tmp_expo_rc +
  coord_flip(ylim = c(0, 0.05)) +
  scale_y_continuous(breaks = c(0, 0.025, 0.05),
                     labels = percent) +
  theme(legend.position = "right") +
  labs(title = "",
       subtitle = "Scale: 5%",
       caption = "Source: NYS Office of General Services") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

png("expo_rc_grshrs_spec.jpg", width = 700, height = 350, bg = "transparent")      # Note: Requires graphics device
grid.arrange(tmp_expo_rc, tmp_expo_rc2, ncol=2)
dev.off()


## I-690 VISUALIZATIONS

# Viz: Hours by Gender

hw_sx_hrs <- all_hours_sex %>%
  filter(project == "I-690") %>%
  arrange(desc(sex))

viz_hw_sx_hrs <- ggplot(hw_sx_hrs, aes(x = sex, y = count)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +
  labs(title = "Total hours by gender",
       subtitle = "I-690",
       x = "Gender", 
       y = "Hours",
       caption = "Source: [Insert Source]")

ggsave(plot = viz_hw_sx_hrs, 
       filename = "hw_sx_hrs_bar.jpg",
       bg = "transparent")

# Table: Hours by Gender

tbl_hw_sx_hrs <- hw_sx_hrs %>%
  select(-total, - project) %>%
  mutate(count = number(count, big.mark = ","),
         percent = percent(percent, accuracy = 0.01)) %>%
  rename(Gender = sex,
         Hours = count,
         `Hours (%)` = percent)

# Viz: Workforce by Race

hw_rc_wf <- all_races %>%
  filter(project == "I-690") %>%
  arrange(reorder(race, desc(percent)))

viz_hw_rc_wf <- ggplot(hw_rc_wf, aes(x = reorder(race, percent), y = count)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma) +
  labs(title = "Total workers by race",
       subtitle = "I-690",
       x = "Race", 
       y = "Workers",
       caption = "Source: [Insert Source]")

ggsave(plot = viz_hw_rc_wf, 
       filename = "hw_rc_wf_bar.jpg",
       bg = "transparent")

# Table: Workforce by Race

tbl_hw_rc_wf <- hw_rc_wf %>%
  select(-total, - project) %>%
  mutate(count = number(count, big.mark = ","),
         percent = percent(percent, accuracy = 0.01)) %>%
  rename("Race" = race,
         "Workers" = count,
         `Workers (%)` = percent)

# Preparation: I-690 Sex, Race, & Trades (Hours)

url <- "https://raw.githubusercontent.com/jamisoncrawford/reis/master/Datasets/690_utilization.csv"

hw_trades <- read_csv(url) %>%
  select(trade:native_f, total_emp_m:total_min_f)

index <- which(hw_trades$trade == "Welders & Citters")
hw_trades$trade[index] <- "Welders & Cutters"

hw_black <- hw_trades %>% 
  select(trade, black_m:black_f) %>% 
  rename(Male = black_m, Female = black_f) %>%
  gather(key = sex, value = count, Male, Female) %>%
  filter(count != 0) %>%
  mutate(race = "Black") %>%
  arrange(trade)

hw_hispanic <- hw_trades %>% 
  select(trade, hispanic_m:hispanic_f) %>% 
  rename(Male = hispanic_m, Female = hispanic_f) %>%
  gather(key = sex, value = count, Male, Female) %>%
  filter(count != 0) %>%
  mutate(race = "Hispanic") %>%
  arrange(trade)

hw_asian <- hw_trades %>% 
  select(trade, asian_m:asian_f) %>% 
  rename(Male = asian_m, Female = asian_f) %>%
  gather(key = sex, value = count, Male, Female) %>%
  filter(count != 0) %>%
  mutate(race = "Asian") %>%
  arrange(trade)

hw_native <- hw_trades %>% 
  select(trade, native_m:native_f) %>% 
  rename(Male = native_m, Female = native_f) %>%
  gather(key = sex, value = count, Male, Female) %>%
  filter(count != 0) %>%
  mutate(race = "Native") %>%
  arrange(trade)

hw_rc_mins <- bind_rows(hw_black, hw_hispanic, hw_asian, hw_native)

hw_white_m <- hw_trades %>%
  select(trade:native_f) %>%
  select(trade, ends_with(match = "_m")) %>%
  mutate(white_male_hrs = total_m - (black_m + hispanic_m + asian_m + native_m)) %>%
  select(trade, white_male_hrs) %>%
  rename(count = white_male_hrs) %>%
  filter(count != 0) %>%
  mutate(race = "White") %>%
  arrange(trade) %>%
  mutate(sex = "Male")

hw_white_f <- hw_trades %>%
  select(trade:native_f) %>%
  select(trade, ends_with(match = "_f")) %>%
  mutate(white_female_hrs = total_f - (black_f + hispanic_f + asian_f + native_f)) %>%
  select(trade, white_female_hrs) %>%
  rename(count = white_female_hrs) %>%
  filter(count != 0) %>%
  mutate(race = "White") %>%
  arrange(trade) %>%
  mutate(sex = "Female")

hw_white <- bind_rows(hw_white_m, hw_white_f) %>%
  arrange(desc(sex), desc(count)) %>%
  select(trade, sex, count, race)

# Viz: Hours by Sex & Race

hw_sxrc_hrs_trade <- bind_rows(hw_white, hw_rc_mins) %>%
  mutate(total = sum(count),
         percent = count / total) %>%
  select(-total) %>%
  arrange(desc(sex), reorder(trade, desc(count)))

viz_hw_sxrc_hrs_trade <- ggplot(hw_sxrc_hrs_trade, aes(x = race, y = count, fill = sex)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = c("Hispanic", "Native", "Black", "White")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Total hours by gender and race",
       subtitle = "I-690",
       x = "Race", 
       y = "Hours",
       fill = "Gender",
       caption = "Source: [Insert Source]")

ggsave(plot = viz_hw_sxrc_hrs_trade, 
       filename = "hw_sxrc_hrs_bar.jpg",
       bg = "transparent")

# Viz: Hours by Race & Trade

viz_hw_sxrc_hrs_trade <- ggplot(hw_sxrc_hrs_trade, aes(x = reorder(trade, desc(count)), y = count, fill = race)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = c("Foreman", "Laborers Unskilled", "Piledriver", "Surveyors", "Cement Masons",       
                              "Electricians", "Iron Workers", "Carpenters", "Equipment Operator",  
                              "Laborers Semiskilled")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 10 trades by hourage and race",
       subtitle = "All workers, 99.45% of hours",
       x = "Trades", 
       y = "Hours",
       fill = "Race",
       caption = "") +
  theme(legend.position = "none")

hw_sxrc_hrs_trade_min <- hw_sxrc_hrs_trade
index <- which(hw_sxrc_hrs_trade_min$race == "White")
hw_sxrc_hrs_trade_min[index, "count"] <- 0
hw_sxrc_hrs_trade_min[index, "percent"] <- 0

viz_hw_sxrc_hrs_trade_min <- ggplot(hw_sxrc_hrs_trade_min, aes(x = reorder(trade, desc(count)), y = count, fill = race)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = c("Foreman", "Laborers Unskilled", "Piledriver", "Surveyors", "Cement Masons",       
                              "Electricians", "Iron Workers", "Carpenters", "Equipment Operator",  
                              "Laborers Semiskilled")) +
  scale_y_continuous(labels = comma) +
  labs(title = "",
       subtitle = "Minority workers only",
       x = "", 
       y = "",
       fill = "Race",
       caption = "Source: [Insert Source]") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

png("hw_rc_hrs_trades_spec.jpg", width = 800, height = 400, bg = "transparent")      # Note: Requires graphics device
grid.arrange(viz_hw_sxrc_hrs_trade, viz_hw_sxrc_hrs_trade_min, ncol=2)
dev.off()

# Preparation: I-690 Workers by Race, Sex, Trade

url <- "https://raw.githubusercontent.com/jamisoncrawford/reis/master/Datasets/690_utilization.csv"

hw_wf_trades_m <- read_csv(url) %>%
  select(trade, total_emp_m:total_min_f) %>%
  select(trade, ends_with("_m")) %>%
  mutate(White = total_emp_m - total_min_m) %>%
  rename(Minority = total_min_m,
         Total = total_emp_m,
         Trade = trade) %>%
  gather(key = Race, value = Count, Minority, White) %>%
  select(Trade, Race, Count, Total) %>%
  filter(Total != 0) %>%
  mutate(Percent = Count / Total,
         Gender = "Male")
  
hw_wf_trades_f <- read_csv(url) %>%
  select(trade, total_emp_m:total_min_f) %>%
  select(trade, ends_with("_f")) %>%
  mutate(White = total_emp_f - total_min_f) %>%
  rename(Minority = total_min_f,
         Total = total_emp_f,
         Trade = trade) %>%
  gather(key = Race, value = Count, Minority, White) %>%
  select(Trade, Race, Count, Total) %>%
  filter(Total != 0) %>%
  mutate(Percent = Count / Total,
         Gender = "Female")

hw_wf_trades <- bind_rows(hw_wf_trades_m, hw_wf_trades_f) %>%
  select(Trade, Gender, Race, Count, Total) %>%
  arrange(desc(Race), desc(Count))

index <- which(hw_wf_trades$Trade == "Welders & Citters")

hw_wf_trades[index, "Trade"] <- "Welders & Cutters"

# Viz: Total Minority Workers by Trade

trades <- c("Clerical", "Mechanic", "Other", "Surveyors", "Asbestos Workers", "Supervisor",
            "Welders & Cutters", "Truck Driver", "Foreman", "Laborers Unskilled", "Piledriver",
            "Electricians", "Cement Masons", "Carpenters", "Iron Workers", "Equipment Operator",
            "Laborers Semiskilled")

viz_hw_wf_trades <- ggplot(hw_wf_trades, aes(x = reorder(Trade, Count), y = Count, fill = Race)) +
  coord_flip() +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma, breaks = c(0, 25, 50, 75, 100)) +
  scale_x_discrete(limits = trades) +
  labs(title = "Total workers by minority status and trade",
       subtitle = "I-690",
       x = "Trades", 
       y = "Workers",
       fill = "Status",
       caption = "Source: [Insert Source]")

ggsave(plot = viz_hw_wf_trades, 
       filename = "hw_wf_trades_bar.jpg",
       bg = "transparent")

# Table: Total Workers, Minority Workers, Proportion by Trade

hw_wf_trades_w <- hw_wf_trades %>%
  group_by(Trade, Race) %>%
  summarize(Count = sum(Count)) %>%
  ungroup() %>%
  mutate(Total = sum(Count),
         Percent = Count / Total) %>%
  filter(Race == "White") %>%
  select(-Total) %>%
  arrange(Trade)

hw_wf_trades_m <- hw_wf_trades %>%
  group_by(Trade, Race) %>%
  summarize(`Minority Count` = sum(Count)) %>%
  ungroup() %>%
  mutate(`Minority Total` = sum(`Minority Count`),
         `Minority Percent` = `Minority Count` / `Minority Total`) %>%
  filter(Race == "Minority") %>%
  select(-`Minority Total`) %>%
  arrange(Trade) %>%
  select(-Trade, -Race)

tidy_hw_wf_trades <- bind_cols(hw_wf_trades_w, hw_wf_trades_m)

tbl_hw_wf_trades <- bind_cols(hw_wf_trades_w, hw_wf_trades_m) %>%
  rename("White Workers" = Count,
         "Minority Workers" = `Minority Count`) %>%
  select(-Race, -Percent, -`Minority Percent`) %>%
  mutate(`Total Workers` = `White Workers` + `Minority Workers`,
         `White Workers (%)` = `White Workers` / `Total Workers`,
         `Minority Workers (%)` = `Minority Workers` / `Total Workers`) %>%
  select(Trade, `White Workers`, `White Workers (%)`, 
         `Minority Workers`, `Minority Workers (%)`, `Total Workers`) %>%
  mutate(`White Workers (%)` = percent(`White Workers (%)`, accuracy = 0.01),
         `Minority Workers (%)` = percent(`Minority Workers (%)`, accuracy = 0.01)) %>%
  arrange(desc(`Total Workers`))

write_csv(tbl_hw_wf_trades, "hw_wf_trades_tbl.csv")

# Table: Total & Proportion of Hours by Race, Gender

hw_sxrc_hrs <- hw_sxrc_hrs_trade %>%
  group_by(sex, race) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  mutate(total = sum(count),
         percent = count / total)

tbl_hw_sxrc_hrs <- hw_sxrc_hrs %>%
  select(-total) %>%
  arrange(desc(count)) %>%
  mutate(count = number(count, big.mark = ","),
         percent = percent(percent, accuracy = 0.01)) %>%
  rename(Gender = sex,
         Race = race,
         Hours = count,
         `Hours (%)` = percent)

write_csv(tbl_hw_sxrc_hrs, "hw_sxrc_hrs_tbl.csv")


### I-690 CLASSES BY GENDER & MINORITY STATUS

url_hr <- "https://raw.githubusercontent.com/jamisoncrawford/reis/master/Datasets/hw_hrs_class.csv"
url_wf <- "https://raw.githubusercontent.com/jamisoncrawford/reis/master/Datasets/hw_wf_class.csv"

hw_hr_class <- read_csv(url_hr)
hw_wf_class <- read_csv(url_wf)

hw_wf_class$trade <- hw_hr_class$trade

rm(url_hr, url_wf)

hw_wf_class <- hw_wf_class %>%
  mutate(denom = as.integer(str_extract(allow_2, "[0-9]{1}$")),
         actual = tot_j / tot_a,
         potent = round(tot_j / denom, digits = 0))

for (i in 1:nrow(hw_wf_class)){
  if (hw_wf_class$actual[i] == Inf & !is.na(hw_wf_class$denom[i])){
    hw_wf_class$actual[i] <- 0
  }
}

hw_wf_class <- hw_wf_class %>%
  mutate(perc_pot = tot_a / potent)

# Workforce & Class Prep

hw_wf_class_jw <- hw_wf_class %>%
  select(trade:jwf, allow_2:perc_pot) %>%
  rename("total" = tot_j,
         "Male" = jwm,
         "Female" = jwf) %>%
  gather(key = gender, value = count, Male, Female) %>%
  mutate(race = "White",
         class = "Journeyman")

hw_wf_class_jm <- hw_wf_class %>%
  select(trade, tot_j, jmm:jmf, allow_2:perc_pot) %>%
  rename("total" = tot_j,
         "Male" = jmm,
         "Female" = jmf) %>%
  gather(key = gender, value = count, Male, Female) %>%
  mutate(race = "Minority",
         class = "Journeyman")

hw_wf_class_aw <- hw_wf_class %>%
  select(trade, tot_a, awm:awf, allow_2:perc_pot) %>%
  rename("total" = tot_a,
         "Male" = awm,
         "Female" = awf) %>%
  gather(key = gender, value = count, Male, Female) %>%
  mutate(race = "White",
         class = "Apprentice")

hw_wf_class_am <- hw_wf_class %>%
  select(trade, tot_a, amm:amf, allow_2:perc_pot) %>%
  rename("total" = tot_a,
         "Male" = amm,
         "Female" = amf) %>%
  gather(key = gender, value = count, Male, Female) %>%
  mutate(race = "Minority",
         class = "Apprentice")

hw_wf_class <- bind_rows(hw_wf_class_jw, 
                         hw_wf_class_jm, 
                         hw_wf_class_aw, 
                         hw_wf_class_am) %>%
  select(trade, class, gender, race, count, total, allow_2, actual, potent, perc_pot) %>%
  filter(count != 0) %>%
  arrange(trade, desc(class), reorder(gender, desc(count)), reorder(race, desc(count))) %>%
  mutate(percent = count / total) %>%
  select(trade, class, gender, race, count, total, percent, allow_2, actual, potent, perc_pot) %>%
  rename(ratio = allow_2) %>%
  mutate(denom = as.integer(str_extract(ratio, "[0-9]{1}$")))

index <- which(hw_wf_class$actual == Inf)

hw_wf_class$actual[index] <- NA

# Hours & Class Prep

hw_hr_class_jw <- hw_hr_class %>%
  select(trade:jwf, allow_2) %>%
  rename("total" = tot_j,
         "Male" = jwm,
         "Female" = jwf) %>%
  gather(key = gender, value = count, Male, Female) %>%
  mutate(race = "White",
         class = "Journeyman")

hw_hr_class_jm <- hw_hr_class %>%
  select(trade, tot_j, jmm:jmf, allow_2) %>%
  rename("total" = tot_j,
         "Male" = jmm,
         "Female" = jmf) %>%
  gather(key = gender, value = count, Male, Female) %>%
  mutate(race = "Minority",
         class = "Journeyman")

hw_hr_class_aw <- hw_hr_class %>%
  select(trade, tot_a, awm:awf, allow_2) %>%
  rename("total" = tot_a,
         "Male" = awm,
         "Female" = awf) %>%
  gather(key = gender, value = count, Male, Female) %>%
  mutate(race = "White",
         class = "Apprentice")

hw_hr_class_am <- hw_hr_class %>%
  select(trade, tot_a, amm:amf, allow_2) %>%
  rename("total" = tot_a,
         "Male" = amm,
         "Female" = amf) %>%
  gather(key = gender, value = count, Male, Female) %>%
  mutate(race = "Minority",
         class = "Apprentice")

hw_hr_class <- bind_rows(hw_hr_class_jw, 
                         hw_hr_class_jm, 
                         hw_hr_class_aw, 
                         hw_hr_class_am) %>%
  select(trade, class, gender, race, count, total, allow_2) %>%
  filter(count != 0) %>%
  arrange(trade, desc(class), reorder(gender, desc(count)), reorder(race, desc(count))) %>%
  mutate(percent = count / total) %>%
  select(trade, class, gender, race, count, total, percent, allow_2) %>%
  rename(ratio = allow_2) %>%
  mutate(denom = as.integer(str_extract(ratio, "[0-9]{1}$")))

# Viz: Classification by Gender and Minority

viz_hw_sxrc_class <- ggplot(hw_wf_class, aes(x = class, y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_grid(reorder(race, desc(count)) ~ .) +
  labs(x = "Class",
       y = "Count",
       fill = "Gender",
       title = "Class by gender and minority status",
       subtitle = "I-690",
       caption = "Source: NYS Department of Transportation")

ggsave(plot = viz_hw_sxrc_class, 
       filename = "hw_sxrc_class_bar.jpg",
       bg = "transparent")

# Table: Classification by Gender and Minority

tbl_hw_sxrc_class <- hw_wf_class %>%
  arrange(desc(class), trade, reorder(gender, desc(count)), reorder(race, desc(count))) %>%
  select(trade:count, percent, ratio, actual, potent, perc_pot) %>%
  mutate(perc_pot = percent(perc_pot, accuracy = 0.01),
         percent = percent(percent, accuracy = 0.01),
         actual = number(actual, accuracy = 0.1),
         actual = as.character(actual),
         actual = str_replace(actual, pattern = ".0$", ""),
         actual = paste0("1:", actual)) %>%
  rename(Trade = trade,
         Class = class,
         Gender = gender,
         Race = race,
         Workers = count,
         "Workers (%)" = percent,
         "Allowed Trade Ratio" = ratio,
         "Actual Trade Ratio" = actual,
         "Potential Trade Apprentices" = potent,
         "Apprentice Potential (%)" = perc_pot)

index <- which(tbl_hw_sxrc_class$`Apprentice Potential (%)` == "NA%")
tbl_hw_sxrc_class$`Apprentice Potential (%)`[index] <- NA

index <- which(tbl_hw_sxrc_class$`Actual Trade Ratio` == "1:NA")
tbl_hw_sxrc_class$`Actual Trade Ratio`[index] <- NA

write_csv(tbl_hw_sxrc_class, "hw_sxrc_class_tbl.csv")

# Viz: Workhours by Classification and Race

viz_hw_sxrc_hrs_class <- ggplot(hw_hr_class, aes(x = class, y = count, fill = race)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(x = "Class",
       y = "Hours",
       fill = "Status",
       title = "Hours by minority status and class",
       subtitle = "I-690",
       caption = "Source: NYS Department of Transportation")

ggsave(plot = viz_hw_sxrc_hrs_class, 
       filename = "hw_sxrc_hrs_class_bar.jpg",
       bg = "transparent")

# Table: Classification by Trades

tbl_hw_wf_class <- hw_wf_class %>%
  group_by(trade, class) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         percent = count / total) %>%
  select(-total) %>%
  arrange(desc(class), trade) %>%
  mutate(percent = percent(percent, accuracy = 0.01)) %>%
  rename(Trade = trade,
         Class = class,
         Workers = count,
         "Workers (%)" = percent)

tmp <- tbl_hw_sxrc_class %>% 
  select(Trade, Class, `Allowed Trade Ratio`:`Apprentice Potential (%)`) %>%
  unique()

tbl_hw_wf_class <- left_join(tbl_hw_wf_class, tmp, by = c("Trade", "Class")) %>%
  arrange(Trade, desc(Class))

write_csv(tbl_hw_wf_class, "hw_wf_class_tbl.csv")

# Viz: Apprenticeship Potential (Prep)

tmp <- hw_wf_class %>%
  select(trade, ratio, actual, potent, perc_pot, denom) %>%
  unique()

hw_wf_class2 <- hw_wf_class %>%
  group_by(trade, class) %>%
  summarize(count = sum(count)) %>%
  ungroup()

tmp <- tmp %>%
  mutate(class = "Potential") %>%
  select(trade, class, potent) %>%
  rename(count = potent)

missing <- data_frame(trade = c("Laborer Unskilled", 
                                "Welders and Cutters", 
                                "Asbestos Workers"),
                      class = "Actual",
                      count = 0)

hw_wf_class2 <- hw_wf_class2 %>%
  filter(class != "Journeyman",
         !is.na(count)) %>%
  mutate(class = "Actual")

hw_wf_class2 <- bind_rows(hw_wf_class2, missing) %>%
  arrange(trade)

tmp <- tmp %>%
  filter(!is.na(count)) %>%
  arrange(trade)

tmp <- bind_cols(hw_wf_class2, tmp) %>%
  mutate(lost = count1 - count) %>%
  select(trade, class, count, trade1, class1, lost)

actual <- tmp %>%
  select(trade:count)

potential <- tmp %>%
  select(trade1:lost) %>%
  rename(trade = trade1,
         class = class1,
         count = lost)

hw_wf_poten <- bind_rows(actual, potential)

viz_hw_wf_poten <- ggplot(hw_wf_poten, 
                          aes(x = reorder(trade, count), y = count, fill = class)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = comma, limits = c(0, 50)) +
  labs(x = "Trade",
       y = "Workers",
       fill = "Status",
       title = "Actual v. potential hired apprentices by trade",
       subtitle = "I-690",
       caption = "Source: NYS Department of Transportation")

ggsave(plot = viz_hw_wf_poten, 
       filename = "hw_wf_poten_bar.jpg",
       bg = "transparent")

# Actual v. Potential Apprenticeship Includign Race

# Here, we want to show proportionality by trade on two levels:

# 1) Proportionality of Minority vs. White workers
# 2) Proportionality of Actual v. Potential Apprentices




