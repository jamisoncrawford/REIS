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


index <- which(hw_wf_poten$trade == "Ironworkers" & hw_wf_poten$count == -5)
hw_wf_poten[index, "count"] <- 0

exceed <- data_frame(trade = "Ironworkers", class = "Exceeding", count = 5)

hw_wf_poten <- bind_rows(hw_wf_poten, exceed)


viz_hw_wf_poten <- ggplot(hw_wf_poten, 
       aes(x = reorder(trade, count), y = count, 
           fill = factor(class, levels = c("Potential", "Exceeding", "Actual")))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = comma, limits = c(0, 50)) +
  labs(x = "Trade",
       y = "Workers",
       fill = "Apprentices",
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

actual <- hw_wf_poten %>%
  filter(class != "Potential") %>%
  group_by(trade) %>%
  summarize(count = sum(count)) %>%
  ungroup()

potent <- hw_wf_poten %>%
  filter(class == "Potential") %>%
  group_by(trade) %>%
  summarize(potent = sum(count)) %>%
  ungroup()

potent[potent$trade == "Ironworkers", "potent"] <- -5

potential <- left_join(actual, potent, by = "trade") %>%
  mutate(potent = count + potent,
         perc = count / potent,
         inverse = 1 - perc)

hw_wf_potent <- hw_wf_class %>%
  filter(class == "Apprentice") %>%
  group_by(trade, race) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  spread(key = race, value = count)

index <- which(is.na(hw_wf_potent$Minority))
hw_wf_potent[index, "Minority"] <- 0

hw_wf_potent <- hw_wf_potent %>%
  mutate(total = Minority + White,
         perc = Minority / total,
         inverse = 1 - perc)

missing <- data_frame(trade = c("Laborer Unskilled", 
                                "Welders and Cutters", 
                                "Asbestos Workers"),
           Minority = 0,
           White = 0,
           total = 0,
           perc = 0,
           inverse = 1)

potential_by_class <- potential %>%
  select(trade, perc, inverse) %>%
  arrange(trade) %>%
  rename(Actual = perc,
         Potential = inverse) %>%
  mutate(Exceeding = Actual - 1) %>%
  gather(key = Status, value = Proportion, Actual, Potential, Exceeding) %>%
  mutate(class = "Hired v. Potential Apprentices")

index <- which(potential_by_class$Status == "Exceeding" & potential_by_class$Proportion < -0.1)
potential_by_class[index, "Proportion"] <- 0

potential_by_race <- bind_rows(hw_wf_potent, missing) %>%
  select(trade, perc, inverse) %>%
  rename(Minority = perc,
         White = inverse) %>%
  arrange(trade) %>%
  gather(key = Status, value = Proportion, Minority, White) %>%
  mutate(class = "White v. Minority Apprentices")

class_potential <- full_join(potential_by_class, potential_by_race) %>%
  filter(Status != "Exceeding")

index <- which(class_potential$trade == "Ironworkers" & class_potential$Status == "Potential")
class_potential[index, "Proportion"] <- 0

index <- which(class_potential$trade == "Ironworkers" & class_potential$Status == "Actual")
class_potential[index, "Proportion"] <- 1

class_potential_focused <- class_potential %>%
  filter(Status != "Actual",
         Status != "White")

index <- which(class_potential_focused$Status == "Minority")
class_potential_focused[index, "Status"] <- "Minority Occupied (%)"

index <- which(class_potential_focused$Status == "Potential")
class_potential_focused[index, "Status"] <- "Unoccupied (%)"

viz_class_potential <- ggplot(class_potential_focused, aes(x = factor(trade,
                                               levels = rev(c("Asbestos Workers",
                                                          "Laborer Unskilled",
                                                          "Welders and Cutters",
                                                          "Laborer Semi-skilled",
                                                          "Equipment Operator",
                                                          "Cement Masons",
                                                          "Carpenters",
                                                          "Piledriver",
                                                          "Ironworkers",
                                                          "Electrician"))), 
                                    y = Proportion, fill = Status)) +
  geom_bar(stat = "identity", alpha = 0.75, position = "dodge") +
  coord_flip() +
  theme(axis.title.y = element_blank()) +
  scale_y_continuous(labels = percent) +
  labs(title = "Minority apprentices v. apprentice vacancies by trade",
       subtitle = "I-690",
       fill = "Position Status",
       x = NULL,
       caption = "Source: NYS Department of Transportation")

ggsave(plot = viz_class_potential, 
       filename = "class_potential_bar.jpg",
       bg = "transparent")


### CONCLUSION VISUALIZATIONS

# Viz: Workforce by Race: Hancock, Lakeview, I-690

viz_wf_race_1 <- ggplot(all_races, aes(x = reorder(project, count), y = count, fill = race)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Workforce composition by race",
       subtitle = "All projects",
       x = "Project",
       y = "Total Workers",
       fill = "Race",
       caption = "Sources: Syracuse Airport Authority, Onondaga County, NYS OGS, NYS DOT")

viz_wf_race_2 <- ggplot(all_races, aes(x = reorder(race, count), y = count)) +
  geom_bar(stat = "identity", fill = "tomato", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Workforce composition by race",
       subtitle = "All projects",
       x = NULL,
       y = "Total Workers",
       fill = "Race",
       caption = "Sources: Syracuse Airport Authority, Onondaga County, NYS OGS, NYS DOT")

viz_wf_race_3 <- ggplot(all_races, aes(x = reorder(race, count), y = count)) +
  geom_bar(stat = "identity", fill = "tomato", alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Workforce composition by race",
       subtitle = "All projects",
       x = NULL,
       y = "Total Workers",
       fill = "Race",
       caption = "Sources: Syracuse Airport Authority, Onondaga County, NYS OGS, NYS DOT") +
  facet_wrap(~ reorder(project, desc(count)), nrow = 4)

ggsave(plot = viz_wf_race_1, 
       filename = "wf_race_bar_1.jpg",
       bg = "transparent")

ggsave(plot = viz_wf_race_2, 
       filename = "wf_race_bar_2.jpg",
       bg = "transparent")

ggsave(plot = viz_wf_race_3, 
       filename = "wf_race_bar_3.jpg",
       bg = "transparent")

# Viz: Percentage of Workers Who Identify Race & Gender

wf_sxrc <- all_sxrc %>%
  select(sex:count) %>%
  group_by(sex, race) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  mutate(total = sum(count),
         perc = count / total)

viz_wf_sxrc <- ggplot(wf_sxrc, aes(x = factor(race, 
                               levels = c("Multiracial",
                                          "Asian",
                                          "Hispanic",
                                          "Native",
                                          "Black",
                                          "White"),
                               labels = c("Multiracial",
                                          "Asian",
                                          "Hispanic",
                                          "Indigenous",
                                          "Black",
                                          "White")), 
                     y = perc, 
                     fill = sex)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = percent) +
  labs(title = "Workforce proportions by gender and race",
       subtitle = "Records disclosing race and gender, all projects",
       x = NULL,
       y = "Workforce (%)",
       fill = "Gender",
       caption = "Source: Syracuse Airport Authority, Onondaga County, NYS OGS, NYS DOT")

ggsave(plot = viz_wf_sxrc, 
       filename = "wf_sxrc_bar.jpg",
       bg = "transparent")

# Viz: Workhours and Wages, Hancock, Lakeview, I-690, Expo by Race

hours_conclusion <- all_hours_race %>%
  select(race, count) %>%
  group_by(race) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  arrange(reorder(race, desc(count))) %>%
  mutate(var = "Hours")

gross_conclusion <- all_gross_race %>%
  select(race, count) %>%
  group_by(race) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  arrange(reorder(race, desc(count))) %>%
  mutate(var = "Gross")

conclusion <- bind_rows(hours_conclusion, gross_conclusion)

options(scipen = 999)

con1 <- ggplot(gross_conclusion, aes(x = reorder(race, count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(label = dollar) +
  labs(title = "Total gross pay and hours by race, all projects",
       subtitle = "Total gross",
       x = NULL,
       y = NULL,
       caption = "")

con_2 <- ggplot(hours_conclusion, aes(x = reorder(race, count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(label = comma) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "",
       subtitle = "Total hours",
       x = NULL,
       y = NULL,
       caption = "Source: Syracuse Airport Authority, Onondaga County, NYS OGS, NYS DOT")

png("conclusion_hrs_grs_spec.jpg", width = 700, height = 350, bg = "transparent")      # Note: Requires graphics device
grid.arrange(con1, con_2, ncol = 2)
dev.off()


### SECTION: "LAKEVIEW"

# Table: Worker Demographics

tbl_lv_dem <- all_sxrc %>%
  filter(project == "Lakeview") %>%
  select(-total, -project) %>%
  mutate(percent = percent(percent, accuracy = 0.01)) %>% 
  arrange(desc(sex), reorder(race, desc(count))) %>%
  rename(Gender = sex,
         Race = race,
         Total = count,
         "Workforce (%)" = percent)

write_csv(tbl_lv_dem, "lv_dem_tbl.csv")

# Viz: Workers by Race & Gender

lv_dem <- all_sxrc %>%
  filter(project == "Lakeview") %>%
  select(-total, -project)

viz_lv_dem <- ggplot(lv_dem, aes(x = reorder(race, count), y = count, fill = sex)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Workforce race and gender",
       subtitle = "Lakeview Amphitheater",
       x = "Race",
       y = "Total",
       fill = "Gender",
       caption = "Source: Onondaga County")

ggsave(plot = viz_lv_dem,
       filename = "lv_dem_bar.jpg", 
       bg = "transparent")

# Viz: Workers by Race & Contractor

lv_rc_con <- lvhc %>%
  select(project:name, zip:ssn, sex:race) %>%
  unique() %>%
  filter(project == "Lakeview",
         !is.na(sex),
         !is.na(race)) %>%
  select(-project) %>%
  group_by(name, race) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count),
         percent = count / total) %>%
  arrange(desc(percent)) %>%
  select(-total) %>%
  mutate(name = factor(name,
                       levels = c("AM Electric",
                                  "Rommel Fence",
                                  "Postler & Jaeckle",
                                  "Atlas Fence",
                                  "Davis Ulmer",
                                  "Eugene Sackett",
                                  "Herbert Darling",
                                  "Murnane Construction",
                                  "Seneca Steel",
                                  "Northeast",
                                  "Burn Bros",
                                  "EJ Construction",
                                  "John Lowery",
                                  "O'Connell Electric")))

viz_lv_con <- ggplot(lv_rc_con, aes(x = reorder(name, name), y = count, fill = race)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Contractors by race",
       subtitle = "Lakeview Amphitheater",
       x = "Company",
       y = "Workers",
       fill = "Race",
       caption = "Source: Onondaga County")

ggsave(plot = viz_lv_con,
       filename = "lv_con_bar.jpg",
       bg = "transparent")  

# Table: Wages by Race & Gender

lv_sxrc_grs <- lvhc %>%
  filter(project == "Lakeview",
         !is.na(sex),
         !is.na(race)) %>%
  group_by(project, name, zip, ssn, sex, race) %>%
  summarize(count = sum(gross)) %>%
  ungroup() %>%
  group_by(sex, race) %>%
  summarize(workers = n(),
            workers = sum(workers, na.rm = TRUE),
            gross = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(wrk_perc = workers / sum(workers),
         grs_perc = gross / sum(gross)) %>%
  select(sex:workers, wrk_perc, gross, grs_perc) %>%
  arrange(desc(sex), desc(gross))

tbl_lv_sxrc_grs <- lv_sxrc_grs %>%
  mutate(wrk_perc = percent(wrk_perc, accuracy = 0.01),
         gross = dollar(gross),
         grs_perc = percent(grs_perc, accuracy = 0.01)) %>%
  rename(Gender = sex,
         Race = race,
         Workers = workers,
         "Workforce (%)" = wrk_perc,
         "Total Gross" = gross,
         "Workforce Gross (%)" = grs_perc)

write_csv(tbl_lv_sxrc_grs, "lv_sxrc_grs_tbl.csv")

# Table: Location by County, Workers, Records, Gross, and Hours

data("counties")
data("zip_codes")

county <- counties %>%
  mutate(fips = paste0(state_fips, county_fips),
         county_name = str_replace(county_name, " County$", ""))

lv_uniq_wrk_stats <- lvhc %>%
  filter(project == "Lakeview") %>%
  group_by(project, name, zip, ssn, sex, race) %>%
  summarize(records = n(),
            gross = sum(gross, na.rm = TRUE),
            hours = sum(hours, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(zip = as.character(zip)) %>%
  left_join(zip_codes, by = "zip") %>%
  mutate(fips = as.character(fips)) %>%
  left_join(county, by = c("fips", "state")) %>%
  select(project:longitude, county_name)

lv_county_stats <- lv_uniq_wrk_stats %>%
  group_by(county_name, state) %>%
  summarize(workers = n(),
            records = sum(records, na.rm = TRUE),
            gross = sum(gross, na.rm = TRUE),
            hours = sum(hours, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(county = county_name) %>%
  arrange(desc(gross)) %>%
  mutate(wrk_prc = workers / sum(workers, na.rm = TRUE),
         rec_prc = records / sum(records, na.rm = TRUE),
         grs_prc = gross / sum(gross, na.rm = TRUE),
         hrs_prc = hours / sum(hours, na.rm = TRUE))

index <- which(is.na(lv_county_stats$county))
lv_county_stats[index, "county"] <- "-"   
lv_county_stats[index, "state"] <- "-"

tbl_lv_county_stats <- lv_county_stats %>%
  mutate(records = number(records, big.mark = ","),
         gross = dollar(gross),
         hours = number(hours, big.mark = ","),
         wrk_prc = percent(wrk_prc, accuracy = 0.01),
         rec_prc = percent(rec_prc, accuracy = 0.01),
         grs_prc = percent(grs_prc, accuracy = 0.01),
         hrs_prc = percent(hrs_prc, accuracy = 0.01)) %>%
  select(county:workers, wrk_prc, records, rec_prc, hours, hrs_prc, gross, grs_prc) %>%
  rename(County = county,
         State = state,
         Workers = workers,
         "Workforce (%)" = wrk_prc,
         Periods = records,
         "Workforce Periods (%)" = rec_prc,
         Hours = hours,
         "Workforce Hours (%)" = hrs_prc,
         Gross = gross,
         "Workforce Gross (%)" = grs_prc)

write_csv(tbl_lv_county_stats, "lv_county_stats_tbl.csv")

# Viz: Map of Worker Density

names(county)[1] <- "county"

county_density <- left_join(lv_county_stats, county, by = c("county", "state")) %>%
  mutate(fips = paste0(state_fips, county_fips)) %>%
  select(-fips_class, -CSA, -CBSA, -population) %>%
  filter(!is.na(state_fips)) %>%
  rename(GEOID = fips)

detach("package:noncensus", unload = TRUE)

ny_counties <- counties(state = "ny", class = "sf")

sf_lv_county_stats <- left_join(ny_counties, county_density)

library(tmap)

index <- which(is.na(sf_lv_county_stats$workers))
sf_lv_county_stats[index, "workers"] <- NA
sf_lv_county_stats <- sf_lv_county_stats %>% rename(Workers = workers)

map_lv_counties <- tm_shape(sf_lv_county_stats) +
  tm_borders(col = "grey65") +
  tm_fill(col = "Workers", colorNA = "grey100", palette = "Blues", textNA = "No Data", style = "jenks") +
  tm_layout(main.title = "Worker origin by county",
            main.title.size = 1.25,
            title.position = c("left", "top"),
            main.title.position = c("left", "top"),
            frame = FALSE,
            legend.position = c("left", "top"),
            legend.text.size = 0.5,
            legend.title.size = 0.75) +
  tm_credits(text = "Sources:\nOnondaga County,\nRecords Disclosing ZIP\nUS Census Bureau",
             align = "left",
             position = c("left", "bottom"), size = 0.5)

tmap_save(map_lv_counties, "lv_county_density_map.jpg", bg = "transparent")

# Viz: Number of Workers by County

index <- which(lv_county_stats$county == "-")
lv_county_stats <- lv_county_stats[-index, ]

viz_lv_wf_orig <- ggplot(lv_county_stats, aes(x = reorder(county, workers), y = workers)) +
         geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Worker origin by county",
       subtitle = "Lakeview Amphitheater",
       x = "County",
       y = "Total Workers",
       caption = "Source: Onondaga County")

ggsave(plot = viz_lv_wf_orig,
       filename = "lv_wf_orig_bar.jpg",
       bg = "transparent")

# Table: Workers Within & Without Syracuse by Race, Lakeview

lv_in_syr <- lv_uniq_wrk_stats %>%
  filter(project == "Lakeview") %>%
  mutate(within = NA)

zips <- c("13204",
          "13290",
          "13208",
          "13203",
          "13206",
          "13202",
          "13207",
          "13205",
          "13210",
          "13224")

for (i in seq_along(lv_in_syr$within)){
  if (!is.na(lv_in_syr$zip[i]) & lv_in_syr$zip[i] %in% zips){
    lv_in_syr$within[i] <- "Within" 
  } else {
    lv_in_syr$within[i] <- "Outside"
  }
}

lv_in_syr <- lv_in_syr %>%
  select(name:race, records, hours, gross, within) %>%
  group_by(within, race) %>%
  summarize(workers = n(),
            recs = sum(records, na.rm = TRUE),
            hours = sum(hours, na.rm = TRUE),
            gross = sum(gross, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(within), desc(workers))

tbl_lv_in_syr <- lv_in_syr %>%
  mutate(wrk_prc = workers / sum(workers, na.rm = TRUE),
         rec_prc = recs / sum(recs, na.rm = TRUE),
         hrs_prc = hours / sum(hours, na.rm = TRUE),
         grs_prc = gross / sum(gross, na.rm = TRUE)) %>%
  select(within:workers, wrk_prc, recs, rec_prc, hours, hrs_prc, gross, grs_prc) %>%
  mutate(wrk_prc = percent(wrk_prc, accuracy = 0.01),
         rec_prc = percent(rec_prc, accuracy = 0.01),
         hrs_prc = percent(hrs_prc, accuracy = 0.01),
         grs_prc = percent(grs_prc, accuracy = 0.01),
         recs = number(recs, big.mark = ","),
         hours = number(hours, big.mark = ","),
         gross = dollar(gross)) %>%
  rename("Syracuse City Limits" = within,
         Race = race,
         Workers = workers,
         "Workforce (%)" = wrk_prc,
         "Pay Periods" = recs,
         "Workforce Periods (%)" = rec_prc,
         Hours = hours,
         "Workforce Hourage (%)" = hrs_prc,
         Gross = gross,
         "Workforce Gross (%)" = grs_prc)

index <- which(is.na(tbl_lv_in_syr$Race))
tbl_lv_in_syr[index, "Race"] <- "-"

write_csv(tbl_lv_in_syr, "lv_in_syr_tbl.csv")

# Viz: Workers Within & Without Syracuse by Race

index <- which(is.na(lv_in_syr$race))
lv_in_syr <- lv_in_syr[-index, ]

viz_lv_in_syr <- ggplot(lv_in_syr, 
       aes(x = reorder(race, workers), 
           y = workers, 
           fill = factor(within, levels = c("Within", "Outside")))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Workers within Syracuse by race",
       subtitle = "Lakeview Amphetheater",
       x = "Race",
       y = "Workers",
       fill = "Location",
       caption = "Source: Onondaga County")

ggsave(plot = viz_lv_in_syr,
       filename = "lv_in_syr_bar.jpg",
       bg = "transparent")


### Hancock International Airport

# Table: Worker Demographics

hc_sxrc <- lvhc %>%
  filter(project == "Hancock") %>%
  select(project:name, zip:ssn, sex:race) %>%
  unique() %>%
  group_by(sex, race) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(total = sum(count, na.rm = TRUE),
         perc = count / total) %>%
  arrange(desc(count)) %>%
  select(-total)

tbl_hc_sxrc <- hc_sxrc %>%
  mutate(perc = percent(perc, accuracy = 0.01)) %>%
  rename(Gender = sex,
         Race = race,
         Workers = count,
         "Workforce (%)" = perc)

write_csv(tbl_hc_sxrc, "hc_sxrc_tbl.csv")

# Viz: Workers by Race

index <- which(is.na(hc_sxrc$sex))
hc_sxrc_narm <- hc_sxrc[-index, ]

viz_hc_sxrc_narm <- ggplot(hc_sxrc_narm, aes(x = reorder(race, count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Unique workers by race",
       subtitle = "Hancock Airport",
       x = "Race",
       y = "Workers",
       caption = "Source: Syracuse Regional Airport Authority; Records Disclosing Race")

ggsave(plot = viz_hc_sxrc_narm, 
       filename = "hc_sxrc_narm_bar.jpg",
       bg = "transparent")

# Table: Unique Workers by Race & Company

hc_rc_con <- lvhc %>%
  filter(project == "Hancock") %>%
  select(project:name, zip:ssn, sex:race) %>%
  unique() %>%
  group_by(name, race) %>%
  summarize(workers = n()) %>%
  ungroup() %>%
  mutate(total = sum(workers, na.rm = TRUE),
         perc = workers / total) %>%
  select(-total) %>%
  arrange(reorder(name, desc(workers)), desc(workers))

tbl_hc_rc_con <- hc_rc_con %>%
  filter(!is.na(race)) %>%
  mutate(total = sum(workers),
         perc = workers / total,
         perc = percent(perc, na.rm = TRUE)) %>%
  select(-total) %>%
  rename(Company = name,
         Race = race,
         Workers = workers,
         "Workforce (%)" = perc)

write_csv(tbl_hc_rc_con, "hc_rc_con_tbl.csv")

# Viz: Unique Workers by Race & Company

viz_hc_rc_con <- hc_rc_con %>%
  filter(!is.na(race)) %>%
  mutate(total = sum(workers),
         perc = workers / total) %>%
  select(-total)

bar_hc_rc_con <- ggplot(viz_hc_rc_con, aes(x = factor(name, levels = c("Longhouse",
                                                      "Patricia Electric",
                                                      "Stone Bridge",
                                                      "Schalk and Son",
                                                      "Quality Structures")), 
                          y = workers, fill = race)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Unique workers by company and race",
       subtitle = "Hancock Airport",
       x = "Company",
       y = "Workers",
       fill = "Race",
       caption = "Source: Syracuse Regional Airport Authority; Records Disclosing Race")

ggsave(plot = bar_hc_rc_con,
       filename = "hc_rc_con_bar.jpg",
       bg = "transparent")

# Table: Worker Hours by Race

hc_rc_hrs <- lvhc %>%
  filter(project == "Hancock") %>%
  group_by(project, name, zip, ssn, sex, race) %>%
  summarize(hours = sum(hours, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(race) %>%
  summarize(hours = sum(hours, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(race)) %>%
  mutate(total = sum(hours, na.rm = TRUE),
         perc = hours / total) %>%
  arrange(desc(hours))

tbl_hc_rc_hrs <- hc_rc_hrs %>%
  mutate(hours = number(hours, big.mark = ","),
         perc = percent(perc, accuracy = 0.01)) %>%
  select(-total) %>%
  rename(Race = race,
         Hours = hours,
         "Workforce Hourage (%)" = perc)

write_csv(tbl_hc_rc_hrs, "hc_rc_hrs_tbl.csv")

# Viz: Worker Hours by Race

viz_hc_rc_hrs <- ggplot(hc_rc_hrs, aes(x = reorder(race, hours), y = hours)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(label = comma) +
  labs(title = "Total hours by race",
       subtitle = "Hancock Airport",
       x = "Race",
       y = "Hours",
       caption = "Source: Syracuse Regional Airport Authority; Records Diclosing Race")

ggsave(plot = viz_hc_rc_hrs,
       filename = "hc_rc_hrs_bars.jpg",
       bg = "transparent")

# Table: Worker Wages by Race, Numbers & Percentages, Hancock

lv_grs <- lvhc %>%
  filter(project == "Hancock") %>%
  group_by(project, name, zip, ssn, sex, race) %>%
  summarize(gross = sum(gross, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(race) %>%
  summarize(gross = sum(gross, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total = sum(gross, na.rm = TRUE),
         perc = gross / total) %>%
  arrange(desc(gross)) %>%
  select(-total)

lv_grs_narm <- lv_grs %>%
  filter(!is.na(race)) %>%
  mutate(total = sum(gross),
         perc = gross / total) %>%
  select(-total)

tbl_lv_grs <- lv_grs_narm %>%
  mutate(total = sum(gross, na.rm = TRUE),
         perc = percent(perc, accuracy = 0.01)) %>%
  select(-total) %>%
  rename(Race = race,
         Gross = gross,
         "Workforce Gross (%)" = perc)

write_csv(tbl_lv_grs, "hc_rc_grs_tbl.csv")

# Viz: Gross by race

lv_grs_narm <- lv_grs %>%
  filter(!is.na(race))

viz_lv_grs <- ggplot(lv_grs_narm, aes(x = reorder(race, gross), y = gross)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(label = dollar) +
  labs(title = "Total gross by race",
       subtitle = "Hancock Airport",
       x = "Race",
       y = "Gross",
       caption = "Source: Syracuse Regional Airport Authority; Records Disclosing Race")

ggsave(plot = viz_lv_grs, 
       filename = "hc_grs_bar.jpg",
       bg = "transparent")

# Viz: Distribution of Records by Gross & Race

lv_rc_grs_dist <- lvhc %>%
  filter(project == "Hancock",
         !is.na(race),
         !is.na(sex))

lv_rc_grs_dist <- ggplot(lv_rc_grs_dist, aes(x = factor(race, 
                                      levels = c("Multiracial",
                                                 "Native",
                                                 "Black",
                                                 "Hispanic",
                                                 "White")), y = gross)) +
  geom_boxplot(alpha = 0,
               col = "grey65") +
  geom_jitter(data = lv_rc_grs_dist, 
              aes(x = reorder(race, gross), y = gross),
              width = 0.2, height = 0, alpha = 0.4, col = "tomato") +
  coord_flip() +
  scale_y_continuous(label = dollar) +
  labs(title = "Distribution of unique gross payments by race",
       subtitle = "Hancock Airport",
       x = "Race",
       y = "Gross",
       caption = "Source: Syracuse Regional Airport Authority; Records Disclosing Race")

ggsave(plot = lv_rc_grs_dist,
       filename = "hc_rc_grs_dist.jpg",
       bg = "transparent")

# Viz: Distribution of Records by Hours & Race

lv_rc_hrs_dist <- ggplot(lv_rc_grs_dist, aes(x = factor(race, 
                                      levels = c("Multiracial",
                                                 "Native",
                                                 "Hispanic",
                                                 "White",
                                                 "Black")), y = hours)) +
  geom_boxplot(alpha = 0,
               col = "grey65") +
  geom_jitter(data = lv_rc_grs_dist, 
              aes(x = reorder(race, hours), y = hours),
              width = 0.2, height = 0, alpha = 0.2, col = "darkorchid1") +
  coord_flip() +
  scale_y_continuous(label = comma) +
  labs(title = "Distribution of unique work period hours by race",
       subtitle = "Hancock Airport",
       x = "Race",
       y = "Hours",
       caption = "Source: Syracuse Regional Airport Authority; Records Disclosing Race")

ggsave(plot = lv_rc_hrs_dist,
       filename = "hc_rc_hrs_dist.jpg",
       bg = "transparent")

# Table: Workforce County Stats

library(noncensus)
library(tigris)

data(zip_codes)
data(counties)

counties <- counties %>%
  mutate(fips = paste0(state_fips, county_fips),
         fips = as.character(fips))

hc_stats <- lvhc %>%
  filter(project == "Hancock") %>%
  group_by(project, name, zip, ssn, sex, race) %>%
  summarize(records = n(),
            hours = sum(hours),
            gross = sum(gross)) %>%
  ungroup() %>%
  mutate(zip = as.character(zip)) %>%
  left_join(zip_codes, by = "zip") %>%
  mutate(fips = as.character(fips)) %>%
  left_join(counties, by = c("fips", "state")) %>%
  select(-project, -name, -latitude, -longitude, 
         -state_fips, -county_fips, - population, 
         -CSA, -CBSA, -fips_class, -city) %>%
  rename(county = county_name) %>%
  mutate(county = str_replace(county, " County$", ""))

tbl_hc_stats <- hc_stats %>%
  group_by(state, county) %>%
  summarize(workers = n(),
            records = sum(records),
            hours = sum(hours),
            gross = sum(gross)) %>%
  ungroup() %>%
  arrange(desc(gross)) %>%
  mutate(wrk_prc = workers / sum(workers),
         rec_prc = records / sum(records),
         hrs_prc = hours / sum(hours),
         grs_prc = gross / sum(gross),
         wrk_prc = percent(wrk_prc, accuracy = 0.01),
         rec_prc = percent(rec_prc, accuracy = 0.01),
         hrs_prc = percent(hrs_prc, accuracy = 0.01),
         grs_prc = percent(grs_prc, accuracy = 0.01),
         hours = number(hours, big.mark = ","),
         gross = dollar(gross)) %>%
  select(county, state, workers, wrk_prc, records, 
         rec_prc, hours, hrs_prc, gross, grs_prc) %>%
  rename(County = county,
         State = state,
         Workers = workers,
         "Workforce (%)" = wrk_prc,
         Periods = records,
         "Workforce Periods (%)" = rec_prc,
         Hours = hours,
         "Workforce Hourage (%)" = hrs_prc,
         Gross = gross,
         "Workforce Gross (%)" = grs_prc)

index <- which(is.na(tbl_hc_stats$County))
tbl_hc_stats[index, "County"] <- "-"
tbl_hc_stats[index, "State"] <- "-"
  
write_csv(tbl_hc_stats, "hc_county_stats_tbl.csv")

# Viz: Number of Workers by County

hc_county_stats <- hc_stats %>%
  group_by(state, county) %>%
  summarize(workers = n()) %>%
  ungroup() %>%
  filter(!is.na(county))

viz_hc_county <- ggplot(hc_county_stats, aes(x = reorder(county, workers), y = workers, fill = state)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Workers by county of origin",
       subtitle = "Hancock Airport",
       x = "County",
       y = "Workers",
       fill = "State",
       caption = "Source: Syracuse Regional Airport Authority; Records Containing ZIP")

ggsave(plot = viz_hc_county,
       filename = "hc_county_bar.jpg",
       bg = "transparent")

# Map: Worker Density by County, Lakeview

library(tigris)
county <- counties(state = c("ny", "pa"), class = "sf")

hc_county_map <- hc_county_stats %>%
  mutate(county_name = paste(county, "County")) %>%
  left_join(counties, by = c("county_name", "state")) %>%
  rename(GEOID = fips)

sf_hc_county <- county %>%
  left_join(hc_county_map, by = "GEOID") %>%
  rename(Workers = workers)

hc_wrk_map <- tm_shape(sf_hc_county) +
  tm_borders(col = "grey65") +
  tm_fill("Workers", colorNA = "grey100", palette = "Blues", textNA = "No Data", style = "jenks") +
  tm_layout(main.title = "Worker origin by county",
            main.title.size = 1.25,
            title.position = c("left", "top"),
            main.title.position = c("left", "top"),
            frame = FALSE,
            legend.position = c("left", "top"),
            legend.text.size = 0.5,
            legend.title.size = 0.75) +
  tm_credits(text = "Sources:\nSyracuse Regional Airport Authority\nRecords Disclosing ZIP\nUS Census Bureau",
             align = "right",
             position = c("right", "bottom"), size = 0.5)

tmap_save(tm = hc_wrk_map, 
          filename = "hc_wrk_map.jpg",
          bg = "transparent")

# Map: Gross by County, Lakeview

hc_county_gross <- hc_stats %>%
  group_by(state, county, fips) %>%
  summarize(gross = sum(gross)) %>%
  ungroup() %>%
  filter(!is.na(county)) %>%
  rename(GEOID = fips)

hc_gross_map <- county %>%
  left_join(hc_county_gross, by = "GEOID") %>%
  rename(Gross = gross)

hc_gross_map <- tm_shape(hc_gross_map) +
  tm_borders(col = "grey65") +
  tm_fill("Gross", colorNA = "grey100", palette = "Blues", textNA = "No Data", style = "kmeans") +
  tm_layout(main.title = "Gross earnings by county",
            main.title.size = 1.25,
            title.position = c("left", "top"),
            main.title.position = c("left", "top"),
            frame = FALSE,
            legend.position = c("left", "top"),
            legend.text.size = 0.5,
            legend.title.size = 0.75) +
  tm_credits(text = "Sources:\nSyracuse Regional Airport Authority\nRecords Disclosing ZIP\nUS Census Bureau",
             align = "right",
             position = c("right", "bottom"), size = 0.5)

tmap_save(tm = hc_gross_map, 
          filename = "hc_gross_map.jpg", 
          bg = "transparent")

# Workers Within & Without Syracuse by Race

zips <- c("13204",
          "13290",
          "13208",
          "13203",
          "13206",
          "13202",
          "13207",
          "13205",
          "13210",
          "13224")

hc_in_city <- lvhc %>%
  filter(project == "Hancock") %>%
  group_by(project, name, zip, ssn, sex, race) %>%
  summarize(records = n(),
            hours = sum(hours),
            gross = sum(gross)) %>%
  ungroup() %>%
  mutate(zip = as.character(zip),
         within = NA)

for (i in seq_along(hc_in_city$within)){
  if (!is.na(hc_in_city$zip[i]) & hc_in_city$zip[i] %in% zips){
    hc_in_city$within[i] <- "Within"
  } else {
    hc_in_city$within[i] <- "Outside"
  }
}

hc_in_city_stats <- hc_in_city %>%
  group_by(within, race) %>%
  summarize(workers = n(),
            records = sum(records),
            hours = sum(hours),
            gross = sum(gross)) %>%
  ungroup() %>%
  arrange(desc(within), desc(workers)) %>%
  mutate(wrk_prc = workers / sum(workers),
         rec_prc = records / sum(records),
         hrs_prc = hours / sum(hours),
         grs_prc = gross / sum(gross)) %>%
  select(within:workers, wrk_prc, records, rec_prc, hours, hrs_prc, gross, grs_prc)

tbl_hc_in_city <- hc_in_city_stats %>%
  mutate(hours = number(hours, big.mark = ","),
         gross = dollar(gross, big.mark = ","),
         wrk_prc = percent(wrk_prc, accuracy = 0.01),
         rec_prc = percent(rec_prc, accuracy = 0.01),
         hrs_prc = percent(hrs_prc, accuracy = 0.01),
         grs_prc = percent(grs_prc, accuracy = 0.01))

names(tbl_hc_in_city) <- names(tbl_lv_in_syr)
index <- which(is.na(tbl_hc_in_city$Race))
tbl_hc_in_city[index, "Race"] <- "-"

write_csv(tbl_hc_in_city, "hc_in_city_tbl.csv")

# Viz: Workers in City, 

index <- which(is.na(hc_in_city_stats$race))
hc_in_city_stats <- hc_in_city_stats[-index, ]

viz_hc_city <- ggplot(hc_in_city_stats, aes(x = reorder(race, workers), y = workers, 
                             fill = factor(within, levels = c("Within", "Outside")))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Workers within Syracuse by race",
       subtitle = "Hancock Airport",
       x = "Race",
       y = "Workers",
       fill = "Location",
       caption = "Source: Syracuse Regional Airport Authority; Records Disclosing ZIP, Race")

ggsave(plot = viz_hc_city,
       filename = "hc_in_city_bar.jpg",
       bg = "transparent")


### Recommended Visualizations & Tables

# Hancock: Unique Records & Gross Distribution by Race

hc_rc_gr_sum <- lv_rc_grs_dist %>%
  group_by(race) %>%
  summarize("1st Quartile" = quantile(gross, na.rm = TRUE)[2], 
            Mean = mean(gross),
            Median = median(gross),
            "3rd Quartile" = quantile(gross, na.rm = TRUE)[4],
            Maximum = max(gross)) %>%
  mutate(`1st Quartile` = dollar(`1st Quartile`, accuracy = 1),
         `3rd Quartile` = dollar(`3rd Quartile`, accuracy = 1),
         Mean = dollar(Mean, accuracy = 1),
         Median = dollar(Median, accuracy = 1),
         Maximum = dollar(Maximum, accuracy = 1)) %>%
  rename(Race = race)

write_csv(hc_rc_gr_sum, "hc_rc_gr_sum_tbl.csv")

# Hancock: Unique Records & Hour Distribution by Race

tbl_hc_rc_hrs_dist <- lv_rc_grs_dist %>%
  group_by(race) %>%
  summarize("1st Quartile" = quantile(hours, na.rm = TRUE)[2], 
            Mean = mean(hours),
            Median = median(hours),
            "3rd Quartile" = quantile(hours, na.rm = TRUE)[4],
            Maximum = max(hours)) %>%
  ungroup() %>%
  arrange(desc(Median)) %>%
  mutate(`1st Quartile` = number(`1st Quartile`, accuracy = 1),
         `3rd Quartile` = number(`3rd Quartile`, accuracy = 1),
         Mean = number(Mean, accuracy = 1),
         Median = number(Median, accuracy = 1),
         Maximum = number(Maximum, accuracy = 1)) %>%
  rename(Race = race)

write_csv(tbl_hc_rc_hrs_dist, "hc_rc_hrs_sum_tbl.csv")

# Lakeview: Hours Distribution by Unique Record

lv_dist <- lvhc %>%
  filter(project == "Lakeview",
         !is.na(race)) %>%
  select(project:race)

viz_lv_rc_hrs_dist <- ggplot(lv_dist, aes(x = factor(race,
                                                     levels = c("White",
                                                                    "Black",
                                                                    "Hispanic",
                                                                    "Asian",
                                                                    "Native")), y = hours)) +
  geom_boxplot(alpha = 0,
               col = "grey65") +
  geom_jitter(data = lv_dist, 
              aes(x = reorder(race, hours), y = hours),
              width = 0.2, height = 0, alpha = 0.4, col = "tomato") +
  coord_flip() +
  labs(title = "Distribution of weekly hours by race",
       subtitle = "Lakeview Amphitheater",
       x = "Race",
       y = "Hours",
       caption = "Source: Onondaga County; Records Disclosing Race")

ggsave(plot = viz_lv_rc_hrs_dist, 
       filename = "lv_rc_hrs_dist.jpg", 
       bg = "transparent")

# Lakeview: Gross Distribution by Unique Record

viz_lv_rc_grs_dist <- ggplot(lv_dist, aes(x = factor(race, 
                               levels = c("Black",
                                          "White",
                                          "Asian",
                                          "Hispanic",
                                          "Native")), y = gross)) +
  geom_boxplot(alpha = 0,
               col = "grey65") +
  geom_jitter(data = lv_dist, 
              aes(x = reorder(race, gross), y = gross),
              width = 0.2, height = 0, alpha = 0.4, col = "tomato") +
  coord_flip() +
  scale_y_continuous(label = dollar) +
  labs(title = "Distribution of weekly gross by race",
       subtitle = "Lakeview Amphitheater",
       x = "Race",
       y = "Gross",
       caption = "Source: Onondaga County; Records Disclosing Race")

ggsave(plot = viz_lv_rc_grs_dist, 
       filename = "lv_rc_grs_dist.jpg", 
       bg = "transparent")

# Lakeview: Distribution of Unique Record Hours by Race, Table

lv_rc_grs_sum <- lvhc %>%
  filter(project == "Lakeview",
         !is.na(race)) %>%
  group_by(race) %>%
  summarize("1st Quartile" = quantile(gross, na.rm = TRUE)[2], 
            Mean = mean(gross, na.rm = TRUE),
            Median = median(gross, na.rm = TRUE),
            "3rd Quartile" = quantile(gross, na.rm = TRUE)[4],
            Maximum = max(gross, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Median)) %>%
  mutate(`1st Quartile` = dollar(`1st Quartile`, accuracy = 1),
         `3rd Quartile` = dollar(`3rd Quartile`, accuracy = 1),
         Mean = dollar(Mean, accuracy = 1),
         Median = dollar(Median, accuracy = 1),
         Maximum = dollar(Maximum, accuracy = 1)) %>%
  rename(Race = race)

write_csv(lv_rc_grs_sum, "lv_rc_grs_sum_tbl.csv")

# Lakeview: Distribution of Unique Record Hours by Race, Table

lv_rc_hrs_sum <- lvhc %>%
  filter(project == "Lakeview",
         !is.na(race)) %>%
  group_by(race) %>%
  summarize("1st Quartile" = quantile(hours, na.rm = TRUE)[2], 
            Mean = mean(hours, na.rm = TRUE),
            Median = median(hours, na.rm = TRUE),
            "3rd Quartile" = quantile(hours, na.rm = TRUE)[4],
            Maximum = max(hours, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Median)) %>%
  mutate(`1st Quartile` = number(`1st Quartile`, accuracy = 1),
         `3rd Quartile` = number(`3rd Quartile`, accuracy = 1),
         Mean = number(Mean, accuracy = 1),
         Median = number(Median, accuracy = 1),
         Maximum = number(Maximum, accuracy = 1)) %>%
  rename(Race = race)

write_csv(lv_rc_hrs_sum, "lv_rc_hrs_sum_tbl.csv")

# All: Distribution of Total Gross by Unique Worker

url <- "https://raw.githubusercontent.com/jamisoncrawford/reis/master/Datasets/690_workforce_summary.csv"

hw_all <- read_csv(url) %>%
  select(project, name, zip, ssn, sex, race, gross, hours)

lv_all <- lvhc %>%
  filter(project == "Lakeview") %>%
  group_by(project, name, zip, ssn, sex, race) %>%
  summarize(gross = sum(gross, na.rm = TRUE),
            hours = sum(hours, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(zip = as.character(zip))
  
hc_all <- lvhc %>%
  filter(project == "Hancock") %>%
  group_by(project, name, zip, ssn, sex, race) %>%
  summarize(gross = sum(gross, na.rm = TRUE),
            hours = sum(hours, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(zip = as.character(zip))

all_wrk_agg <- bind_rows(hw_all, lv_all, hc_all)

index <- which(is.na(all_wrk_agg$race))
all_wrk_agg <- all_wrk_agg[-index, ]

all_grs_dist <- ggplot(all_wrk_agg, aes(x = factor(race, 
                                   levels = rev(c("Hispanic",
                                              "Black",
                                              "Asian",
                                              "White",
                                              "Native",
                                              "Multiracial"))), 
                        y = gross)) +
  geom_boxplot(alpha = 0,
               col = "grey65") +
  geom_jitter(data = all_wrk_agg, 
              aes(x = factor(race, 
                             levels = rev(c("Hispanic",
                                            "Black",
                                            "Asian",
                                            "White",
                                            "Native",
                                            "Multiracial"))), y = gross),
              width = 0.2, height = 0, alpha = 0.4, col = "tomato") +
  coord_flip() +
  ylim(c(0, 150000)) +
  scale_y_continuous(limits = c(0, 150000), breaks = c(0, 25000, 50000, 75000, 100000, 125000, 150000), labels = c("$ 0", "25", "50", "75", "100", "125", "150 K")) +
  labs(title = "Distribution of gross worker earnings by race",
       subtitle = "Scale: $0 - 150K",
       x = "Race",
       y = "Gross",
       caption = "")

all_grs_dist_zoom <- ggplot(all_wrk_agg, aes(x = factor(race, 
                                   levels = rev(c("Hispanic",
                                                  "Black",
                                                  "Asian",
                                                  "White",
                                                  "Native",
                                                  "Multiracial"))), 
                        y = gross)) +
  geom_boxplot(alpha = 0,
               col = "grey65") +
  geom_jitter(data = all_wrk_agg, 
              aes(x = factor(race, 
                             levels = rev(c("Hispanic",
                                            "Black",
                                            "Asian",
                                            "White",
                                            "Native",
                                            "Multiracial"))), y = gross),
              width = 0.2, height = 0, alpha = 0.4, col = "tomato") +
  coord_flip(ylim = c(0, 25000)) +
  scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 20000, 25000),
                     labels = c("$ 0", "5", "10", "15", "20", "25 K")) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "",
       subtitle = "Scale: $0 - 25K",
       y = "",
       caption = "Sources: Syracuse Airport Authority, Onondaga County, NYS OGS, NYS DOT; Records Disclosing Race")

png("all_grs_dist_spec.jpg", width = 700, height = 350, bg = "transparent")      # Note: Requires graphics device
grid.arrange(all_grs_dist, all_grs_dist_zoom, ncol=2)
dev.off()

# Distribution of Total Gross by Unique Worker by Project

proj_grs_dist <- all_grs_dist + 
  facet_grid(factor(project, levels = c("I-690", "Lakeview", "Hancock")) ~ . ) + 
  aes(x = factor(race, levels = c("Multiracial", "Asian", "Hispanic", "Native", "Black", "White"))) +
  geom_boxplot(fill = "transparent", col = "grey75", outlier.alpha = 0) + 
  geom_jitter(col = "tomato", alpha = 0.4, height = 0, width = 0.1) +
  labs(subtitle = "All Projects",
       x = "Race",
       y = "Total Worker Earnings",
       caption = "Sources: Syracuse Airport Authority, Onondaga County, NYS OGS, NYS DOT; Records Disclosing Race")

ggsave(plot = proj_grs_dist,
       filename = "proj_grs_dist.jpg",
       bg = "transparent")