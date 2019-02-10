# REIS Final Data Tables & Visualizations

## RStudio Version: 1.1.456
## R Version: 3.5.1
## Windows 10

## Script Version: 1.0
## Updated: 2019-02-09


# CLEAR WORKSPACE; INSTALL/LOAD PACKAGES

rm(list = ls())

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

if(!require(magick)){install.packages("magick")}
if(!require(webshot)){install.packages("webshot")}

library(magick)
library(webshot)

webshot::install_phantomjs()

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
       caption = "Source: Onondaga County")

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
       caption = "Source: Onondaga County")

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
       caption = "Source: Onondaga County")

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
       caption = "Source: Onondaga County")

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
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = percent) +
  labs(title = "Proportion of hours v. gross earnings by race",
       subtitle = "Full Scale: 100%",
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
       subtitle = "Zoom: 5%",
       caption = "Source: Onondaga County") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

png("expo_rc_grshrs_spec.jpg", width = 700, height = 350, bg = "transparent")      # Note: Requires graphics device
grid.arrange(tmp_expo_rc, tmp_expo_rc2, ncol=2)
dev.off()




