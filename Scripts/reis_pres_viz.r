
# REIS Presentation Visualizations
# 2019-03-12

# Load Packages & Set Theme

library(zoo)
library(readr)
library(tidyr)
library(dplyr)
library(scales)
library(readxl)
library(zipcode)
library(ggplot2)
library(stringr)
library(extrafont)
library(lubridate)
library(gridExtra)
library(kableExtra)

setwd("~/Projects/REIS/Presentation")

theme_set(theme_minimal() + theme(plot.title = element_text(family = "Century Gothic", 
                                                            color = "Black",
                                                            size = 20),
                                  plot.subtitle = element_text(family = "Century Gothic", 
                                                               color = "grey20",
                                                               size = 14),
                                  plot.caption = element_text(family = "Century Gothic", 
                                                              color = "grey50",
                                                              size = 7,
                                                              vjust = -0.6,
                                                              lineheight = 1.2),
                                  axis.title = element_text(family = "Century Gothic", 
                                                            color = "grey0"),
                                  axis.text = element_text(family = "Century Gothic", 
                                                           color = "grey30",
                                                           size = 16),
                                  axis.title.x = element_text(vjust = -0.5,
                                                              size = 14),
                                  legend.title = element_text(family = "Century Gothic", 
                                                              color = "grey0",
                                                              size = 16), 
                                  legend.text = element_text(family = "Century Gothic", 
                                                             color = "grey10",
                                                             size = 14,
                                                             lineheight = 1.2),
                                  strip.text = element_text(family = "Century Gothic", 
                                                            color = "grey10",
                                                            size = 18)))


# FIG. 2


fig2 <- ggplot(viz_hc_rc_con, aes(x = factor(name, levels = c("Longhouse",
                                                                       "Patricia Electric",
                                                                       "Stone Bridge",
                                                                       "Schalk and Son",
                                                                       "Quality Structures")), 
                                           y = workers, fill = race)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  labs(title = "Workers by company and race",
       subtitle = "Hancock Airport",
       x = NULL,
       y = "Workers",
       fill = "Race",
       caption = "Source: Records disclosing race\nSyracuse Regional Airport Authority")

ggsave(plot = fig2,
       filename = "fig2.jpg",
       bg = "transparent",
       width = 10,
       height = 6)

# FIG. 3

fig3 <- ggplot(hc_rc_hrs, aes(x = reorder(race, hours), y = hours)) +
  geom_bar(stat = "identity", fill = "tomato", width = 0.75) +
  coord_flip() +
  scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 20000, 25000),
                     labels = c("0", "5", "10", "15", "20", "25 K")) +
  labs(title = "Total hours by race",
       subtitle = "Hancock Airport",
       x = NULL,
       y = "Hours (K)",
       caption = "Source: Records diclosing race\nSyracuse Regional Airport Authority")

ggsave(plot = fig3,
       filename = "fig3.jpg",
       bg = "transparent",
       width = 10,
       height = 6)

# FIG. 4

fig4 <- ggplot(lv_grs_narm, aes(x = reorder(race, gross), y = gross)) +
  geom_bar(stat = "identity", fill = "tomato", width = .75) +
  coord_flip() +
  scale_y_continuous(breaks = c(0, 200000, 400000, 600000, 800000),
                     labels = c("$ 0", "200", "400", "600", "800 K")) +
  labs(title = "Total gross by race",
       subtitle = "Hancock Airport",
       x = NULL,
       y = "Gross (K)",
       caption = "Source: Records disclosing race\nSyracuse Regional Airport Authority")

ggsave(plot = fig4, 
       filename = "fig4.jpg",
       bg = "transparent",
       width = 10,
       height = 6)

# FIG. 5

fig5 <- ggplot(hc_county_stats, aes(x = reorder(county, workers), y = workers, fill = state)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  labs(title = "Workers by county, state",
       subtitle = "Hancock Airport",
       x = NULL,
       y = "Workers",
       fill = "State",
       caption = "Source: Syracuse Regional Airport Authority; Records disclosing ZIP")

ggsave(plot = fig5,
       filename = "fig5.jpg",
       bg = "transparent",
       height = 7,
       width = 10)

# FIG. 7

fig7 <- ggplot(hc_in_city_stats, aes(x = reorder(race, workers), y = workers, 
                                            fill = factor(within, levels = c("Within", "Outside")))) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  labs(title = "Workers in Syracuse by race",
       subtitle = "Hancock Airport",
       x = NULL,
       y = "Workers",
       fill = "Syracuse",
       caption = "Source: Records disclosing ZIP, race\nSyracuse Regional Airport Authority")

ggsave(plot = fig7,
       filename = "fig7.jpg",
       bg = "transparent",
       width = 10,
       height = 6)

# FIG. 9

fig9 <- ggplot(lv_rc_con, aes(x = reorder(name, name), y = count, fill = race)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  labs(title = "Workers by company and race",
       subtitle = "Lakeview Amphitheater",
       x = NULL,
       y = "Workers",
       fill = "Race",
       caption = "Source: Onondaga County")

ggsave(plot = fig9,
       filename = "fig9.jpg",
       bg = "transparent",
       width = 10,
       height = 6)  

# FIG. 10

fig10 <- ggplot(lv_uniq_wrk_stats2, aes(x = reorder(county_name, count), y = count)) +
  geom_bar(stat = "identity", fill = "tomato", width = 0.75 ) +
  coord_flip() +
  labs(title = "Workers by county",
       subtitle = "Lakeview Amphitheater",
       x = NULL,
       y = "Workers",
       caption = "Source: Onondaga County; Records disclosing ZIP")

ggsave(plot = fig10,
       filename = "fig10.jpg",
       bg = "transparent",
       width = 10,
       height = 8)

# FIG. 12

fig12 <- ggplot(lv_in_syr, 
                        aes(x = reorder(race, workers), 
                            y = workers, 
                            fill = factor(within, levels = c("Within", "Outside")))) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  labs(title = "Workers in Syracuse by race",
       subtitle = "Lakeview Amphetheater",
       x = NULL,
       y = "Workers",
       fill = "Syracuse",
       caption = "Source: Onondaga County")

ggsave(plot = fig12,
       filename = "fig12.jpg",
       bg = "transparent",
       width = 10,
       height = 6)

# FIG. 13

fig13 <- ggplot(viz_expo_rc_hrs, aes(x = reorder(Race, Percent), y = Count)) +
  coord_flip() +
  geom_bar(stat = "identity", fill = "tomato", width = 0.75) +
  scale_y_continuous(limits = c(0, 142500),
                     breaks = c(0,20000, 40000, 60000, 80000, 100000, 120000, 140000),
                     labels = c("0", "20", "40", "60", "80", "100", "120", "140 K")) +
  labs(title = "Total hours worked by race",
       subtitle = "Expo Center",
       x = NULL, 
       y = "Hours (K)", 
       caption = "Source: NYS OGS")

ggsave(plot = fig13, 
       filename = "fig13.jpg",
       bg = "transparent",
       height = 6,
       width = 10)

# FIG. 15

fig15 <- ggplot(viz_expo_rc_grs, aes(x = reorder(Race, Percent), y = Count)) +
  coord_flip() +
  geom_bar(stat = "identity", fill = "tomato", width = 0.75) +
  scale_y_continuous(limits = c(0, 4400000),
                     breaks = c(0, 1000000, 2000000, 3000000, 4000000),
                     labels = c("$ 0", "1", "2", "3", "4 M")) +
  labs(title = "Total gross by race",
       subtitle = "Expo Center",
       x = NULL, 
       y = "Gross (M)", 
       caption = "Source: NYS OGS")

ggsave(plot = fig15, 
       filename = "fig15.jpg",
       bg = "transparent",
       width = 10,
       height = 6)

# FIG. 16

fig16a <- ggplot(tmp_expo_rc_viz, aes(x = Race, y = Percent, fill = reorder(Indicator, Percent))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0", "", "50", "", "100 %")) +
  labs(title = "",
       subtitle = "Scale: 100 %",
       x = NULL,
       y = NULL,
       fill = "Proportion",
       caption = "Source: NYS OGS") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(size = 16,
                                     color = "grey0"))

fig16b <- ggplot(tmp_expo_rc_viz, aes(x = Race, y = Percent, fill = reorder(Indicator, Percent))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.5, 1),
                     labels = c("0 %", "50 %", "100 %")) +
  labs(title = "",
       subtitle = "Scale: 100 %",
       x = NULL,
       y = NULL,
       fill = "Proportion",
       caption = "Source: NYS OGS") +
  coord_flip(ylim = c(0, 0.05)) +
  scale_y_continuous(breaks = c(0, 0.025, 0.05),
                     labels = c("0", "2.5 %", "5 %")) +
  theme(legend.position = "right") +
  scale_fill_manual(values = c(adjustcolor(hue_pal()(2)[1], alpha.f = 1), 
                               adjustcolor(hue_pal()(2)[2], alpha.f = 1)),
                    breaks = c("Hours", "Gross")) +
  labs(title = "Percent of hours and gross by race",
       subtitle = "Scale: 5 %",
       caption = "") +
  theme(legend.position = "none",
        axis.text.y = element_text(family = "Century Schoolbook",
                                   hjust = 1),
        plot.subtitle = element_text(size = 16,
                                     color = "grey0"))

grs_hrs_spec <- arrangeGrob(fig16b, fig16a, ncol = 2)
ggsave(file="fig16.jpg", grs_hrs_spec, bg = "transparent", height = 6, width = 10)

dev.off()

# FIG. 18

fig18 <- ggplot(hw_wf_trades, aes(x = reorder(Trade, Count), y = Count, fill = Race)) +
  coord_flip() +
  geom_bar(stat = "identity", width = 0.75) +
  scale_y_continuous(labels = comma, breaks = c(0, 25, 50, 75, 100)) +
  scale_x_discrete(limits = trades) +
  labs(title = "Workers by trade, minority status",
       subtitle = "I-690",
       x = NULL, 
       y = "Workers",
       fill = "Status",
       caption = "Source: NYS DOT")

ggsave(plot = fig18, 
       filename = "fig18.jpg",
       bg = "transparent",
       width = 10,
       height = 6)

# FIG. 19

fig19 <- ggplot(hw_sxrc_hrs_trade, aes(x = race, y = count, fill = sex)) +
  coord_flip() +
  geom_bar(stat = "identity", width = 0.75) +
  scale_x_discrete(limits = c("Hispanic", "Indigenous", "Black", "White")) +
  scale_y_continuous(limits = c(0, 155000),
                     breaks = c(0, 50000, 100000, 150000),
                     labels = c("0", "50", "100", "150 K")) +
  labs(title = "Total hours by gender and race",
       subtitle = "I-690",
       x = NULL, 
       y = "Hours (K)",
       fill = "Gender",
       caption = "Source: NYS DOT")

ggsave(plot = fig19, 
       filename = "fig19.jpg",
       bg = "transparent",
       width = 10,
       height = 6)

# FIG. 21

fig21 <- ggplot(hw_hr_class1, aes(x = factor(class, levels = c("Apprentice", "Journeyman")), y = count, fill = race)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 166000),
                     breaks = c(0, 40000, 80000, 120000, 160000),
                     labels = c("0", "40", "80", "120", "160 K")) +
  labs(x = NULL,
       y = "Hours (K)",
       fill = "Status",
       title = "Hours by minority status, class",
       subtitle = "I-690",
       caption = "Source: NYS DOT")

ggsave(plot = fig21, 
       filename = "fig21.jpg",
       bg = "transparent",
       width = 10,
       height = 4)

# FIG. 22

index <- which(all_races_viz_tmp$race == "Native")
all_races_viz_tmp[index, "race"] <- "Indigenous"

fig22 <- ggplot(all_races_viz_tmp, aes(x = reorder(race, count), y = count)) +
  geom_bar(stat = "identity", fill = "tomato", width = 0.75) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Workforce composition by race",
       subtitle = "All projects",
       x = NULL,
       y = "Total Workers",
       fill = "Race",
       caption = "Sources: Records disclosing race\nSyracuse Airport Authority, Onondaga County, NYS DOT")

ggsave(plot = fig22, 
       filename = "fig22.jpg",
       bg = "transparent",
       width = 10,
       height = 6)

# FIG. 23

fig23 <- ggplot(all_races_viz_tmp, aes(x = reorder(race, count), y = count)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Workforce composition by race",
       subtitle = "All projects",
       x = NULL,
       y = "Workers",
       fill = "Race",
       caption = "Sources: Records disclosing race\nSyracuse Airport Authority, Onondaga County, NYS DOT") +
  facet_wrap(~ factor(project, levels = c("Lakeview", "I-690", "Hancock")), nrow = 3)

ggsave(plot = fig23, 
       filename = "fig23.jpg",
       bg = "transparent",
       width = 10,
       height = 8)

# FIG. 24

fig24 <- ggplot(all_sxrc3, aes(x = factor(race, 
                                                levels = c("Multiracial",
                                                           "Asian",
                                                           "Hispanic",
                                                           "Indigenous",
                                                           "Black",
                                                           "White"),
                                                labels = c("Multiracial",
                                                           "Asian",
                                                           "Hispanic",
                                                           "Indigenous",
                                                           "Black",
                                                           "White")), 
                                     y = percent, 
                                     fill = sex)) +
  geom_bar(stat = "identity", width = 0.75) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.5, 1),
                     labels = c("0", "50", "100 %")) +
  labs(title = "Worker proportions by race, gender",
       subtitle = "I-690, Lakeview, Hancock",
       x = NULL,
       y = NULL,
       fill = "Gender",
       caption = "Sources: Records disclosing race, gender\nSyracuse Airport Authority, Onondaga County, NYS DOT")

ggsave(plot = fig24, 
       filename = "fig24.jpg",
       bg = "transparent",
       width = 10,
       height = 6)

# FIG. 25

fig25a <- ggplot(gross_conclusion, aes(x = reorder(race, count), y = count)) +
  geom_bar(stat = "identity", fill = "tomato", width = 0.75) +
  coord_flip() +
  scale_y_continuous(breaks = c(0, 5000000, 10000000),
                     labels = c("$ 0", "5", "10 M")) +
  theme(plot.subtitle = element_text(color = "grey10",
                                     size = 16)) +
  labs(title = "All projects",
       subtitle = "Total gross",
       x = NULL,
       y = NULL,
       caption = "\n")

fig25b <- ggplot(hours_conclusion, aes(x = reorder(race, count), y = count)) +
  geom_bar(stat = "identity", fill = "tomato", width = 0.75) +
  coord_flip() +
  scale_y_continuous(breaks = c(0, 100000, 200000, 300000, 400000), 
                     label = c("0 Hrs", "100", "200", "300", "400 K")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.subtitle = element_text(color = "grey10",
                                     size = 16)) +
  labs(title = "",
       subtitle = "Total hours",
       x = NULL,
       y = NULL,
       caption = "Sources: Records disclosing race\nSyracuse Airport Authority, Onondaga County, NYS OGS, NYS DOT")

grs_hrs_spec <- arrangeGrob(fig25a, fig25b, ncol = 2)
ggsave(file="fig25.jpg", grs_hrs_spec, bg = "transparent", width = 12, height = 6)

dev.off()
