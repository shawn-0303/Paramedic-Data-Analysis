library(tidyverse)
library(ggplot2)
library(forcats)
library(here)
library(vcd)
library(RColorBrewer)

load(here("data", "HPS.rda"))

# Plotting Descriptive Statistics (Location, Age, and Gender)

## Pick-Up Location / Sending Facility
HPS_age_groups_pickup_plot <- ggplot(HPS,
                                     aes(x = fct_infreq(PickupLocationDescription),
                                         y = (after_stat(count)) / sum(after_stat(count)),
                                         fill = Age_Groups)) +
  geom_bar(position = "stack") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 0.6, 0.05))+
  scale_x_discrete(labels = c("House/Town House" = "House/ \nTown House",
                              "Hospital (Acute & Non-Acute)" = "Hospital (Acute \n& Non-Acute)",
                              "Apartment/Condo. Building" = "Apartment/ \nCondo. Building",
                              "Street/Highway/Road" = "Street/Highway/ \nRoad",
                              "Long-Term Care Home" = "Long-Term \nCare Home",
                              "Other (Describe in Remarks)" = "Other"))+
  labs(title = "HPS Drug Toxicity Calls - Pick-Up Location/Sending Facility by Age Group",
       y = "Percent of Calls",
       x = "Pick-Up Location/Sending Facility",
       fill = "Age Groups")

HPS_age_groups_pickup_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_HPS", "2_descriptive_statistic_plots_agegroups_HPS", "HPS_pickup_plot_w_agegroup.png"),
       width = 8,
       height = 5)

## Destination / Recieving Facility
HPS_age_groups_destination_plot <- ggplot(HPS,
                                          aes(x = fct_infreq(`Receiving Facility/Destination`),
                                              y = after_stat(count) / sum(after_stat(count)),
                                              fill = Age_Groups)) +
  geom_bar(position = "stack") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 0.65, 0.05)) +
  scale_x_discrete(labels = label_wrap_gen(22))+
  labs(title = "HPS Drug Toxicity Calls - Destination/Recieving Facility  by Age Group",
       y = "Percent of Calls",
       x = "Destination/Recieving Facility",
       fill = "Age Groups") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

HPS_age_groups_destination_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_HPS", "2_descriptive_statistic_plots_agegroups_HPS", "HPS_destination_plot_w_agegroup.png"),
       width = 8,
       height = 5)

## Gender
HPS_age_group_gender_plot <- ggplot(HPS,
                                    aes(x = Gender,
                                        fill = Age_Groups)) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count))),
           position = "stack") +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 0.6, 0.05)) +
  scale_x_discrete(label = c("Female", "Male")) +
  labs(title = "HPS Drug Toxicity Calls - Gender by Age Group",
       y = "Percentage of Calls",
       x = "Gender",
       fill = "Age Groups")

HPS_age_group_gender_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_HPS", "2_descriptive_statistic_plots_agegroups_HPS", "HPS_gender_w_agegroup_plot.png"),
       width = 8,
       height = 5)

## Age and Gender Mosaic
agegroup_gender_mosaic <- mosaic(~ Gender + Age_Groups, data = HPS,
                                 highlighting = "Age_Groups",
                                 highlighting_fill=brewer.pal(6, "Accent")[2:6],
                                 gp_labels = gpar(fontsize = 10),
                                 labeling_args = list(set_varnames = list(Age_Groups = "Age Groups",
                                                                          Gender = "Gender")))



png(filename = here("scripts", "2_descriptive_stats_and_plots",  "2_descriptive_statistics_HPS", "2_descriptive_statistic_plots_agegroups_HPS", "HPS_agegroup_gender_mosaic.png"),
    width = 8,
    height = 5,
    units = "in",
    res = 300)

## Calls per Year - Age groups
HPS_call_per_year_w_age_group_barplot <- ggplot(HPS,
                                                aes(x = factor(Call_Year, levels = 2016:2025),
                                                    fill = Age_Groups)) +
  geom_bar(position = "stack") +
  scale_x_discrete(drop = FALSE) +
  labs(title = "Number of HPS Drug Toxicity Calls per Year by Age Group",
       x = "Year",
       y = "Number of Calls",
       fill = "Age Groups")

HPS_call_per_year_w_age_group_barplot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots","2_descriptive_statistics_HPS", "2_descriptive_statistic_plots_agegroups_HPS", "HPS_calls_peryear_w_agegroup_barplot.png"),
       width = 9,
       height = 5)

## Calls per Month - Age groups
HPS_call_per_month_w_age_group_barplot <- ggplot(HPS,
                                                 aes(x = Call_Month,
                                                     fill = Age_Groups)) +
  geom_bar(position = "stack") +
  facet_wrap(factor(Call_Year, levels = 2016:2025)~.,
             drop = FALSE,
             ncol = 5) +
  scale_y_continuous(breaks = seq(0, 5, 1)) +
  labs(title = "Number of HPS Drug Toxicity Calls per Month by Age Group",
       x = "Month",
       y = "Number of Calls",
       fill = "Age Groups")

HPS_call_per_month_w_age_group_barplot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_HPS", "2_descriptive_statistic_plots_agegroups_HPS", "HPS_calls_permonth_w_agegroup_barplot.png"),
       width = 11,
       height = 5)








