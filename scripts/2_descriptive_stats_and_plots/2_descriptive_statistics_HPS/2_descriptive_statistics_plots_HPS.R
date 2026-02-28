library(tidyverse)
library(ggplot2)
library(forcats)
library(here)

load(here("data", "HPS.rda"))

# Plotting Descriptive Statistics (Location, Age, and Gender)

## Pick-Up Location / Sending Facility
HPS_pickup_plot <- ggplot(HPS,
                          aes(x = fct_infreq(PickupLocationDescription),
                              y = (after_stat(count)) / sum(after_stat(count)))) +
  geom_bar() +
  geom_text(aes(label = paste0(round((after_stat(count))/sum(after_stat(count)) , 3) * 100, "%")),
            stat = "count",
            vjust = -0.1,
            size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 0.65, 0.1))+
  scale_x_discrete(labels = c("House/Town House" = "House/ \nTown House",
                              "Hospital (Acute & Non-Acute)" = "Hospital (Acute \n& Non-Acute)",
                              "Apartment/Condo. Building" = "Apartment/ \nCondo. Building",
                              "Street/Highway/Road" = "Street/Highway/ \nRoad",
                              "Long-Term Care Home" = "Long-Term \nCare Home",
                              "Other (Describe in Remarks)" = "Other"))+
  labs(title = "HPS Drug Toxicity Calls - Pick-Up Location/Sending Facility",
       y = "Percent of Calls",
       x = "Pick-Up Location/Sending Facility")

HPS_pickup_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_HPS", "2_descriptive_statistic_plots_HPS", "HPS_pickup_plot.png"),
       width = 8,
       height = 5)

## Destination / Recieving Facility
HPS_destination_plot <- ggplot(HPS,
                               aes(x = fct_infreq(`Receiving Facility/Destination`),
                                   y = after_stat(count) / sum(after_stat(count)))) +
  geom_bar() +
  geom_text(aes(label = paste0(round(after_stat(count)/sum(after_stat(count)) , 3) * 100, "%")),
            stat = "count",
            vjust = -0.1,
            size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 0.45, 0.1)) +
  scale_x_discrete(labels = label_wrap_gen(16))+
  labs(title = "HPS Drug Toxicity Calls - Destination/Recieving Facility",
       y = "Percent of Calls",
       x = "Destination/Recieving Facility")

HPS_destination_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_HPS", "2_descriptive_statistic_plots_HPS", "HPS_destination_plot.png"),
       width = 8,
       height = 5)

## Age
HPS_age_plot <- ggplot(HPS,
                       aes(x = Age)) +
  geom_histogram(binwidth = 5,
                 aes(y = after_stat(count) / sum(after_stat(count)))) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.15)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_continuous(breaks = seq(0, 100, by= 10)) +
  labs(title = "HPS Drug Toxicity Calls - Age Distribution",
       y = "Percentage of Calls")

HPS_age_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_HPS", "2_descriptive_statistic_plots_HPS", "HPS_age_plot.png"),
       width = 8,
       height = 5)

## Gender
HPS_gender_plot <- ggplot(HPS,
                          aes(x = Gender)) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)))) +
  geom_text(aes(y = after_stat(count) / sum(after_stat(count)),
                label = paste0(round(after_stat(count) / sum(after_stat(count)) , 3) * 100, "%")),
            stat = "count",
            vjust = -0.1) +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 0.6, 0.1)) +
  scale_x_discrete(label = c("Female", "Male")) +
  labs(title = "HPS Drug Toxicity Calls - Gender",
       y = "Percentage of Calls",
       x = "Gender")

HPS_gender_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_HPS", "2_descriptive_statistic_plots_HPS", "HPS_gender_plot.png"),
       width = 8,
       height = 5)

## Age and Gender
HPS_age_gender_plot <- ggplot(HPS,
                              aes(x = Gender,
                                  y = Age)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_discrete(labels = c("Female", "Male")) +
  labs(title = "HPS Drug Toxicity Calls - Age and Gender")

HPS_age_gender_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_HPS", "2_descriptive_statistic_plots_HPS", "HPS_age_gender_plot.png"),
       width = 7,
       height = 5)









