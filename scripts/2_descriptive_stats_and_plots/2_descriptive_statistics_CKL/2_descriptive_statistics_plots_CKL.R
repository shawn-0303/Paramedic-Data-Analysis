library(tidyverse)
library(ggplot2)
library(forcats)
library(here)

load(here("data", "CKL.rda"))

# Plotting Descriptive Statistics (Location, Age, and Gender)

## Pick-Up Location / Sending Facility
CKL_pickup_plot <- CKL |>
  # Plot the percentage of calls from each pick-up location
  ggplot(aes(x = fct_infreq(PickupLocationDescription),
             y = (after_stat(count)) / sum(after_stat(count)))) +
  geom_bar() +
  geom_text(aes(label = paste0(round((after_stat(count))/sum(after_stat(count)) , 3) * 100, "%")),
            stat = "count",
            vjust = -0.2,
            size = 3) +
  labs(title = "CKL Drug Toxicity Calls - Pick-Up Location/Sending Facility",
       y = "Percent of Calls",
       x = "Pick-Up Location/Sending Facility") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

CKL_pickup_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_CKL", "2_descriptive_statistic_plots_CKL", "CKL_pickup_plot.png"),
       width = 11,
       height = 5)

## Destination / Recieving Facility
CKL_destination_plot <- ggplot(CKL,
                               aes(x = fct_infreq(`Receiving Facility/Destination`),
                                   y = after_stat(count) / sum(after_stat(count)))) +
  geom_bar() +
  geom_text(aes(label = paste0(round(after_stat(count)/sum(after_stat(count)) , 3) * 100, "%")),
            stat = "count",
            vjust = -0.1,
            size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 0.8, 0.1)) +
  scale_x_discrete(labels = label_wrap_gen(22))+
  labs(title = "CKL Drug Toxicity Calls - Destination/Recieving Facility",
       y = "Percent of Calls",
       x = "Destination/Recieving Facility") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

CKL_destination_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_CKL", "2_descriptive_statistic_plots_CKL", "CKL_destination_plot.png"),
       width = 9,
       height = 5)

## Age
CKL_age_plot <- CKL |>
  # Remove the data points that are missing an age
  filter(!is.na(Age)) |>
  ggplot(aes(x = Age)) +
  geom_histogram(binwidth = 4,
                 aes(y = after_stat(count) / sum(after_stat(count)))) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.15)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_continuous(breaks = seq(0, 130, by= 10)) +
  labs(title = "CKL Drug Toxicity Calls - Age Distribution",
       y = "Percentage of Calls")

CKL_age_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_CKL", "2_descriptive_statistic_plots_CKL", "CKL_age_plot.png"),
       width = 7,
       height = 5)

## Gender
CKL_gender_plot <- CKL |>
  # Remove the data points that are missing an age
  filter(!is.na(Gender)) |>
  ggplot(aes(x = Gender)) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count)))) +
  geom_text(aes(y = after_stat(count) / sum(after_stat(count)),
                label = paste0(round(after_stat(count) / sum(after_stat(count)) , 3) * 100, "%")),
            stat = "count",
            vjust = -0.1) +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 0.6, 0.1)) +
  scale_x_discrete(label = c("Female", "Male")) +
  labs(title = "CKL Drug Toxicity Calls - Gender",
       y = "Percentage of Calls",
       x = "Gender")

CKL_gender_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_CKL", "2_descriptive_statistic_plots_CKL", "CKL_gender_plot.png"),
       width = 7,
       height = 5)

## Age and Gender
CKL_age_gender_plot <- CKL |>
  # Remove the data points that are missing an gender
  filter(!is.na(Gender)) |>
  # Remove the data points that are missing an age
  filter(!is.na(Age)) |>
  ggplot(aes(x = Gender,
             y = Age)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 130, by = 10)) +
  scale_x_discrete(labels = c("Female", "Male")) +
  labs(title = "CKL Drug Toxicity Calls - Age and Gender")

CKL_age_gender_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_CKL", "2_descriptive_statistic_plots_CKL", "CKL_age_gender_plot.png"),
       width = 7,
       height = 5)









