library(tidyverse)
library(ggplot2)
library(forcats)
library(here)

load(here("data", "HPS.rda"))

# Store the population for the County of Haliburton (2021 Census)
Haliburton_County_pop <- 20571

# Summarize call counts per year
HPS_call_counts_yearly <- HPS |>
  count(Call_Year) |>
  # Set the years 2016 to 2025 as factors
  complete(Call_Year = factor(2016:2025),
           # Fill all empty values with 0
           fill = list(n = 0)) |>
  # Express the yearly call counts as a crude rate per 1000 population
  # Crude Rate (per 1000 population) = (Call Counts / Population) * 1000
  mutate(standardized_per_1000 = (n / Haliburton_County_pop) * 1000)

# Summarize call counts per month
HPS_call_counts_monthly <- HPS |>
  count(Call_Year, Call_Month) |>
  complete(Call_Year = factor(2016:2025),
           Call_Month = factor(1:12),
           fill = list(n = 0)) |>
  # Express the yearly call counts as a crude rate per 1000 population
  # Crude Rate (per 1000 population) = (Call Counts / Population) * 1000
  mutate(standardized_per_1000 = (n / Haliburton_County_pop) * 1000)

## Calls per Year - BARPLOT
HPS_call_per_year_barplot <- ggplot(HPS_call_counts_yearly,
                                    aes(x = Call_Year,
                                        y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n),
            vjust = -0.1,
            size = 3.5) +
  labs(title = "Number of HPS Drug Toxicity Calls per Year",
       x = "Year",
       y = "Number of Calls")

HPS_call_per_year_barplot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_HPS", "2_descriptive_statistic_plots_HPS", "HPS_calls_peryear_barplot.png"),
       width = 9,
       height = 5)

## Calls per Year - LINEPLOT
HPS_call_per_year_lineplot <- ggplot(HPS_call_counts_yearly,
                                     aes(x = Call_Year,
                                         y = n,
                                         group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of HPS Drug Toxicity Calls per Year",
       x = "Year",
       y = "Number of Calls")

HPS_call_per_year_lineplot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_HPS", "2_descriptive_statistic_plots_HPS", "HPS_calls_peryear_lineplot.png"),
       width = 9,
       height = 5)

## Calls per Month - BARPLOT
HPS_call_per_month_barplot <- ggplot(HPS_call_counts_monthly,
                                     aes(x = Call_Month,
                                         y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(Call_Year~.,
             ncol = 5) +
  labs(title = "Number of HPS Drug Toxicity Calls per Month",
       x = "Month",
       y = "Number of Calls")

HPS_call_per_month_barplot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_HPS", "2_descriptive_statistic_plots_HPS", "HPS_calls_permonth_barplot.png"),
       width = 9,
       height = 5)

## Calls per Year - Standardized Crude Plots
HPS_call_per_year_crude_plot <- ggplot(HPS_call_counts_yearly,
                                       aes(x = Call_Year,
                                           y = standardized_per_1000,
                                           group = 1)) +
  geom_line() +
  geom_point() +
  coord_cartesian(ylim = c(0, 2)) +
  labs(title = "Crude Rate of HPS Drug Toxicity Calls per Year",
       x = "Year",
       y = "Number of Calls - Crude Rate (per 1000 Population)")

HPS_call_per_year_crude_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_HPS", "2_descriptive_statistic_plots_HPS", "HPS_calls_peryear_standardized_plot.png"),
       width = 9,
       height = 5)

## Calls per Month - Standardized Crude Plots
HPS_call_per_month_crude_plot <- ggplot(HPS_call_counts_monthly,
                                        aes(x = Call_Month,
                                            y = standardized_per_1000,
                                            group = 1)) +
  geom_line() +
  facet_wrap(Call_Year~.,
             ncol = 5) +
  coord_cartesian(ylim = c(0, 0.3)) +
  labs(title = "Crude Rate of HPS Drug Toxicity Calls per Month",
       x = "Month",
       y = "Number of Calls - Crude Rate (per 1000 Population)")

HPS_call_per_month_crude_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_HPS", "2_descriptive_statistic_plots_HPS", "HPS_calls_permonth_standardized_plot.png"),
       width = 9,
       height = 5)




