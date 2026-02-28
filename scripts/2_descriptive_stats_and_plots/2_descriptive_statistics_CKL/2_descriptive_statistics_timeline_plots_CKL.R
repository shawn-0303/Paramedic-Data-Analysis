library(tidyverse)
library(ggplot2)
library(forcats)
library(here)

load(here("data", "CKL.rda"))

# Store the population for the City of Kawartha Lakes (2021 Census)
KawarthaLakes_City_pop <- 79247

# Summarize call counts per year
CKL_call_counts_yearly <- CKL |>
  count(Call_Year) |>
  # Express the yearly call counts as a crude rate per 1000 population
  # Crude Rate (per 1000 population) = (Call Counts / Population) * 1000
  mutate(Call_Year = factor(Call_Year),
         standardized_per_1000 = (n / KawarthaLakes_City_pop) * 1000)

# Summarize call counts per month
CKL_call_counts_monthly <- CKL |>
  count(Call_Year, Call_Month) |>
  complete(Call_Year = factor(2016:2025),
          Call_Month = factor(1:12),
          fill = list(n = 0)) |>
  # Express the yearly call counts as a crude rate per 1000 population
  # Crude Rate (per 1000 population) = (Call Counts / Population) * 1000
  mutate(standardized_per_1000 = ((n / KawarthaLakes_City_pop) * 1000))

## Calls per Year - BARPLOT
CKL_call_per_year_barplot <- ggplot(CKL_call_counts_yearly,
                                    aes(x = Call_Year,
                                        y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n),
            vjust = -0.1,
            size = 3.5) +
  labs(title = "Number of CKL Drug Toxicity Calls per Year",
       x = "Year",
       y = "Number of Calls")

CKL_call_per_year_barplot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_CKL", "2_descriptive_statistic_plots_CKL", "CKL_calls_peryear_barplot.png"),
       width = 9,
       height = 5)

## Calls per Year - LINEPLOT
CKL_call_per_year_lineplot <- ggplot(CKL_call_counts_yearly,
                                     aes(x = Call_Year,
                                         y = n,
                                         group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Number of CKL Drug Toxicity Calls per Year",
       x = "Year",
       y = "Number of Calls")

CKL_call_per_year_lineplot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_CKL", "2_descriptive_statistic_plots_CKL", "CKL_calls_peryear_lineplot.png"),
       width = 9,
       height = 5)

## Calls per Month - BARPLOT
CKL_call_per_month_barplot <- ggplot(CKL_call_counts_monthly,
                                     aes(x = Call_Month,
                                         y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(Call_Year~.,
             ncol = 5) +
  labs(title = "Number of CKL Drug Toxicity Calls per Month",
       x = "Month",
       y = "Number of Calls")

CKL_call_per_month_barplot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_CKL", "2_descriptive_statistic_plots_CKL", "CKL_calls_permonth_barplot.png"),
       width = 11,
       height = 5)

## Calls per Year - Standardized Crude Plots
CKL_call_per_year_crude_plot <- ggplot(CKL_call_counts_yearly,
                                       aes(x = Call_Year,
                                           y = standardized_per_1000,
                                           group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Crude Rate of CKL Drug Toxicity Calls per Year",
       x = "Year",
       y = "Number of Calls - Crude Rate (per 1000 Population)")

CKL_call_per_year_crude_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_CKL", "2_descriptive_statistic_plots_CKL", "CKL_calls_peryear_standardized_plot.png"),
       width = 9,
       height = 5)

## Calls per Month - Standardized Crude Plots
CKL_call_per_month_crude_plot <- ggplot(CKL_call_counts_monthly,
                                        aes(x = Call_Month,
                                            y = standardized_per_1000,
                                            group = 1)) +
  geom_line() +
  facet_wrap(Call_Year~.,
             ncol = 5) +
  labs(title = "Crude Rate of CKL Drug Toxicity Calls per Month",
       x = "Month",
       y = "Number of Calls - Crude Rate (per 1000 Population)")

CKL_call_per_month_crude_plot

ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistics_CKL", "2_descriptive_statistic_plots_CKL", "CKL_calls_permonth_standardized_plot.png"),
       width = 9,
       height = 5)
















