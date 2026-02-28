library(tidyverse)
library(ggplot2)
library(here)

# load CKL data
load(here("data", "CKL.rda"))

# Load HPS data
load(here("data", "HPS.rda"))

# Make a combined dataset for comparison
combined_data <- bind_rows("City of Kawartha Lakes (n = 1080) " = CKL,
                           "Haliburton (n = 43)" = HPS,
                           .id = "Source") |>
                 select(Source, Gender, Age_Groups) |>
                 filter(!is.na(Age_Groups) & !is.na(Gender))

# Age and Gender comparisons
Age_Gender_plot <- ggplot(combined_data,
                          aes(x = Gender,
                              fill = Age_Groups)) +
  geom_bar(aes(y = after_stat(count) / tapply(after_stat(count), after_stat(PANEL), sum)[after_stat(PANEL)]),
           position = "stack") +
  facet_wrap(~Source) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.6),
                     breaks = seq(0, 1, 0.1)) +
  scale_x_discrete(label = c("Female", "Male")) +
  scale_fill_viridis_d(option = "H") +
  labs(title = "Percentage of Calls by Paramedic Service, Gender, and Age Group",
       y = "Percentage of Calls",
       x = "Gender",
       fill = "Age Groups")

Age_Gender_plot


ggsave(file = here("scripts", "2_descriptive_stats_and_plots", "2_descriptive_statistic_plots_combined", "gender_w_agegroup_combined_plot.png"),
       width = 8,
       height = 5)
