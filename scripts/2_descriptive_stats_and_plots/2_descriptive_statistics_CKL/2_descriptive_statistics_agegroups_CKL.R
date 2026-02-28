library(tidyverse)
library(ggplot2)
library(forcats)
library(here)

load(here("data", "CKL.rda"))

# Descriptive Statistics (by Age Group)

## Pick-Up Location / Sending Facility
CKL |>
  group_by(PickupLocationDescription, Age_Groups) |>
  count()

## Destination / Recieving Facility
CKL |>
  group_by(`Receiving Facility/Destination`, Age_Groups) |>
  count()

## Gender
CKL |> group_by(Age_Groups, Gender) |> count()

## Age + Gender
CKL |> group_by(Gender, Age_Groups) |>
  drop_na(Gender, Age_Groups) |>
  summarise(count = n(),
            mean_age = mean(Age, na.rm = TRUE),
            median_age = median(Age, na.rm = TRUE),
            min = min(Age, na.rm = TRUE),
            max = max(Age, na.rm = TRUE),
            .groups = "drop")




















