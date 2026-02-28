library(tidyverse)
library(dplyr)
library(forcats)
library(here)

load(here("data", "CKL.rda"))

# Descriptive Statistics

## Pick-Up Location / Sending Facility
CKL |>
  group_by(fct_infreq(PickupLocationDescription)) |>
  count()

## Destination / Recieving Facility
CKL |>
  group_by(fct_infreq(`Receiving Facility/Destination`)) |>
  count()

## Age
CKL |> pull(Age) |> summary()

## Gender
CKL |> group_by(Gender) |> count()

## Age + Gender
CKL |> group_by(Gender) |>
  summarise(count = n(),
            mean_age = mean(Age, na.rm = TRUE),
            median_age = median(Age, na.rm = TRUE),
            min = min(Age, na.rm = TRUE),
            max = max(Age, na.rm = TRUE))

## Call Year
CKL |> group_by(Call_Year) |>
  count()


















