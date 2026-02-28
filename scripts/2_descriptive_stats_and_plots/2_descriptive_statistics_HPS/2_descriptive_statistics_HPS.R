library(tidyverse)
library(dplyr)
library(forcats)
library(here)

load(here("data", "HPS.rda"))

# Descriptive Statistics

## Pick-Up Location / Sending Facility
HPS |>
  group_by(fct_infreq(PickupLocationDescription)) |>
  count()

## Destination / Recieving Facility
HPS |>
  group_by(fct_infreq(`Receiving Facility/Destination`)) |>
  count()

## Age
HPS |> pull(Age) |> summary()

## Gender
HPS |> group_by(Gender) |> count()

## Age + Gender
HPS |> group_by(Gender) |>
  summarise(count = n(),
            mean_age = mean(Age, na.rm = TRUE),
            median_age = median(Age, na.rm = TRUE),
            min = min(Age, na.rm = TRUE),
            max = max(Age, na.rm = TRUE))

## Call Year
HPS |> group_by(Call_Year) |>
  count()


















