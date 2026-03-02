library(tidyverse)
library(here)

load(here("data", "raw_data", "Chris_OpOD_PCCP_2016-2020.rda"))

# Add Age Groups
PCCP_Chris <- OpOD_PCCP |> mutate(Age_Groups = case_when(Age <= 13 ~ "0-13",
                                                        Age >= 14 & Age <= 18 ~ "14-18",
                                                        Age >= 19 & Age <= 24 ~ "19-24",
                                                        Age >= 25 & Age <= 44 ~ "25-44",
                                                        Age >= 45 & Age <= 65 ~ "45-65",
                                                        Age >= 65 ~ "65+"))

# Change Column Values After Discussion With Paramedic
## Missing Destination data (NA) to "Patient Not Transported"
PCCP_Chris$Receiving.Facility.Destination[PCCP_Chris$Receiving.Facility.Destination == ""] <- NA
PCCP_Chris$Receiving.Facility.Destination[is.na(PCCP_Chris$Receiving.Facility.Destination)] <- "Not Transported"

##  Change Other label in pick-up location
PCCP_Chris$PickupLocationDescription[PCCP_Chris$PickupLocationDescription == "Other (Describe in Remarks)"] <- "Other"

# Save Cleaned Data
save(PCCP_Chris,
     file = here("data", "PCCP_Chris.rda"))





