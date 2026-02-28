library(readxl)
library(here)

raw_HPS <- read_xlsx(here("data", "raw_data" , "HPS-InTox_Data_Pull.xlsx"),
                     col_names = TRUE)

# Pick up location lat and long added later
raw_HPS_pickup_locations <- read_xlsx(here("data", "raw_data" , "HPS-Lat_long.xlsx"),
                                      col_names = TRUE)

# join datasets
raw_HPS <- raw_HPS |>
           inner_join(raw_HPS_pickup_locations,
                      by = "Call Number/Patient Number")

save(raw_HPS,
     file = here("data", "raw_data", "raw_HPS.rda"))
