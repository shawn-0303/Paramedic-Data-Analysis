library(readxl)
library(here)

raw_HPS <- read_xlsx(here("data", "raw_data" , "HPS-InTox_Data_Pull.xlsx"),
                     col_names = TRUE)

save(raw_HPS,
     file = here("data", "raw_data", "raw_HPS.rda"))