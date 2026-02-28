library(readxl)
library(here)

raw_CKL <- read_xlsx(here("data", "raw_data" , "CKL-Intox_Data_Pull.xlsx"),
                     col_names = TRUE)

save(raw_CKL,
     file = here("data", "raw_data", "raw_CKL.rda"))
