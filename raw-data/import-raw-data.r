
ProjectTemplate::load.project(list(munging = FALSE, data_loading = FALSE))

# Import data from txt/sas ------------------------------------------------

karen <- data.table::fread("./raw-data/karen 539.txt", data.table = FALSE)
cvdeath <- read_sas("./raw-data/karen_cvdeath.sas7bdat")

# Merge -------------------------------------------------------------------

karen <- left_join(karen, cvdeath %>% select(PatID, OUTCOME5, CENSDATE6, TIME6, OUTCOME6), by = "PatID")

# Store as RData in /data folder ------------------------------------------

save(file = "./data/karen.RData", list = c("karen"))
