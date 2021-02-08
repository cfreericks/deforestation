# This master script is responsible for launching and orchestrating the
# functions defined in the src folder


# Load libraries ----------------------------------------------------------

library(here)


# Load scripts ------------------------------------------------------------

# "01_data_engineering.R", 
# "02_factoranalysis.R", 
# "03_clusterBeschreiben.R", 
# "04_regressionanalysis.R"

path = here("src/")
pathnames <- list.files(pattern = "[.]R$", path = path, full.names = TRUE)

sapply(pathnames, FUN = source)