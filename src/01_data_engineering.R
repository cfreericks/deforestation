# Runs all data engineering scripts in ./src/dataeng/
# Final dataset which will be used for further analysis goes to ./data/processed


# Get list of all relevant files ------------------------------------------

path = here("src/dataeng/")
pathnames <- list.files(pattern = "[.]R$", path = path, full.names = TRUE)

# Run the data engineering ------------------------------------------------

sapply(pathnames, FUN = source)
