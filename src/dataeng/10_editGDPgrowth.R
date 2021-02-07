# Data source:
# The World Bank (2013a). Economic Indicators: GDP growth (based on World Bank 
# national accounts data, and OECD National Accounts data ﬁles). Website. 
# http://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG/; 
# abgerufen am 06.10.2013.
###############################################################################

# Load and transform data -------------------------------------------------

infile <- here("data", "raw", "GDP_growth.csv")
GDP_growth <- read.table(infile
                          , header = TRUE
                          , sep = "\t"
                          , quote = "\"'"
                          , dec = "."
                          , check.names = TRUE
                          , na.strings = c("#NUM!", "#DIV/0!")
)


# Save data ---------------------------------------------------------------
outfile <- here("data", "interim", "GDP_growth_edt.csv")
write.table(
  GDP_growth,
  file = outfile,
  row.names = FALSE,
  quote = TRUE,
  sep = "\t",
  dec = ".",
  append = FALSE
)