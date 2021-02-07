# Originaldatei 'Forest_area.csv' ist Export aus FRA-Database (2010)
# Exportiert am 14.7.2013
# Beachten beim einlesen: sowohl Separator als auch Dezimaltrenner ist Komma!
# Enthaelt Informationen zu folgenden Flaechen: Forest, Inland water, 
# Other land, Other wooded land
# Werte fuer folgende Jahre enthalten: 1990, 2000, 2005, 2010
# Daten fuer 233 Laender
################################################################################

# Load libraries ----------------------------------------------------------
library("rworldmap") # fuer Laendertabellen

# Load raw data -----------------------------------------------------------
infile <- here("data", "raw", "Forest_area.csv")
outfile <- here("data", "interim", "Forest_area_new.csv")

FORAREA.df <- read.table(
  infile,
  header = TRUE,
  sep = ",",
  quote = "\"'",
  dec = ",",
  check.names = TRUE
)

# Rename variables --------------------------------------------------------
names(FORAREA.df) <- c("FRACATCODE", "FRACAT", "YEAR", "COUNTRYCODE", "COUNTRY",
                       "VALUE", "FLAG")

# Add ISO3 country codes --------------------------------------------------

infile <- here("data", "raw", "FRA.COUNTRIES.ISO3.txt")
FRA.COUNTRIES.ISO3 <- read.table(
  infile,
  header = TRUE,
  sep = "\t",
  quote = "\"'",
  dec = ".",
  check.names = TRUE
)

# ISO3 Codes dem FRA-Datensatz anfuegen
FORAREA.df.new <- merge(FORAREA.df, FRA.COUNTRIES.ISO3, by = "COUNTRYCODE")

# Neuen FRA-Datensatz nach Laendern sortieren und doppelte Variablen entfernen
FORAREA.df.new <- data.frame(
  ISO3 = FORAREA.df.new$ISO3,
  FRACOUNTRYCODE = FORAREA.df.new$COUNTRYCODE,
  COUNTRY = FORAREA.df.new$COUNTRY.x,
  FRACATCODE = FORAREA.df.new$FRACATCODE,
  FRACAT = FORAREA.df.new$FRACAT,
  YEAR = FORAREA.df.new$YEAR,
  VALUE = FORAREA.df.new$VALUE
)

# FRA-Datensatz nach Laendern sortieren
FORAREA.df.new <- FORAREA.df.new[ order(FORAREA.df.new[,3]), ]

# Save  dataset -----------------------------------------------------------
outfile <- here("data", "interim", "Forest_area_iso3.csv")
write.table(
  FORAREA.df.new,
  file = outfile,
  row.names = FALSE,
  quote = TRUE,
  sep = "\t",
  dec = ".",
  append = FALSE
)