# FAOSTAT Export - Population
# Datei enthaelt Bevoelkerungszahlen fuer 2000 und 2010
# total, rural, urban und agricultural population
###############################################################################

# Load data ---------------------------------------------------------------

infile <- here("data", "raw", "Population.csv")
POPULATION.df <- read.table(infile
                             , header = TRUE
                             , sep = "\t"
                             , quote = "\"'"
                             , dec = "."
                             , check.names = TRUE
)
str(POPULATION.df)
names(POPULATION.df) <- c("COUNTRY", "COUNTRYCODE", "POP", "POPCODE", "YEAR", 
                          "TOTPOP", "RURPOP", "URBPOP", "AGRIPOP")
head(POPULATION.df)

# Transform data ----------------------------------------------------------

# Alle Datensaetze fuer das Jahr 2000 auswaehlen und speichern
# Alle Datensaetze fuer das Jahr 2010 auswaehlen und speichern
# beiden Datensaetze anschliessend mergen (all.x = TRUE setzen)
# pruefen, ob merge zu NAs gefuehrt hat -- liegt an Staatenumbildung
# z.B. Serbien und Montenegro
POPULATION.2000 <- subset(POPULATION.df, YEAR == 2000)
TOTPOP.2000 <-
  data.frame(COUNTRY = POPULATION.2000$COUNTRY, 
             TOTPOP.2000 = POPULATION.2000$TOTPOP)

POPULATION.2010 <- subset(POPULATION.df, YEAR == 2010)
TOTPOP.2010 <-
  data.frame(COUNTRY = POPULATION.2010$COUNTRY, 
             TOTPOP.2010 = POPULATION.2010$TOTPOP)

POPDIF.00.10.df <- merge(TOTPOP.2010, TOTPOP.2000, by = "COUNTRY", all.x = TRUE)

POPDIF.00.10.df[(is.na(POPDIF.00.10.df[,3])),]


# Change in population ----------------------------------------------------

# Absolute und relative Aenderung der Gesamtbevoelkerung je Land ermitteln
# und Datensaetze anschliessend zusammenfuehren
POPDIF.00.10 <- POPDIF.00.10.df$TOTPOP.2010 - POPDIF.00.10.df$TOTPOP.2000
POPDIF.00.10.rel <- POPDIF.00.10/POPDIF.00.10.df$TOTPOP.2000 * 100
POPDIF.00.10.df <-
  data.frame(POPDIF.00.10.df,
             POPDIF.00.10 = POPDIF.00.10,
             POPDIF.00.10.rel = POPDIF.00.10.rel)

POPULATION.new <- POPDIF.00.10.df[,c(1,2,4,5)]
POPULATION.new <- merge(POPULATION.new, POPULATION.2010, by = "COUNTRY")
POPULATION.new <- POPULATION.new[,c(1,5,2,3,4,10:12)]

POPULATION.new <- merge(POPULATION.new, POPULATION.2000, by = "COUNTRY")
RURPOPDIF.00.10.rel <-
  ((POPULATION.new$RURPOP.x - POPULATION.new$RURPOP.y) / 
     POPULATION.new$RURPOP.y) * 100

URBPOPDIF.00.10.rel <-
  ((POPULATION.new$URBPOP.x - POPULATION.new$URBPOP.y) / 
     POPULATION.new$URBPOP.y) * 100

AGRIPOPDIF.00.10.rel <-
  ((POPULATION.new$AGRIPOP.x - POPULATION.new$AGRIPOP.y) / 
     POPULATION.new$AGRIPOP.y) * 100

POPULATION.new <- data.frame(POPULATION.new[,1:5]
                             , RURPOPDIF.00.10.rel = RURPOPDIF.00.10.rel
                             , URBPOPDIF.00.10.rel = URBPOPDIF.00.10.rel
                             , AGRIPOPDIF.00.10.rel = AGRIPOPDIF.00.10.rel
                             )

names(POPULATION.new)[2] <- "COUNTRYCODE"

# Save  dataset -----------------------------------------------------------

outfile <- here("data", "interim", "POPULATION_edt.csv")
write.table(
  POPULATION.new,
  file = outfile,
  row.names = FALSE,
  quote = TRUE,
  sep = "\t",
  dec = ".",
  append = FALSE
)
