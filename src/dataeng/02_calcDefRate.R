# Der FRA-Datensatz enthaelt die kategoriale Variable (Factor) 'FRACAT'
# steht fuer FRA Category und bezeichnet verschiedene Flaechentypen:
# Forest, Inland water, Other land, Other wooded land
# Werte fuer Jahre: 1990, 2000, 2005, 2010
# Daten fuer 233 Laender

# Es sollen folgende Variablen errechnet werden:
# 'DiFF.90.00' Aenderung der Waldflaeche (absolut) 1990-2000
# 'DiFF.00.10' Aenderung der Waldflaeche (absolut) 2000-2010
# 'ANDiFF.90.00' Jaehrlich durchschnittl. Aenderung der Waldflaeche (absolut) 
# 1990-2000
# 'ANDiFF.00.10' Jaehrlich durchschnittl. Aenderung der Waldflaeche (absolut) 
# 2000-2010 
# 'ANDiFFREL.90.00' Jaehrlich durchschnittl. Aenderungsrate der Waldflaeche 
# (prozentual) 1990-2000
# 'ANDiFFREL.00.10' Jaehrlich durchschnittl. Aenderungsrate der Waldflaeche 
# (prozentual) 2000-2010
# 'FORAREAREL.10' Anteil Waldflaeche an Gesamtflaeche des Landes (prozentual) 
# 2010
################################################################################

# Load libraries ----------------------------------------------------------

library("rworldmap")
library("sqldf")
library("RColorBrewer")

# Load data ---------------------------------------------------------------

infile <- here("data", "interim", "Forest_area_iso3.csv")
FORAREA.df.new <- read.table(
  infile,
  header = TRUE,
  sep = "\t",
  quote = "\"'",
  dec = ".",
  check.names = TRUE
)

# Extract forest area -----------------------------------------------------

# Variable 'FRACATCODE' enthaelt numerisch kodiert die Flaechentypen
# fuer automat. erstellen der subsets Variable in Faktor umwandeln
FORAREA.df.new$FRACATCODE <- as.factor(FORAREA.df.new$FRACATCODE)
str(FORAREA.df.new)

# subset, welches nur die Forstflaeche (in 1000ha) enthaelt, erstellen
# anschliessend dataframe fuer jedes Jahr (1990, 2000, 2005, 2010) erstellen
# Forstflaeche als Variable einfuegen und Datensatz mergen
FORAREA.FOREST.df <- subset(FORAREA.df.new, FORAREA.df.new$FRACATCODE == 0)

FORAREA.1990.df <- subset(
  FORAREA.FOREST.df,
  FORAREA.FOREST.df$YEAR == 1990,
  select = c(ISO3, VALUE)
)
names(FORAREA.1990.df)[2] <- "FORAREA90"

FORAREA.2000.df <- subset(
  FORAREA.FOREST.df,
  FORAREA.FOREST.df$YEAR == 2000,
  select = c(ISO3, VALUE)
)
names(FORAREA.2000.df)[2] <- "FORAREA00"

FORAREA.2005.df <- subset(
  FORAREA.FOREST.df,
  FORAREA.FOREST.df$YEAR == 2005,
  select = c(ISO3, VALUE)
)
names(FORAREA.2005.df)[2] <- "FORAREA05"

FORAREA.2010.df <- subset(
  FORAREA.FOREST.df,
  FORAREA.FOREST.df$YEAR == 2010,
  select = c(ISO3, VALUE)
)
names(FORAREA.2010.df)[2] <- "FORAREA10"

# ISO3 Laendercode Datensatz einlesen
FRACOUNTRIES.ISO3.df <- read.table(
  here("data", "raw", "FRA.COUNTRIES.ISO3.txt"),
  header = TRUE,
  sep = "\t",
  quote = "\"'",
  dec = ".",
  check.names = TRUE
)

# Forstflaechen in den jew. Jahren in neuem Datensatz zusammenfuehren
FORAREA.CHANGE.df <- merge(FORAREA.1990.df, FORAREA.2000.df, by = "ISO3")
FORAREA.CHANGE.df <- merge(FORAREA.CHANGE.df, FORAREA.2005.df, by = "ISO3")
FORAREA.CHANGE.df <- merge(FORAREA.CHANGE.df, FORAREA.2010.df, by = "ISO3")
FORAREA.CHANGE.df <- merge(FORAREA.CHANGE.df, FRACOUNTRIES.ISO3.df, by = "ISO3")
FORAREA.CHANGE.df <- FORAREA.CHANGE.df[c(1,6,2:5)]


# Calculate forest area statistics ----------------------------------------

# Ermitteln der Laenderflaechen
# liefert eine Matrix mit den ISO3 codes als Zeilen und den Jahren als Spalten
(COUNTRYAREA <- with(FORAREA.df.new, {
  tapply(VALUE, list(ISO3, YEAR), sum)
}))

# Pruefen, ob groessere Differenzen zwischen den Jahren sind
# Flaeche sollte idealerweise gleich bleiben, also Differenz in etwa null
table(COUNTRYAREA[,3] - COUNTRYAREA[,4])

# Aus der COUNTRYAREA Matrix wird die Flaeche fuer 2010 als richtige gewaehlt
# Dataframe mit Flaeche und ISO3 code erzeugen
COUNTRYAREA <- data.frame(ISO3 = dimnames(COUNTRYAREA)[[1]], 
                          AREATOT = COUNTRYAREA[, 4])

# Datensatz FORAREA.CHANGE mit COUNTRYAREA verbinden
FORAREA.CHANGE.df <- merge(FORAREA.CHANGE.df, COUNTRYAREA, by = "ISO3")
  
# Anteil Wald an Gesamtflaeche ermitteln 'FORAREAREL.10'
FORAREAREL.10 <- (FORAREA.CHANGE.df$FORAREA10 / FORAREA.CHANGE.df$AREATOT) * 100
summary(FORAREAREL.10)
FORAREA.CHANGE.df <- cbind(FORAREA.CHANGE.df, FORAREAREL.10)
FORAREA.CHANGE.df$FORAREAREL.10 <- format(FORAREA.CHANGE.df$FORAREAREL.10,
                                          digits = 2,
                                          scientific = FALSE)

# Calculate deforestation rate --------------------------------------------

# Aenderung der Waldflaeche von 1990-2000 und 2000-2010 bestimmen
# und Aenderungen in Tabelle schreiben
DIFF.90.00 <- FORAREA.CHANGE.df$FORAREA00 - FORAREA.CHANGE.df$FORAREA90
DIFF.00.10 <- FORAREA.CHANGE.df$FORAREA10 - FORAREA.CHANGE.df$FORAREA00
FORAREA.CHANGE.df <- cbind(FORAREA.CHANGE.df, DIFF.90.00, DIFF.00.10)

# jaherliche Aenderung ermitteln
ANDIFF.90.00 <- (FORAREA.CHANGE.df$FORAREA00 - FORAREA.CHANGE.df$FORAREA90)/10
ANDIFF.00.10 <- (FORAREA.CHANGE.df$FORAREA10 - FORAREA.CHANGE.df$FORAREA00)/10
FORAREA.CHANGE.df <- cbind(FORAREA.CHANGE.df, ANDIFF.90.00, ANDIFF.00.10)

# jaherliche Aenderungsrate in % ermitteln
ANDIFF.90.00.rel <- ((ANDIFF.90.00 / FORAREA.CHANGE.df$FORAREA90)*100)
ANDIFF.00.10.rel <- ((ANDIFF.00.10 / FORAREA.CHANGE.df$FORAREA00)*100)
FORAREA.CHANGE.df <- cbind(FORAREA.CHANGE.df,
                           ANDIFF.90.00.rel,
                           ANDIFF.00.10.rel)

# Aenderungsraten =0 setzen, die wegen Div durch null zu NaN gefuehrt haben
FORAREA.CHANGE.df[is.nan(FORAREA.CHANGE.df$ANDIFF.90.00.rel),] 
FORAREA.CHANGE.df[c(25,68,77,128,159,176,186,190,207,221),13:14] <- c(0.0,0.0)

# Aenderungsraten formatieren auf 2 Nachkommastellen
FORAREA.CHANGE.df$ANDIFF.90.00.rel <- format(FORAREA.CHANGE.df$ANDIFF.90.00.rel,
                                             digits = 2)
FORAREA.CHANGE.df$ANDIFF.00.10.rel <- format(FORAREA.CHANGE.df$ANDIFF.00.10.rel,
                                             digits = 2)

# Lagemasse fuer jaehrliche Aenderungsraten ansehen
FORAREA.CHANGE.df$ANDIFF.90.00.rel <- as.numeric(
  FORAREA.CHANGE.df$ANDIFF.90.00.rel,
  digits = 2,
  scientific = FALSE)
FORAREA.CHANGE.df$ANDIFF.00.10.rel <- as.numeric(
  FORAREA.CHANGE.df$ANDIFF.00.10.rel,
  digits = 2,
  scientific = FALSE)
summary(FORAREA.CHANGE.df$ANDIFF.90.00.rel)
summary(FORAREA.CHANGE.df$ANDIFF.00.10.rel)

# Save  dataset -----------------------------------------------------------

outfile <- here("data", "interim", "FORAREA.CHANGE.csv")

write.table(
  FORAREA.CHANGE.df,
  file = outfile,
  row.names = FALSE,
  quote = TRUE,
  sep = "\t",
  dec = ".",
  append = FALSE
)