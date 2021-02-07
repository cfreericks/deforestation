# FAO RessourceSTAT Export
# Datei enthaelt Flaechenangaben zu Crops und Pastures
# Permanent crops + Temporary crops = CROPAREA
# Permanent meadows and pastures + Temporary meadows and pastures = MEADOWAREA
########################################

# Load data ---------------------------------------------------------------

infile <- list(here("data", "raw", "FAO_crop_area.csv"),
               here("data", "raw", "COUNTRYCODES.csv"),
               here("data", "interim", "FORAREA.CHANGE.csv")
               )
FAOCROPAREA.df <- read.table(infile[[1]]
                            , header = TRUE
                            , sep = "\t"
                            , quote = "\"'"
                            , dec = "."
                            , check.names = TRUE
                            , na.strings = "NA"
)

COUNTRYCODES <- read.table(infile[[2]]
                           , header = TRUE
                           , sep = "\t"
                           , quote = "\"'"
                           , dec = "."
                           , check.names = TRUE
                           , na.strings = "NA"
)

FORAREA.CHANGE <- read.table(infile[[3]]
                             , header = TRUE
                             , sep = "\t"
                             , quote = "\"'"
                             , dec = "."
                             , check.names = TRUE
                             , na.strings = "NA"
)

str(FAOCROPAREA.df)

# Transform data ----------------------------------------------------------

# Matrix mit Laendern als Zeilen und Flaechennutzung als Spalten
FAOCROPAREA.2000 <- with(FAOCROPAREA.df, {
  tapply(X2000, list(countries, item), FUN = sum)
})
FAOCROPAREA.2010 <- with(FAOCROPAREA.df, {
  tapply(X2010, list(countries, item), FUN = sum)
})

# CROPAREA als Summe von Permanent crops + Temporary crops
# NAs werden wie Null behandelt
CROPAREA.2000 <- rowSums(FAOCROPAREA.2000[,c(2,4)], na.rm = TRUE)
CROPAREA.2010 <- rowSums(FAOCROPAREA.2010[,c(2,4)], na.rm = TRUE)

# MEADOWAREA als Summe von Permanent crops + Temporary crops
# NAs werden wie Null behandelt
MEADOWAREA.2000 <- rowSums(FAOCROPAREA.2000[,c(3,5)], na.rm = TRUE)
MEADOWAREA.2010 <- rowSums(FAOCROPAREA.2010[,c(3,5)], na.rm = TRUE)

# Dataframe der o.g. Daten zusammenstellen
FAOCROPSMEADOWS.2000 <- data.frame(
  COUNTRY = rownames(FAOCROPAREA.2000),
  FALLOWLAND = FAOCROPAREA.2000[, 1],
  PERMCROPS = FAOCROPAREA.2000[, 2],
  PERMMEADOW = FAOCROPAREA.2000[, 3],
  TEMPCROPS = FAOCROPAREA.2000[, 4],
  TEMPMEADOW = FAOCROPAREA.2000[, 5],
  CROPAREA.2000 = CROPAREA.2000,
  MEADOWAREA.2000 = MEADOWAREA.2000
)

FAOCROPSMEADOWS.2010 <- data.frame(
  COUNTRY = rownames(FAOCROPAREA.2010),
  FALLOWLAND = FAOCROPAREA.2010[, 1],
  PERMCROPS = FAOCROPAREA.2010[, 2],
  PERMMEADOW = FAOCROPAREA.2010[, 3],
  TEMPCROPS = FAOCROPAREA.2010[, 4],
  TEMPMEADOW = FAOCROPAREA.2010[, 5],
  CROPAREA.2010 = CROPAREA.2010,
  MEADOWAREA.2010 = MEADOWAREA.2010
)

# wenn sowohl PERMCROPS als auch TEMPCROPS NAs sind, dann ist CROPAREA auch NA
FAOCROPSMEADOWS.2000[is.na(FAOCROPSMEADOWS.2000[,3]) & is.na(FAOCROPSMEADOWS.2000[,5]),7] <- NA
FAOCROPSMEADOWS.2010[is.na(FAOCROPSMEADOWS.2010[,3]) & is.na(FAOCROPSMEADOWS.2010[,5]),7] <- NA
# wenn sowohl PERMCMEADOW als auch TEMPMEADOW NAs sind, dann ist MEADOWAREA auch NA
FAOCROPSMEADOWS.2000[is.na(FAOCROPSMEADOWS.2000[,4]) & is.na(FAOCROPSMEADOWS.2000[,6]),8] <- NA
FAOCROPSMEADOWS.2010[is.na(FAOCROPSMEADOWS.2010[,4]) & is.na(FAOCROPSMEADOWS.2010[,6]),8] <- NA

# Datensaetzen zaehlen, bei denen CROPAREA & MEADOWAREA nicht NA sind
table(!(is.na(FAOCROPSMEADOWS.2000$CROPAREA) & is.na(FAOCROPSMEADOWS.2000$MEADOWAREA)))
table(!(is.na(FAOCROPSMEADOWS.2010$CROPAREA) & is.na(FAOCROPSMEADOWS.2010$MEADOWAREA)))
# Datensaetzen zaehlen, bei denen nur CROPAREA nicht NA ist
table(!(is.na(FAOCROPSMEADOWS.2000$CROPAREA)))
table(!(is.na(FAOCROPSMEADOWS.2010$CROPAREA)))
# Datensaetzen zaehlen, bei denen nur MEADOWAREA nicht NA ist
table(!(is.na(FAOCROPSMEADOWS.2000$MEADOWAREA)))
table(!(is.na(FAOCROPSMEADOWS.2010$MEADOWAREA)))


# Calculate summary statistics --------------------------------------------

countries <- unique(FAOCROPAREA.df[,1:2])
countries <-
  data.frame(COUNTRY = countries$countries,
             FAOSTATCODE = countries$country.codes)

FAOCROPSMEADOWS.new <- merge(FAOCROPSMEADOWS.2000[, c(1, 7, 8)], 
                             FAOCROPSMEADOWS.2010[, c(1, 7, 8)], 
                             by = "COUNTRY"
                             )

FAOCROPSMEADOWS.new <- merge(FAOCROPSMEADOWS.new, 
                             countries, 
                             by = "COUNTRY"
                             )

FAOCROPSMEADOWS.new <- merge(FAOCROPSMEADOWS.new, 
                             COUNTRYCODES[,c(1,5)], 
                             by = "FAOSTATCODE", 
                             all.x = TRUE
                             )

table(is.na(FAOCROPSMEADOWS.new$FAOSTATCODE))
FAOCROPSMEADOWS.new[is.na(FAOCROPSMEADOWS.new$ISO3),c(1,2,7)]

# Gesamtflaeche aus FORAREA.CHANGE auslesen
FAOCROPSMEADOWS.new <- merge(FAOCROPSMEADOWS.new, 
                             FORAREA.CHANGE[,c(1,7)], 
                             by = "ISO3"
                             )

summary(FAOCROPSMEADOWS.new[4:8])


# Area usage shares -------------------------------------------------------

CROPAREA.00.rel <-
  (FAOCROPSMEADOWS.new$CROPAREA.2000 / FAOCROPSMEADOWS.new$AREATOT) * 100
summary(CROPAREA.00.rel)

MEADOWAREA.00.rel <-
  (FAOCROPSMEADOWS.new$MEADOWAREA.2000 / FAOCROPSMEADOWS.new$AREATOT) * 100
summary(MEADOWAREA.00.rel)

CROPAREA.10.rel <-
  (FAOCROPSMEADOWS.new$CROPAREA.2010 / FAOCROPSMEADOWS.new$AREATOT) * 100
summary(CROPAREA.10.rel)

MEADOWAREA.10.rel <-
  (FAOCROPSMEADOWS.new$MEADOWAREA.2010 / FAOCROPSMEADOWS.new$AREATOT) * 100
summary(MEADOWAREA.10.rel)

FAOCROPSMEADOWS.new <-
  cbind(
    FAOCROPSMEADOWS.new,
    CROPAREA.00.rel,
    MEADOWAREA.00.rel,
    CROPAREA.10.rel,
    MEADOWAREA.10.rel
  )
summary(FAOCROPSMEADOWS.new[9:12])

# How have the shares changed over time
CROPAREADIF.00.10 <-
  FAOCROPSMEADOWS.new$CROPAREA.10.rel - FAOCROPSMEADOWS.new$CROPAREA.00.rel
summary(CROPAREADIF.00.10)

MEADOWAREADIF.00.10 <-
  FAOCROPSMEADOWS.new$MEADOWAREA.10.rel - FAOCROPSMEADOWS.new$MEADOWAREA.00.rel
summary(MEADOWAREADIF.00.10)

FAOCROPSMEADOWS.new <- cbind(FAOCROPSMEADOWS.new, CROPAREADIF.00.10, MEADOWAREADIF.00.10)

# Save  dataset -----------------------------------------------------------

outfile <- here("data", "interim", "FAOCROPSMEADOWS.csv")
write.table(
  FAOCROPSMEADOWS.new,
  file = outfile,
  row.names = FALSE,
  quote = TRUE,
  sep = "\t",
  dec = ".",
  append = FALSE
)
