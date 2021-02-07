# FAOSTAT Export - Forestry production and trade 
# UN Stats: Annual Totals Table (ATT) for Imports and Exports 
# Anteil (%) der Holzexporte am Gesamtexportwert
# Daten: http://faostat.fao.org/site/626/DesktopDefault.aspx?PageID=626
# und http://unstats.un.org/unsd/trade/imts/annual%20totals.htm
###############################################################################

# Load data ---------------------------------------------------------------

infile <- list(here("data", "raw", "FORESTTRADE.csv"),
               here("data", "raw", "EXPORT_TOTALS_2000.csv"),
               here("data", "raw", "EXPORT_TOTALS_2010.csv"),
               here("data", "raw", "COUNTRYCODES.csv")
               )

FORESTTRADE.raw <- read.table(infile[[1]]
                              , header = TRUE
                              , sep = "\t"
                              , quote = "\"'"
                              , dec = "."
                              , check.names = TRUE
)
EXPORT_TOTALS_2000.csv <- read.table(infile[[2]]
                              , header = TRUE
                              , sep = "\t"
                              , quote = "\"'"
                              , dec = "."
                              , na.strings = "N/A"
                              , check.names = TRUE
)
EXPORT_TOTALS_2010.csv <- read.table(infile[[3]]
                                     , header = TRUE
                                     , sep = "\t"
                                     , quote = "\"'"
                                     , dec = "."
                                     , na.strings = "N/A"
                                     , check.names = TRUE
)

COUNTRYCODES <- read.table(infile[[4]]
                                , header = TRUE
                                , sep = "\t"
                                , quote = "\"'"
                                , dec = "."
                                , check.names = TRUE
)

# Transform data ----------------------------------------------------------

# ISO3 Codes zu FORESTTRADE.raw hinzufuegen
FORESTTRADE.raw.ISO3 <-
  as.vector(COUNTRYCODES[match(FORESTTRADE.raw$COUNTRYCODE, 
                               COUNTRYCODES$FAOSTATCODE), 1])

FORESTTRADE.raw.new <-
  data.frame(FORESTTRADE.raw, ISO3 = FORESTTRADE.raw.ISO3)

# pruefen, ob alle Laender erfasst sind
table(is.na(FORESTTRADE.raw.new$ISO3))
FORESTTRADE.raw.new[(is.na(FORESTTRADE.raw.new$ISO3)),]

# Exportwert fuer Ind. Roundwood extrahieren
EXVALIRW.2000 <- subset(
  FORESTTRADE.raw.new,
  ITEM == "Industrial Roundwood" & ELEMENT == "Export Value" & YEAR == 2000,
  select = c("ISO3", "COUNTRY", "VALUE")
)

EXVALIRW.2010 <- subset(
  FORESTTRADE.raw.new, 
  ITEM == "Industrial Roundwood" & ELEMENT == "Export Value" & YEAR == 2010, 
  select = c("ISO3", "COUNTRY", "VALUE")
)

# Kumulierte Exportwerte fuer alle Holzprodukte extrahieren
EXVALFOR.2000 <- subset(FORESTTRADE.raw.new, 
                        ELEMENT == "Export Value" & YEAR == 2000)

EXVALFOR.2000 <- aggregate(EXVALFOR.2000$VALUE, 
                           list(ISO3 = EXVALFOR.2000$ISO3), 
                           sum)

EXVALFOR.2010 <- subset(FORESTTRADE.raw.new, 
                        ELEMENT == "Export Value" & YEAR == 2010)

EXVALFOR.2010 <- aggregate(EXVALFOR.2010$VALUE, 
                           list(ISO3 = EXVALFOR.2010$ISO3), 
                           sum)

# Exportwert liegt in 1.000 USD vor --> auf USD umrechnen
EXVALFOR.2000$x <- EXVALFOR.2000$x * 1000
EXVALFOR.2010$x <- EXVALFOR.2010$x * 1000

# ISO3 Codes zu EXPORT_TOTALS_2000.csv hinzufuegen
EXPORT_TOTALS_2000.ISO3 <-
  as.vector(COUNTRYCODES[match(EXPORT_TOTALS_2000.csv$COUNTRYCODE,
                               COUNTRYCODES$UNSDNUMCODE), 1])

EXPORT_TOTALS_2000.new <-
  data.frame(EXPORT_TOTALS_2000.csv, ISO3 = EXPORT_TOTALS_2000.ISO3)

# pruefen, ob alle Laender erfasst sind
table(is.na(EXPORT_TOTALS_2000.new$ISO3))
EXPORT_TOTALS_2000.new[(is.na(EXPORT_TOTALS_2000.new$ISO3)),]

# ISO3 Codes zu EXPORT_TOTALS_2010.csv hinzufuegen
EXPORT_TOTALS_2010.ISO3 <-
  as.vector(COUNTRYCODES[match(EXPORT_TOTALS_2010.csv$COUNTRYCODE,
                               COUNTRYCODES$UNSDNUMCODE), 1])

EXPORT_TOTALS_2010.new <-
  data.frame(EXPORT_TOTALS_2010.csv, ISO3 = EXPORT_TOTALS_2010.ISO3)

# pruefen, ob alle Laender erfasst sind
table(is.na(EXPORT_TOTALS_2010.new$ISO3))
EXPORT_TOTALS_2010.new[(is.na(EXPORT_TOTALS_2010.new$ISO3)),]


# Export share of wood ----------------------------------------------------

# EXPORTANTEIL Holz an Gesamtexportwert 2000
FOREXSHARE.2000 <- merge(EXVALFOR.2000, EXPORT_TOTALS_2000.new, by = "ISO3")
FOREXSHARE.2000$ANNUALTOTAL <- as.numeric(FOREXSHARE.2000$ANNUALTOTAL)

attach(FOREXSHARE.2000)
FOREXSHARE.2000 <- data.frame(
  ISO3 = ISO3,
  COUNTRY = COUNTRY,
  FOREXVAL = x,
  TOTEXVAL = ANNUALTOTAL
)
FOREXSHARE <- with(FOREXSHARE.2000, FOREXVAL / TOTEXVAL * 100)
FOREXSHARE.2000 <- cbind(FOREXSHARE.2000, FOREXSHARE)
detach(FOREXSHARE.2000)
summary(FOREXSHARE.2000)

# -------------------------
# ACHTUNG: DATENFEHLER FUER SOLOMON ISLANDS #
# Item "Industrial Roundwood" und "Roundwood" enthalten die gleichen Werte #
# Exportshare ergibt sich zu 186% # 
# Wert fuer Industrial Roundwood manuell auf 0 gesetzt #
# Fuer 2010 ebenso. Hier allerdings nach Bereinigung immer noch hoeherer 
# Holzexportwert #
# als Gesamtexportwert; daher auf NA gesetzt #
# -------------------------

# EXPORTANTEIL Holz an Gesamtexportwert 2010
FOREXSHARE.2010 <- merge(EXVALFOR.2010, EXPORT_TOTALS_2010.new, by = "ISO3")
FOREXSHARE.2010$ANNUALTOTAL <- as.numeric(FOREXSHARE.2010$ANNUALTOTAL)

attach(FOREXSHARE.2010)
FOREXSHARE.2010 <- data.frame(
  ISO3 = ISO3,
  COUNTRY = COUNTRY,
  FOREXVAL2010 = x,
  TOTEXVAL2010 = ANNUALTOTAL
)
FOREXSHARE <- with(FOREXSHARE.2010, FOREXVAL2010 / TOTEXVAL2010 * 100)
FOREXSHARE.2010 <- cbind(FOREXSHARE.2010, FOREXSHARE)
detach(FOREXSHARE.2010)
summary(FOREXSHARE.2010)


# Plot wood export shares -------------------------------------------------

# Fuer das Jahr 2000
h <- hist(FOREXSHARE.2000$FOREXSHARE
          , br = c(0,1,5,20, 50, 100)
          , freq = TRUE
          , ylab = "Anzahl"
          , xlab = "Anteil in %"
          , main = "Anteil Holzexporte am Gesamtexportwert 2000"
          , cex.main = 0.9, cex.axis = 0.9, cex.lab = 0.9
          , ylim = c(0,140)
          , pty = "s"
          )

mylegend <- paste(
  "n = ", sum(h$counts), "\n", 
  "25%Q = ", format(quantile(FOREXSHARE.2000$FOREXSHARE, 0.25, na.rm = TRUE), 
                    digits = 2, scientific = FALSE), "\n", 
  "MEDIAN = ", format(median(FOREXSHARE.2000$FOREXSHARE, na.rm = TRUE), 
                      digits = 2, scientific = FALSE), "\n", 
  "75%Q = ", format(quantile(FOREXSHARE.2000$FOREXSHARE, 0.75, na.rm = TRUE), 
                    digits = 2, scientific = FALSE), "\n", 
  "MEAN = ", format(mean(FOREXSHARE.2000$FOREXSHARE, na.rm =T), 
                    digits = 2, scientific = FALSE), "\n", 
  "SD = ", format(sd(FOREXSHARE.2000$FOREXSHARE, na.rm = TRUE), 
                  digits = 2, scientific = FALSE), "\n", 
  "VAR.COEFF = ", format(sd(FOREXSHARE.2000$FOREXSHARE, na.rm = TRUE) / 
                           mean(FOREXSHARE.2000$FOREXSHARE, na.rm =T) *100, 
                         digits = 2, scientific = FALSE), " %"
)

legend("top", legend = mylegend, bty = "n", cex = 0.9)

text(h$mids, h$counts + 4, labels = h$counts, cex = 0.9)

# Fuer das Jahr 2010
h <- hist(FOREXSHARE.2010$FOREXSHARE
          , br = c(0,1,5,20, 50, 100)
          , freq = TRUE
          , ylab = "Anzahl"
          , xlab = "Anteil in %"
          , main = "Anteil Holzexporte am Gesamtexportwert 2010"
          , cex.main = 0.9, cex.axis = 0.9, cex.lab = 0.9
          , ylim = c(0,150)
          , pty = "s"
)

mylegend <- paste(
  "n = ", sum(h$counts), "\n", 
  "25%Q = ", format(quantile(FOREXSHARE.2010$FOREXSHARE, 0.25, na.rm = TRUE), 
                    digits = 2, scientific = FALSE), "\n", 
  "MEDIAN = ", format(median(FOREXSHARE.2010$FOREXSHARE, na.rm = TRUE), 
                      digits = 2, scientific = FALSE), "\n", 
  "75%Q = ", format(quantile(FOREXSHARE.2010$FOREXSHARE, 0.75, na.rm = TRUE), 
                    digits = 2, scientific = FALSE), "\n", 
  "MEAN = ", format(mean(FOREXSHARE.2010$FOREXSHARE, na.rm =T), 
                    digits = 2, scientific = FALSE), "\n", 
  "SD = ", format(sd(FOREXSHARE.2010$FOREXSHARE, na.rm = TRUE), 
                  digits = 2, scientific = FALSE), "\n", 
  "VAR.COEFF = ", format(sd(FOREXSHARE.2010$FOREXSHARE, na.rm = TRUE) / 
                           mean(FOREXSHARE.2010$FOREXSHARE, na.rm =T) *100, 
                         digits = 2, scientific = FALSE), " %"
)
  
legend("top", legend = mylegend, bty= "n", cex = 0.9)

text(h$mids, h$counts+4, labels = h$counts, cex = 0.9)


# Merge data and save -----------------------------------------------------

FOREXSHARE.df <-
  merge(FOREXSHARE.2010,
        FOREXSHARE.2000,
        by = "ISO3",
        all.x = TRUE)

FOREXSHARE.df <- data.frame(ISO3 = FOREXSHARE.df$ISO3
                            , COUNTRY = FOREXSHARE.df$COUNTRY.x                            
                            , FOREXVAL2000 = FOREXSHARE.df$FOREXVAL
                            , TOTEXVAL2000 = FOREXSHARE.df$TOTEXVAL
                            , FOREXSHARE2000 = FOREXSHARE.df$FOREXSHARE.y
                            , FOREXVAL2010 = FOREXSHARE.df$FOREXVAL2010
                            , TOTEXVAL2010 = FOREXSHARE.df$TOTEXVAL2010
                            , FOREXSHARE2010 = FOREXSHARE.df$FOREXSHARE.x
                            )

FOREXSHARE.df$FOREXSHAREDIF.00.10 <-
  FOREXSHARE.df$FOREXSHARE2010 - FOREXSHARE.df$FOREXSHARE2000

outfile <- here("data", "interim", "FOREXSHARE.csv")
write.table(
  FOREXSHARE.df,
  file = outfile,
  row.names = FALSE,
  quote = TRUE,
  sep = "\t",
  dec = ".",
  append = FALSE
)
