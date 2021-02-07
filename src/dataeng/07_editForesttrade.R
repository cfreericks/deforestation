# FAOSTAT Export - Forestry production and trade 
# Products: Roundwood = Industrial Roundwood + Wood Fuel,incl. wood for charcoal
# Data: http://faostat.fao.org/site/626/DesktopDefault.aspx?PageID=626
# Metadata: http://faostat.fao.org/Portals/_Faostat/documents/pdf/
# FAOSTAT-Forestry-def-e.pdf
###############################################################################

# Load data ---------------------------------------------------------------

infile <- here("data", "raw", "FORESTTRADE.csv")
FORESTTRADE.raw <- read.table(infile
                                 , header = TRUE
                                 , sep = "\t"
                                 , quote = "\"'"
                                 , dec = "."
                                 , check.names = TRUE
)

# Transform data ----------------------------------------------------------

str(FORESTTRADE.raw)

# YEAR in Faktor umwandeln, um tapply mit Liste anwenden zu koennen
FORESTTRADE.raw$YEAR <- as.factor(FORESTTRADE.raw$YEAR)

levels(FORESTTRADE.raw$ITEM) <-
  c("INDROUNDWOOD", "ROUNDWOOD", "WOODFUEL")
levels(FORESTTRADE.raw$ELEMENT) <-
  c("EXQUANT", "EXVAL", "IMQUAN", "IMVAL", "PROD")


# Share of fuelwood -------------------------------------------------------

FUELWOOD.2000 <- subset(FORESTTRADE.raw, 
                        ITEM == "WOODFUEL" & ELEMENT == "PROD" & YEAR == 2000, 
                        select = c("COUNTRY", "VALUE"))
ROUNDWOOD.2000 <- subset(FORESTTRADE.raw, 
                         ITEM == "ROUNDWOOD" & ELEMENT == "PROD" & YEAR == 2000, 
                         select = c("COUNTRY", "VALUE"))
FUELWOOD.2010 <- subset(FORESTTRADE.raw, 
                        ITEM == "WOODFUEL" & ELEMENT == "PROD" & YEAR == 2010, 
                        select = c("COUNTRY", "VALUE"))
ROUNDWOOD.2010 <- subset(FORESTTRADE.raw, 
                         ITEM == "ROUNDWOOD" & ELEMENT == "PROD" & YEAR == 2010, 
                         select = c("COUNTRY", "VALUE"))

# zusammenfuehren in neuem data frame
new <- merge(ROUNDWOOD.2000, FUELWOOD.2000, by = "COUNTRY", all.x = TRUE)
names(new)[2:3] <- c("ROUNDWOOD.2000", "FUELWOOD.2000")
new2 <- merge(ROUNDWOOD.2010, FUELWOOD.2010, by = "COUNTRY", all.x = TRUE)
names(new2)[2:3] <- c("ROUNDWOOD.2010", "FUELWOOD.2010")
FUELSHARE.df <- merge(new2, new, by = "COUNTRY", all.x = TRUE)

# auf NA's pruefen
table(is.na(FUELSHARE.df))
(NA.index <-
    unique(unlist(lapply(FUELSHARE.df, function(x)
      which(is.na(x))))))
FUELSHARE.df[NA.index,]

# Datensatz zusammenfuehren
FUELSHARE.df <- data.frame(FUELSHARE.df, 
                           "FUELSHARE.2000" = FUELSHARE.df$FUELWOOD.2000 / 
                             FUELSHARE.df$ROUNDWOOD.2000 * 100,
                           "FUELSHARE.2010" = FUELSHARE.df$FUELWOOD.2010 / 
                             FUELSHARE.df$ROUNDWOOD.2010 * 100
                           )
FUELSHARE.df <- data.frame(FUELSHARE.df, 
                           "FUELSHAREDIF.00.10" = FUELSHARE.df$FUELSHARE.2010 -
                             FUELSHARE.df$FUELSHARE.2000
                           )

# Save  dataset -----------------------------------------------------------

outfile <- here("data", "interim", "FUELSHARE_edt.csv")
write.table(
  FUELSHARE.df,
  file = outfile,
  row.names = FALSE,
  quote = TRUE,
  sep = "\t",
  dec = ".",
  append = FALSE
)