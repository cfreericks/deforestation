# FAOSTAT Export - Cattle per ha
# enthaelt Daten zur Viehbesatzdichte fuer Rinder fuer 2000 und 2010
###############################################################################

# Load data ---------------------------------------------------------------

infile <- here("data", "raw", "cattle_per_ha.csv")
CATTLEDENS.df <- read.table(infile
                            , header = TRUE
                            , sep = "\t"
                            , quote = "\"'"
                            , dec = "."
                            , check.names = TRUE
)

str(CATTLEDENS.df)


# Transform data ----------------------------------------------------------

CATTLEDENS.new <- merge(subset(CATTLEDENS.df, 
                               CATTLEDENS.df$Year == 2000, 
                               select = c(Country, Value)), 
                        subset(CATTLEDENS.df, 
                               CATTLEDENS.df$Year == 2010, 
                               select = c(Country, Value)), 
                        by = "Country", 
                        all.y = TRUE
                        )

names(CATTLEDENS.new) <- c("COUNTRY", "CATTLEDENS2000", "CATTLEDENS2010")

CATT.DIFF.00.10 <- with(CATTLEDENS.new, {CATTLEDENS2010 - CATTLEDENS2000})
CATT.DIFF.00.10.rel <-  CATT.DIFF.00.10 / CATTLEDENS.new$CATTLEDENS2000 * 100

CATTLEDENS.new <- data.frame(CATTLEDENS.new
                             , CATT.DIFF.00.10 = CATT.DIFF.00.10
                             , CATT.DIFF.00.10.rel = CATT.DIFF.00.10.rel
                             )

# Beobachtungen, die fuer beide Zeitpunkte 0 haben, fuehren zu NaN bei 
# CATT.DIFF.00.10.rel
# Div durch Null
# die NaN's werden 0 gesetzt
CATTLEDENS.new[is.nan(CATTLEDENS.new[,5]),5] <- 0
# pruefen, ob noch NaN vorkommt
table(is.nan(CATTLEDENS.new[,5]))

str(CATTLEDENS.new)

# Save  dataset -----------------------------------------------------------

outfile <- here("data", "interim", "CATTLEPERHA_edt.csv")
write.table(
  CATTLEDENS.new,
  file = outfile,
  row.names = FALSE,
  quote = TRUE,
  sep = "\t",
  dec = ".",
  append = FALSE
)
