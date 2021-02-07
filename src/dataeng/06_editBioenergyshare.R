# FAOSTAT Export - Bioenergy production as a % of total energy production 
# Sum of different bioenergy (e.g. biodiesel, biogases).
# Share of bioenergy on renewable energy prod. = (Biofuels prod/Total renewable 
# production)*100
# Data: http://faostat.fao.org/site/676/DesktopDefault.aspx?PageID=676#ancor
# Metadata: http://faostat.fao.org/site/699/default.aspx
#
# INTERPRETATION:
# Abnahme des Bioenergieanteils in vielen Laendern aufgrund zunehmendem Anteil 
# sonstiger EE bspw. Wind, Solar, etc
###############################################################################

# Load data ---------------------------------------------------------------

infile <- here("data", "raw", "BIOENERGYSHARE.csv")
BIOENERGYSHARE.raw <- read.table(infile
                            , header = TRUE
                            , sep = "\t"
                            , quote = "\"'"
                            , dec = "."
                            , check.names = TRUE
)

# Transform data ----------------------------------------------------------

str(BIOENERGYSHARE.raw)
# Pruefen ob irgendwo NA's im Datensatz sind
table(is.na(BIOENERGYSHARE.raw))
# Zeilennummer ausgeben lassen, wo NA's in irgendeiner der Variablen vorkommen
NA.index <-
  unique (unlist (lapply (BIOENERGYSHARE.raw, function (x)
    which (is.na (x)))))
# entsprechende Zeile(n) anzeigen lassen
BIOENERGYSHARE.raw[NA.index,]

# Datensatz mit NA ausschliessen
BIOENERGYSHARE.new <- na.omit(BIOENERGYSHARE.raw)

# Save  dataset -----------------------------------------------------------

outfile <- here("data", "interim", "BIOENERGYSHARE_edt.csv")
write.table(
  BIOENERGYSHARE.new,
  file = outfile,
  row.names = FALSE,
  quote = TRUE,
  sep = "\t",
  dec = ".",
  append = FALSE
)