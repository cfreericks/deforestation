# Data source:
# The World Bank (2013b). World Development Indicators: Roads, paved (data 
# based on World Road Statistics, International Road Federation). Website. 
# http://data.worldbank.org/indicator/IS.ROD.PAVE.ZS; abgerufen am 06.10.2013.
###############################################################################

# Load data ---------------------------------------------------------------

infile <- here("data", "raw", "roads_paved.csv")
roads_paved <- read.table(infile
                            , header = TRUE
                            , sep = "\t"
                            , quote = "\"'"
                            , dec = "."
                            , check.names = TRUE
                            , na.strings = "NA"
)

# Transform data ----------------------------------------------------------

# define function for calculating average annual growth rate
# average annual growth rate is geometric mean of annual growth rates

av_annual_growth <- function(data) {
  data <- as.numeric(data)
  
  # find position of the FIRST not NA value in the vector
  pos.first <- min(which(is.finite(data) == TRUE), na.rm = TRUE)
  # find position of the LAST not NA value in the vector
  pos.last <- max(which(is.finite(data) == TRUE), na.rm = TRUE)
  # calculate periods n = t_n - t_0
  n <- pos.last - pos.first
  
  # find FIRST not NA value
  val.first <- data[pos.first]
  # find LAST not NA value
  val.last <- data[pos.last]
  
  # calculate growth factor q = x_n / x_0
  q <- val.last / val.first
  
  # calculate average annual growth rate as geometric mean
  p <- q ^ (1 / n) - 1
  return(p)
  
}

av_annual_growth(roads_paved[33,-(1:2)])

(agr <- apply(roads_paved[,-(1:2)], 1, av_annual_growth))
agr <- agr * 100
agr <- as.numeric(format(agr, digits = 2, scientific = FALSE))
roads_paved_new <- data.frame(roads_paved, ROAD.CHANGE.00.10.rel = agr)
length(which(roads_paved_new$AVANGROW!=0))

str(roads_paved_new)

# Save  dataset -----------------------------------------------------------

outfile <- here("data", "interim", "roads_paved_edt.csv")
write.table(
  roads_paved_new,
  file = outfile,
  row.names = FALSE,
  quote = TRUE,
  sep = "\t",
  dec = ".",
  append = FALSE
)
