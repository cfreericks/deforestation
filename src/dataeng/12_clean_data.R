# Handling missing observations and outliers
# extensive exploraratory walk-through can be found as R Notebook in /rmd
# file: analyseMasterdataDist.Rmd
###############################################################################

# Load libraries ----------------------------------------------------------
library(here)
library(moments)

# Load data ---------------------------------------------------------------
infile <- here("data", "processed", "MASTERDATA2.csv")
masterdata <- read.table(infile
                         , header = TRUE
                         , sep = "\t"
                         , quote = "\"'"
                         , dec = "."
                         , check.names = TRUE
                         , na.strings = c("NA", "#VALUE!")
)


# Missing observations ----------------------------------------------------

#' Alle Faelle, die mind. 1 NA enthalten in neues df schreiben
masterdata.na <- masterdata[rowSums(is.na(masterdata)) > 0,]

#' Fallweiser Ausschluss, d.h. jedes Land, welches mindestens einen fehlenden
#' Wert für eine Variable enthält, wird aus dem Datensatz ausgeschlossen.
#' Es werden alle Variablen beibehalten
#' (cc = complete cases, cc)
#' 

masterdata.cc <- masterdata[rowSums(is.na(masterdata)) == 0,]


# Setting outliers to NA --------------------------------------------------

#' CROPAREADIF.00.10 > 40
masterdata.nooutlier <- masterdata.cc
masterdata.nooutlier[masterdata.nooutlier$CROPAREADIF.00.10 > 40, 4] <- NA
head(masterdata.nooutlier[order(masterdata.nooutlier[,4], 
                                decreasing = TRUE),c(1,2,4)])
table(is.na(masterdata.nooutlier[,4]))

#' MEADOWAREADIF.00.10 > 10 und < -10
masterdata.nooutlier[masterdata.nooutlier$MEADOWAREADIF.00.10 > 10, 5] <- NA
head(masterdata.nooutlier[order(masterdata.nooutlier[,5], 
                                decreasing = TRUE),c(1,2,5)])

masterdata.nooutlier[masterdata.nooutlier$MEADOWAREADIF.00.10 < -10 
                     & !is.na(masterdata.nooutlier$MEADOWAREADIF.00.10), 
                     5] <- NA
tail(masterdata.nooutlier[order(masterdata.nooutlier[,5], 
                                decreasing = TRUE),c(1,2,5)])
table(is.na(masterdata.nooutlier[,5]))

#' CATT.DIFF.00.10.rel > 50 und < -50
masterdata.nooutlier[masterdata.nooutlier$CATT.DIFF.00.10.rel > 50, 6] <- NA
head(masterdata.nooutlier[order(masterdata.nooutlier[,6], 
                                decreasing = TRUE),c(1,2,6)])

masterdata.nooutlier[masterdata.nooutlier$CATT.DIFF.00.10.rel < -50 
                     & !is.na(masterdata.nooutlier$CATT.DIFF.00.10.rel), 
                     6] <- NA
tail(masterdata.nooutlier[order(masterdata.nooutlier[,6], 
                                decreasing = TRUE),c(1,2,6)])
table(is.na(masterdata.nooutlier[,6]))

#' BIOENDIFF.00.09.rel > 100 und < -40
masterdata.nooutlier[masterdata.nooutlier$BIOENDIFF.00.09.rel > 100, 11] <- NA
head(masterdata.nooutlier[order(masterdata.nooutlier[,11], 
                                decreasing = TRUE),c(1,2,11)])

masterdata.nooutlier[masterdata.nooutlier$BIOENDIFF.00.09.rel < -40 
                     & !is.na(masterdata.nooutlier$BIOENDIFF.00.09.rel), 
                     11] <- NA
tail(masterdata.nooutlier[order(masterdata.nooutlier[,11], 
                                decreasing = TRUE),c(1,2,11)])
table(is.na(masterdata.nooutlier[,11]))

#' FUELSHAREDIF.00.10 > 20 und < -20
masterdata.nooutlier[masterdata.nooutlier$FUELSHAREDIF.00.10 > 20, 12] <- NA
head(masterdata.nooutlier[order(masterdata.nooutlier[,12], 
                                decreasing = TRUE),c(1,2,12)])

masterdata.nooutlier[masterdata.nooutlier$FUELSHAREDIF.00.10 < -20 
                     & !is.na(masterdata.nooutlier$FUELSHAREDIF.00.10), 
                     12] <- NA
tail(masterdata.nooutlier[order(masterdata.nooutlier[,12], 
                                decreasing = TRUE),c(1,2,12)])
table(is.na(masterdata.nooutlier[,12]))

#' FOREXSHAREDIF.00.10 > 5 und < -4
masterdata.nooutlier[masterdata.nooutlier$FOREXSHAREDIF.00.10 > 5, 14] <- NA
head(masterdata.nooutlier[order(masterdata.nooutlier[,14], 
                                decreasing = TRUE),c(1,2,14)])

masterdata.nooutlier[masterdata.nooutlier$FOREXSHAREDIF.00.10 < -4 
                     & !is.na(masterdata.nooutlier$FOREXSHAREDIF.00.10), 
                     14] <- NA
tail(masterdata.nooutlier[order(masterdata.nooutlier[,14], 
                                decreasing = TRUE),c(1,2,14)])
table(is.na(masterdata.nooutlier[,14]))

#' ROAD.CHANGE.00.10.rel > 10 und < -10
head(masterdata.nooutlier[order(masterdata.nooutlier[,15], 
                                decreasing = TRUE),c(1,2,15)])

masterdata.nooutlier[masterdata.nooutlier$ROAD.CHANGE.00.10.rel < -10 
                     & !is.na(masterdata.nooutlier$ROAD.CHANGE.00.10.rel), 
                     15] <- NA
tail(masterdata.nooutlier[order(masterdata.nooutlier[,15], 
                                decreasing = TRUE),c(1,2,15)])
table(is.na(masterdata.nooutlier[,15]))

#' GDPGROWTHAV.00.10 > 12 und < -1
masterdata.nooutlier[masterdata.nooutlier$GDPGROWTHAV.00.10 > 12, 16] <- NA
head(masterdata.nooutlier[order(masterdata.nooutlier[,16], 
                                decreasing = TRUE),c(1,2,16)])

masterdata.nooutlier[masterdata.nooutlier$GDPGROWTHAV.00.10 < -1 
                     & !is.na(masterdata.nooutlier$GDPGROWTHAV.00.10), 16] <- NA
tail(masterdata.nooutlier[order(masterdata.nooutlier[,16], 
                                decreasing = TRUE),c(1,2,16)])
table(is.na(masterdata.nooutlier[,16]))

#' Ausreisser aus Datensatz ausschliessen
table(complete.cases(masterdata.nooutlier))
masterdata.nooutlier <- 
  masterdata.nooutlier[rowSums(is.na(masterdata.nooutlier)) == 0,]


# Plot histogram ----------------------------------------------------------

## Histogramme aller 15 Variablen, inkl. Schiefe + Kurtosis in einer Plotmatrix
outfile <- here("output/plots/", "hist106.eps")
postscript(file = outfile
           , horizontal = FALSE
           , width = 11.7, height = 8.3
           , pointsize = 10)
nf <- layout(matrix(c(1:15,0),4,4, byrow=T))
# layout.show(nf)
for(i in 3:17){
  skew <- format(skewness(masterdata.cc[,i], na.rm = TRUE), scientific = FALSE, digits = 2)
#   kurt <- format(kurtosis(masterdata.cc[,i], na.rm = TRUE), scientific = FALSE, digits = 2)
  hist(masterdata.cc[,i], main=dimnames(masterdata.cc)[[2]][i], freq = TRUE
       , xlab = paste("Schiefe: ", skew , sep=""))
}
dev.off()
# Save data ---------------------------------------------------------------

outfile <- here("data", "processed", "MASTERDATANOOUTLIER.csv")
write.table(
  masterdata.nooutlier,
  file = outfile,
  row.names = FALSE,
  quote = TRUE,
  sep = "\t",
  dec = ".",
  append = FALSE
)
