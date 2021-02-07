# Plot histograms for all variables


# Load libraries ----------------------------------------------------------

library(here)

# Load data ---------------------------------------------------------------

infile <- list(here("data", "interim", "FORAREA.CHANGE.csv"), #1
               here("data", "interim", "FAOCROPSMEADOWS.csv"), #2
               here("data", "interim", "CATTLEPERHA_edt.csv"), #3
               here("data", "interim", "POPULATION_edt.csv"), #4
               here("data", "interim", "BIOENERGYSHARE_edt.csv"), #5
               here("data", "interim", "FUELSHARE_edt.csv"), #6
               here("data", "raw", "SOLIDFUELUSE.csv"), #7
               here("data", "interim", "FOREXSHARE.csv"), #8
               here("data", "interim", "roads_paved_edt.csv"), #9
               here("data", "interim", "GDP_growth_edt.csv"), #10
               here("data", "raw", "TI_corruption_2012.csv") #11
)

data.list <- lapply(infile, read.table
                              , header = TRUE
                              , sep = "\t"
                              , quote = "\"'"
                              , dec = "."
                              , check.names = TRUE
)
str(data.list)
###############################################################################
outfile <- here("output", "plots", "hist_qqplot.pdf")
pdf(file = outfile
    , width = 210 / 25.4 # 1in = 25.4mm
    , height = 297 / 25.4
)
par(mfrow = c(4,2))
###############################################################################

## Histogramm 1: ANDIFF.00.10.rel
i <- 1 ## Welches Listenelement enthaelt das Dataframe
var <- "ANDIFF.00.10.rel"
j <- which(colnames(data.list[[i]])== var) ## Spalte des jew. Dataframes

## die zu plottende Variable aus dem dataframe in Vektor "data" extrahieren
data <- data.list[[i]][,j] 
str(data.list[[i]])

h <- hist(
  data,
  freq = TRUE,
  ylab = "Anzahl",
  xlab = "Änderung in %",
  main = "Relative jährl. durchschnittl. Änderung \n der Waldfläche von 2000 - 2010",
  cex.main = 0.9,
  cex.axis = 0.9,
  cex.lab = 0.9,
  ylim = c(0, 150),
  pty = "s"
)

legend(
  "topright",
  legend = paste(
    "n = ",
    sum(h$counts),
    "\n",
    "25%Q = ",
    format(
      quantile(data, 0.25, na.rm = TRUE),
      digits = 2,
      scientific = FALSE
    ),
    "\n",
    "MEDIAN = ",
    format(
      median(data, na.rm = TRUE),
      digits = 2,
      scientific = FALSE
    ),
    "\n",
    "75%Q = ",
    format(
      quantile(data, 0.75, na.rm = TRUE),
      digits = 2,
      scientific = FALSE
    ),
    "\n",
    "MEAN = ",
    format(mean(data, na.rm = T), digits = 2, scientific = FALSE),
    "\n",
    "SD = ",
    format(
      sd(data, na.rm = TRUE),
      digits = 2,
      scientific = FALSE
    ),
    "\n",
    "VAR.COEFF = ",
    format(
      sd(data, na.rm = T) / mean(data, na.rm = T) * 100,
      digits = 2,
      scientific = FALSE
    ),
    " %"
  ),
  bty = "n",
  cex = 0.9
)

text(h$mids, h$counts+4, labels = h$counts, cex = 0.9)
 
## Quantil-Quantil-Plot
qqnorm(data)
qqline(data)
shapiro <- shapiro.test(data)
legend("topleft", legend = paste(shapiro$method, "\n", "W = ", format(shapiro$statistic, digits = 2, scientific = FALSE), "; p-value = ", format(shapiro$p.value, digits = 2, scientific = TRUE)), bty= "n", cex = 0.9)
####################################################################################

## Histogramm 2: CROPAREA.10.rel
i <- 2 ## Welches Listenelement enthaelt das Dataframe
var <- "CROPAREA.10.rel"
(j <- which(colnames(data.list[[i]])== var)) ## Spalte des jew. Dataframes

data <- data.list[[i]][,j] ## die zu plottende Variable aus dem dataframe in Vektor "data" extrahieren
str(data.list[[i]])

h <- hist(data
#           , br = 10
          , freq = TRUE
          , ylab = "Anzahl"
          , xlab = "Anteil in %"
          , main = "Anteil der Anbaufläche einjähriger und \n mehrjähriger Kulturen an Gesamtfläche, 2010"
          , cex.main = 0.9, cex.axis = 0.9, cex.lab = 0.9
          , ylim = c(0,110)
          , pty = "s"
)
legend("topright", legend = paste("n = ", sum(h$counts), "\n"
                                  , "25%Q = ", format(quantile(data, 0.25, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "MEDIAN = ", format(median(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "75%Q = ", format(quantile(data, 0.75, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "MEAN = ", format(mean(data, na.rm =T), digits = 2, scientific = FALSE), "\n"
                                  , "SD = ", format(sd(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "VAR.COEFF = ", format(sd(data, na.rm =T) / mean(data, na.rm =T) *100, digits = 2, scientific = FALSE), " %"
), bty= "n", cex = 0.9)

text(h$mids, h$counts+4, labels = h$counts, cex = 0.9)

## Quantil-Quantil-Plot
qqnorm(data)
qqline(data)
shapiro <- shapiro.test(data)
legend("topleft", legend = paste(shapiro$method, "\n", "W = ", format(shapiro$statistic, digits = 2, scientific = FALSE), "; p-value = ", format(shapiro$p.value, digits = 2, scientific = TRUE)), bty= "n", cex = 0.9)
####################################################################################

## Histogramm 2.1: CROPAREA.10.rel.LOG
i <- 2 ## Welches Listenelement enthaelt das Dataframe
var <- "CROPAREA.10.rel"
(j <- which(colnames(data.list[[i]])== var)) ## Spalte des jew. Dataframes

data <- data.list[[i]][,j] ## die zu plottende Variable aus dem dataframe in Vektor "data" extrahieren
data <- log10(data) ## Logtransformiert
str(data.list[[i]])

h <- hist(data
          , axes = FALSE
#           , br = 10
          , freq = TRUE
          , ylab = "Anzahl"
          , xlab = "Anteil in %"
          , main = "Anteil der Anbaufläche einjähriger und \n mehrjähriger Kulturen an Gesamtfläche, 2010 \n (logtransformiert)"
          , cex.main = 0.9, cex.axis = 0.9, cex.lab = 0.9
          , ylim = c(0,60)
          , pty = "s"
)
Axis(side = 2)
Axis(at = h$breaks, labels = 10^h$breaks, side = 1)

h$breaks
legend("topleft", legend = paste("n = ", sum(h$counts), "\n"
                                 , "25%Q = ", format(quantile(data, 0.25, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                 , "MEDIAN = ", format(median(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                 , "75%Q = ", format(quantile(data, 0.75, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                 , "MEAN = ", format(mean(data, na.rm =T), digits = 2, scientific = FALSE), "\n"
                                 , "SD = ", format(sd(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                 , "VAR.COEFF = ", format(sd(data, na.rm =T) / mean(data, na.rm =T) *100, digits = 2, scientific = FALSE), " %"
), bty= "n", cex = 0.9)

text(h$mids, h$counts+4, labels = h$counts, cex = 0.9)

## Quantil-Quantil-Plot
qqnorm(data)
qqline(data)
shapiro <- shapiro.test(data)
legend("topleft", legend = paste(shapiro$method, "\n", "W = ", format(shapiro$statistic, digits = 2, scientific = FALSE), "; p-value = ", format(shapiro$p.value, digits = 2, scientific = TRUE)), bty= "n", cex = 0.9)
####################################################################################

## Histogramm 3: MEADOWAREA.10.rel
i <- 2 ## Welches Listenelement enthaelt das Dataframe
var <- "MEADOWAREA.10.rel"
(j <- which(colnames(data.list[[i]])== var)) ## Spalte des jew. Dataframes

data <- data.list[[i]][,j] ## die zu plottende Variable aus dem dataframe in Vektor "data" extrahieren
str(data.list[[i]])

h <- hist(data
          , br = 10
          , freq = TRUE
          , ylab = "Anzahl"
          , xlab = "Anteil in %"
          , main = "Anteil dauerhaft und zeitweise genutzter \n Wiesen an Gesamtfläche, 2010"
          , cex.main = 0.9, cex.axis = 0.9, cex.lab = 0.9
          , ylim = c(0,80)
          , pty = "s"
)
legend("topright", legend = paste("n = ", sum(h$counts), "\n"
                                  , "25%Q = ", format(quantile(data, 0.25, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "MEDIAN = ", format(median(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "75%Q = ", format(quantile(data, 0.75, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "MEAN = ", format(mean(data, na.rm =T), digits = 2, scientific = FALSE), "\n"
                                  , "SD = ", format(sd(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "VAR.COEFF = ", format(sd(data, na.rm =T) / mean(data, na.rm =T) *100, digits = 2, scientific = FALSE), " %"
), bty= "n", cex = 0.9)

text(h$mids, h$counts+4, labels = h$counts, cex = 0.9)

## Quantil-Quantil-Plot
qqnorm(data)
qqline(data)
shapiro <- shapiro.test(data)
legend("topleft", legend = paste(shapiro$method, "\n", "W = ", format(shapiro$statistic, digits = 2, scientific = FALSE), "; p-value = ", format(shapiro$p.value, digits = 2, scientific = TRUE)), bty= "n", cex = 0.9)
####################################################################################

## Histogramm 3.1: MEADOWAREA.10.rel.LOG
i <- 2 ## Welches Listenelement enthaelt das Dataframe
var <- "MEADOWAREA.10.rel"
(j <- which(colnames(data.list[[i]])== var)) ## Spalte des jew. Dataframes

data <- data.list[[i]][,j] ## die zu plottende Variable aus dem dataframe in Vektor "data" extrahieren
data <- log10(data) ## Logtransformiert
str(data.list[[i]])

h <- hist(data
          , axes = FALSE
          #           , br = 10
          , freq = TRUE
          , ylab = "Anzahl"
          , xlab = "Anteil in %"
          , main = "Anteil dauerhaft und zeitweise genutzter \n Wiesen an Gesamtfläche, 2010 \n (logtransformiert)"
          , cex.main = 0.9, cex.axis = 0.9, cex.lab = 0.9
          , ylim = c(0,80)
          , pty = "s"
)
Axis(side = 2)
Axis(at = h$breaks, labels = 10^h$breaks, side = 1)

h$breaks
legend("topleft", legend = paste("n = ", sum(h$counts), "\n"
                                 , "25%Q = ", format(quantile(data, 0.25, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                 , "MEDIAN = ", format(median(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                 , "75%Q = ", format(quantile(data, 0.75, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                 , "MEAN = ", format(mean(data, na.rm =T), digits = 2, scientific = FALSE), "\n"
                                 , "SD = ", format(sd(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                 , "VAR.COEFF = ", format(sd(data, na.rm =T) / mean(data, na.rm =T) *100, digits = 2, scientific = FALSE), " %"
), bty= "n", cex = 0.9)

text(h$mids, h$counts+4, labels = h$counts, cex = 0.9)

## Quantil-Quantil-Plot
qqnorm(data)
qqline(data)
shapiro <- shapiro.test(data)
legend("topleft", legend = paste(shapiro$method, "\n", "W = ", format(shapiro$statistic, digits = 2, scientific = FALSE), "; p-value = ", format(shapiro$p.value, digits = 2, scientific = TRUE)), bty= "n", cex = 0.9)
####################################################################################

## Histogramm 4: CATT.DIFF.00.10.rel
i <- 3 ## Welches Listenelement enthaelt das Dataframe
var <- "CATT.DIFF.00.10.rel"
(j <- which(colnames(data.list[[i]])== var)) ## Spalte des jew. Dataframes

data <- data.list[[i]][,j] ## die zu plottende Variable aus dem dataframe in Vektor "data" extrahieren
str(data.list[[i]])

h <- hist(data
          , br = 10
          , freq = TRUE
          , ylab = "Anzahl"
          , xlab = "Änderung in %"
          , main = "Änderung der Viehbesatzdichte Rinder \n von 2000 bis 2010"
          , cex.main = 0.9, cex.axis = 0.9, cex.lab = 0.9
          , ylim = c(0,100)
          , pty = "s"
)
legend("topright", legend = paste("n = ", sum(h$counts), "\n"
                                  , "25%Q = ", format(quantile(data, 0.25, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "MEDIAN = ", format(median(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "75%Q = ", format(quantile(data, 0.75, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "MEAN = ", format(mean(data, na.rm =T), digits = 2, scientific = FALSE), "\n"
                                  , "SD = ", format(sd(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "VAR.COEFF = ", format(sd(data, na.rm =T) / mean(data, na.rm =T) *100, digits = 2, scientific = FALSE), " %"
), bty= "n", cex = 0.9)

text(h$mids, h$counts+4, labels = h$counts, cex = 0.9)

## Quantil-Quantil-Plot
qqnorm(data)
qqline(data)
shapiro <- shapiro.test(data)
legend("topleft", legend = paste(shapiro$method, "\n", "W = ", format(shapiro$statistic, digits = 2, scientific = FALSE), "; p-value = ", format(shapiro$p.value, digits = 2, scientific = TRUE)), bty= "n", cex = 0.9)
####################################################################################

## Histogramm 5: POPDIF.00.10.rel
i <- 4 ## Welches Listenelement enthaelt das Dataframe
var <- "POPDIF.00.10.rel"
(j <- which(colnames(data.list[[i]])== var)) ## Spalte des jew. Dataframes

data <- data.list[[i]][,j] ## die zu plottende Variable aus dem dataframe in Vektor "data" extrahieren
str(data.list[[i]])

h <- hist(data
          , br = 15
          , freq = TRUE
          , ylab = "Anzahl"
          , xlab = "Änderung in %"
          , main = "Änderung der Gesamtlandesbevölkerung \n von 2000 bis 2010"
          , cex.main = 0.9, cex.axis = 0.9, cex.lab = 0.9
          , ylim = c(0,130)
          , pty = "s"
)
h$breaks
legend("topright", legend = paste("n = ", sum(h$counts), "\n"
                                  , "25%Q = ", format(quantile(data, 0.25, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "MEDIAN = ", format(median(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "75%Q = ", format(quantile(data, 0.75, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "MEAN = ", format(mean(data, na.rm =T), digits = 2, scientific = FALSE), "\n"
                                  , "SD = ", format(sd(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "VAR.COEFF = ", format(sd(data, na.rm =T) / mean(data, na.rm =T) *100, digits = 2, scientific = FALSE), " %"
), bty= "n", cex = 0.9)

text(h$mids, h$counts+4, labels = h$counts, cex = 0.9)

## Quantil-Quantil-Plot
qqnorm(data)
qqline(data)
shapiro <- shapiro.test(data)
legend("topleft", legend = paste(shapiro$method, "\n", "W = ", format(shapiro$statistic, digits = 2, scientific = FALSE), "; p-value = ", format(shapiro$p.value, digits = 2, scientific = TRUE)), bty= "n", cex = 0.9)
####################################################################################
# 
# ## Histogramm 6: RURPOP.rel 
# i <- 4 ## Welches Listenelement enthaelt das Dataframe
# var <- "RURPOP.rel"
# (j <- which(colnames(data.list[[i]])== var)) ## Spalte des jew. Dataframes
# 
# data <- data.list[[i]][,j] ## die zu plottende Variable aus dem dataframe in Vektor "data" extrahieren
# str(data.list[[i]])
# 
# h <- hist(data
# #           , br = 15
#           , freq = TRUE
#           , ylab = "Anzahl"
#           , xlab = "Anteil in %"
#           , main = "Anteil Ländliche Bevölkerung \n an Gesamtbevölkerung, 2010"
#           , cex.main = 0.9, cex.axis = 0.9, cex.lab = 0.9
#           , ylim = c(0,60)
#           , pty = "s"
# )
# h$breaks
# legend("topright", legend = paste("n = ", sum(h$counts), "\n"
#                                   , "25%Q = ", format(quantile(data, 0.25, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
#                                   , "MEDIAN = ", format(median(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
#                                   , "75%Q = ", format(quantile(data, 0.75, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
#                                   , "MEAN = ", format(mean(data, na.rm =T), digits = 2, scientific = FALSE), "\n"
#                                   , "SD = ", format(sd(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
#                                   , "VAR.COEFF = ", format(sd(data, na.rm =T) / mean(data, na.rm =T) *100, digits = 2, scientific = FALSE), " %"
# ), bty= "n", cex = 0.9)
# 
# text(h$mids, h$counts+4, labels = h$counts, cex = 0.9)
# 
# ## Quantil-Quantil-Plot
# qqnorm(data)
# qqline(data)
# shapiro <- shapiro.test(data)
# legend("topleft", legend = paste(shapiro$method, "\n", "W = ", format(shapiro$statistic, digits = 2, scientific = FALSE), "; p-value = ", format(shapiro$p.value, digits = 2, scientific = TRUE)), bty= "n", cex = 0.9)
# ####################################################################################
# 
# ## Histogramm 7: URBPOP.rel  
# i <- 4 ## Welches Listenelement enthaelt das Dataframe
# var <- "URBPOP.rel"
# (j <- which(colnames(data.list[[i]])== var)) ## Spalte des jew. Dataframes
# 
# data <- data.list[[i]][,j] ## die zu plottende Variable aus dem dataframe in Vektor "data" extrahieren
# str(data.list[[i]])
# 
# h <- hist(data
#           #           , br = 15
#           , freq = TRUE
#           , ylab = "Anzahl"
#           , xlab = "Anteil in %"
#           , main = "Anteil Urbane Bevölkerung \n an Gesamtbevölkerung, 2010"
#           , cex.main = 0.9, cex.axis = 0.9, cex.lab = 0.9
#           , ylim = c(0,60)
#           , pty = "s"
# )
# h$breaks
# legend("topleft", legend = paste("n = ", sum(h$counts), "\n"
#                                   , "25%Q = ", format(quantile(data, 0.25, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
#                                   , "MEDIAN = ", format(median(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
#                                   , "75%Q = ", format(quantile(data, 0.75, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
#                                   , "MEAN = ", format(mean(data, na.rm =T), digits = 2, scientific = FALSE), "\n"
#                                   , "SD = ", format(sd(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
#                                   , "VAR.COEFF = ", format(sd(data, na.rm =T) / mean(data, na.rm =T) *100, digits = 2, scientific = FALSE), " %"
# ), bty= "n", cex = 0.9)
# 
# text(h$mids, h$counts+4, labels = h$counts, cex = 0.9)
# 
# ## Quantil-Quantil-Plot
# qqnorm(data)
# qqline(data)
# shapiro <- shapiro.test(data)
# legend("topleft", legend = paste(shapiro$method, "\n", "W = ", format(shapiro$statistic, digits = 2, scientific = FALSE), "; p-value = ", format(shapiro$p.value, digits = 2, scientific = TRUE)), bty= "n", cex = 0.9)
# ####################################################################################
# 
# ## Histogramm 8: AGRIPOP.rel  
# i <- 4 ## Welches Listenelement enthaelt das Dataframe
# var <- "AGRIPOP.rel"
# (j <- which(colnames(data.list[[i]])== var)) ## Spalte des jew. Dataframes
# 
# data <- data.list[[i]][,j] ## die zu plottende Variable aus dem dataframe in Vektor "data" extrahieren
# str(data.list[[i]])
# 
# h <- hist(data
#           #           , br = 15
#           , freq = TRUE
#           , ylab = "Anzahl"
#           , xlab = "Anteil in %"
#           , main = "Anteil Bäuerlicher Bevölkerung \n an Gesamtbevölkerung, 2010"
#           , cex.main = 0.9, cex.axis = 0.9, cex.lab = 0.9
#           , ylim = c(0,80)
#           , pty = "s"
# )
# h$breaks
# legend("topright", legend = paste("n = ", sum(h$counts), "\n"
#                                  , "25%Q = ", format(quantile(data, 0.25, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
#                                  , "MEDIAN = ", format(median(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
#                                  , "75%Q = ", format(quantile(data, 0.75, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
#                                  , "MEAN = ", format(mean(data, na.rm =T), digits = 2, scientific = FALSE), "\n"
#                                  , "SD = ", format(sd(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
#                                  , "VAR.COEFF = ", format(sd(data, na.rm =T) / mean(data, na.rm =T) *100, digits = 2, scientific = FALSE), " %"
# ), bty= "n", cex = 0.9)
# 
# text(h$mids, h$counts+4, labels = h$counts, cex = 0.9)
# 
# ## Quantil-Quantil-Plot
# qqnorm(data)
# qqline(data)
# shapiro <- shapiro.test(data)
# legend("topleft", legend = paste(shapiro$method, "\n", "W = ", format(shapiro$statistic, digits = 2, scientific = FALSE), "; p-value = ", format(shapiro$p.value, digits = 2, scientific = TRUE)), bty= "n", cex = 0.9)
####################################################################################

## Histogramm 9: BIOENDIFF.00.09.rel  
i <- 5 ## Welches Listenelement enthaelt das Dataframe
var <- "BIOENDIFF.00.09.rel"
(j <- which(colnames(data.list[[i]])== var)) ## Spalte des jew. Dataframes

data <- data.list[[i]][,j] ## die zu plottende Variable aus dem dataframe in Vektor "data" extrahieren
str(data.list[[i]])

h <- hist(data
          , br = 15
          , freq = TRUE
          , ylab = "Anzahl"
          , xlab = "Änderung in %"
          , main = "Änderung des Bioenergieanteils an \n gesamter EE-Erzeugung von 2000 bis 2009"
          , cex.main = 0.9, cex.axis = 0.9, cex.lab = 0.9
          , ylim = c(0,80)
          , pty = "s"
)
h$breaks
legend("topright", legend = paste("n = ", sum(h$counts), "\n"
                                  , "25%Q = ", format(quantile(data, 0.25, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "MEDIAN = ", format(median(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "75%Q = ", format(quantile(data, 0.75, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "MEAN = ", format(mean(data, na.rm =T), digits = 2, scientific = FALSE), "\n"
                                  , "SD = ", format(sd(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "VAR.COEFF = ", format(sd(data, na.rm =T) / mean(data, na.rm =T) *100, digits = 2, scientific = FALSE), " %"
), bty= "n", cex = 0.9)

text(h$mids, h$counts+4, labels = h$counts, cex = 0.9)

## Quantil-Quantil-Plot
qqnorm(data)
qqline(data)
shapiro <- shapiro.test(data)
legend("topleft", legend = paste(shapiro$method, "\n", "W = ", format(shapiro$statistic, digits = 2, scientific = FALSE), "; p-value = ", format(shapiro$p.value, digits = 2, scientific = TRUE)), bty= "n", cex = 0.9)
####################################################################################

## Histogramm 10: FUELSHARE.2010  
i <- 6 ## Welches Listenelement enthaelt das Dataframe
var <- "FUELSHARE.2010"
(j <- which(colnames(data.list[[i]])== var)) ## Spalte des jew. Dataframes

data <- data.list[[i]][,j] ## die zu plottende Variable aus dem dataframe in Vektor "data" extrahieren
str(data.list[[i]])

h <- hist(data
#           , br = 15
          , freq = TRUE
          , ylab = "Anzahl"
          , xlab = "Anteil in %"
          , main = "Anteil Holzentnahmen zur energet. Nutzung \n an Gesamtentnahme, 2010"
          , cex.main = 0.9, cex.axis = 0.9, cex.lab = 0.9
          , ylim = c(0,80)
          , pty = "s"
)
h$breaks
legend("topleft", legend = paste("n = ", sum(h$counts), "\n"
                                  , "25%Q = ", format(quantile(data, 0.25, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "MEDIAN = ", format(median(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "75%Q = ", format(quantile(data, 0.75, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "MEAN = ", format(mean(data, na.rm =T), digits = 2, scientific = FALSE), "\n"
                                  , "SD = ", format(sd(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "VAR.COEFF = ", format(sd(data, na.rm =T) / mean(data, na.rm =T) *100, digits = 2, scientific = FALSE), " %"
), bty= "n", cex = 0.9)

text(h$mids, h$counts+4, labels = h$counts, cex = 0.9)

## Quantil-Quantil-Plot
qqnorm(data)
qqline(data)
shapiro <- shapiro.test(data)
legend("topleft", legend = paste(shapiro$method, "\n", "W = ", format(shapiro$statistic, digits = 2, scientific = FALSE), "; p-value = ", format(shapiro$p.value, digits = 2, scientific = TRUE)), bty= "n", cex = 0.9)
####################################################################################

## Histogramm 11: SOLIDFUELUSE  
i <- 7 ## Welches Listenelement enthaelt das Dataframe
var <- "SOLIDFUELUSE.2010"
(j <- which(colnames(data.list[[i]])== var)) ## Spalte des jew. Dataframes

data <- data.list[[i]][,j] ## die zu plottende Variable aus dem dataframe in Vektor "data" extrahieren
str(data.list[[i]])

h <- hist(data
          #           , br = 15
          , freq = TRUE
          , ylab = "Anzahl"
          , xlab = "Anteil in %"
          , main = "Anteil der Bevölkerung, die von Festbrennstoffen \n als Primärenergieträger abhängt, 2010"
          , cex.main = 0.9, cex.axis = 0.9, cex.lab = 0.9
          , ylim = c(0,100)
          , pty = "s"
)
h$breaks
legend("topright", legend = paste("n = ", sum(h$counts), "\n"
                                 , "25%Q = ", format(quantile(data, 0.25, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                 , "MEDIAN = ", format(median(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                 , "75%Q = ", format(quantile(data, 0.75, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                 , "MEAN = ", format(mean(data, na.rm =T), digits = 2, scientific = FALSE), "\n"
                                 , "SD = ", format(sd(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                 , "VAR.COEFF = ", format(sd(data, na.rm =T) / mean(data, na.rm =T) *100, digits = 2, scientific = FALSE), " %"
), bty= "n", cex = 0.9)

text(h$mids, h$counts+4, labels = h$counts, cex = 0.9)

## Quantil-Quantil-Plot
qqnorm(data)
qqline(data)
shapiro <- shapiro.test(data)
legend("topleft", legend = paste(shapiro$method, "\n", "W = ", format(shapiro$statistic, digits = 2, scientific = FALSE), "; p-value = ", format(shapiro$p.value, digits = 2, scientific = TRUE)), bty= "n", cex = 0.9)
####################################################################################

## Histogramm 12: FOREXSHARE2010  
i <- 8 ## Welches Listenelement enthaelt das Dataframe
var <- "FOREXSHARE2010"
(j <- which(colnames(data.list[[i]])== var)) ## Spalte des jew. Dataframes

data <- data.list[[i]][,j] ## die zu plottende Variable aus dem dataframe in Vektor "data" extrahieren
str(data.list[[i]])

h <- hist(data
          , br = 15
          , freq = TRUE
          , ylab = "Anzahl"
          , xlab = "Anteil in %"
          , main = "Anteil ausgeführter Holzprodukte \n an Gesamtexportwert des jew. Landes, 2010"
          , cex.main = 0.9, cex.axis = 0.9, cex.lab = 0.9
          , ylim = c(0,200)
          , pty = "s"
)
h$breaks
legend("topright", legend = paste("n = ", sum(h$counts), "\n"
                                  , "25%Q = ", format(quantile(data, 0.25, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "MEDIAN = ", format(median(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "75%Q = ", format(quantile(data, 0.75, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "MEAN = ", format(mean(data, na.rm =T), digits = 2, scientific = FALSE), "\n"
                                  , "SD = ", format(sd(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "VAR.COEFF = ", format(sd(data, na.rm =T) / mean(data, na.rm =T) *100, digits = 2, scientific = FALSE), " %"
), bty= "n", cex = 0.9)

text(h$mids, h$counts+4, labels = h$counts, cex = 0.9)

## Quantil-Quantil-Plot
qqnorm(data)
qqline(data)
shapiro <- shapiro.test(data)
legend("topleft", legend = paste(shapiro$method, "\n", "W = ", format(shapiro$statistic, digits = 2, scientific = FALSE), "; p-value = ", format(shapiro$p.value, digits = 2, scientific = TRUE)), bty= "n", cex = 0.9)
####################################################################################

## Histogramm 12.1: FOREXSHARE2010.LOG
i <- 8 ## Welches Listenelement enthaelt das Dataframe
var <- "FOREXSHARE2010"
(j <- which(colnames(data.list[[i]])== var)) ## Spalte des jew. Dataframes

data <- data.list[[i]][,j] ## die zu plottende Variable aus dem dataframe in Vektor "data" extrahieren
data <- log10(data) ## Logtransformiert
str(data.list[[i]])

h <- hist(data
          , axes = FALSE
          #           , br = 15
          , freq = TRUE
          , ylab = "Anzahl"
          , xlab = "Anteil in %"
          , main = "Anteil ausgeführter Holzprodukte \n an Gesamtexportwert des jew. Landes, 2010 \n (logtransformiert)"
          , cex.main = 0.9, cex.axis = 0.9, cex.lab = 0.9
          , ylim = c(0,60)
          , pty = "s"
)
Axis(side = 2)
Axis(at = h$breaks, labels = 10^h$breaks, side = 1)

h$breaks
legend("topleft", legend = paste("n = ", sum(h$counts), "\n"
                                  , "25%Q = ", format(quantile(data, 0.25, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "MEDIAN = ", format(median(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "75%Q = ", format(quantile(data, 0.75, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "MEAN = ", format(mean(data, na.rm =T), digits = 2, scientific = FALSE), "\n"
                                  , "SD = ", format(sd(data, na.rm = TRUE), digits = 2, scientific = FALSE), "\n"
                                  , "VAR.COEFF = ", format(sd(data, na.rm =T) / mean(data, na.rm =T) *100, digits = 2, scientific = FALSE), " %"
), bty= "n", cex = 0.9)

text(h$mids, h$counts+4, labels = h$counts, cex = 0.9)

## Quantil-Quantil-Plot
# qqnorm(data)
# qqline(data)
# shapiro <- shapiro.test(data)
# legend("topleft", legend = paste(shapiro$method, "\n", "W = ", format(shapiro$statistic, digits = 2, scientific = FALSE), "; p-value = ", format(shapiro$p.value, digits = 2, scientific = TRUE)), bty= "n", cex = 0.9)
####################################################################################
dev.off() ## Plot beenden
####################################################################################

