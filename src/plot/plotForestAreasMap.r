#' Load libraries ----------------------------------------------------------

library("rworldmap")
library("RColorBrewer")

#' Load data ---------------------------------------------------------------

infile <- here("data", "interim", "FORAREA.CHANGE.csv")
FORAREA.CHANGE.df <- read.table(infile
                             , header = TRUE
                             , sep = "\t"
                             , quote = "\"'"
                             , dec = "."
                             , check.names = TRUE
)

#' Plot data ---------------------------------------------------------------

#' Karte der relativen Bewaldung plotten

#' Daten mit spatial Polygon zusammenfuehren
sPDF <- joinCountryData2Map(FORAREA.CHANGE.df
                            , joinCode = "ISO3"
                            , nameJoinColumn = "ISO3"
                            , mapResolution="coarse"
                            , verbose = TRUE
)

#'#' auf Karte plotten
colourPalette <- brewer.pal (5, "Greens")
x11(width = 14, height = 10)
mapParams <- 
  mapCountryData(sPDF, 
                 nameColumnToPlot="FORAREAREL.10", 
                 addLegend ='FALSE', 
                 colourPalette = colourPalette, 
                 missingCountryCol = "lightgrey", 
                 oceanCol = "lightblue", 
                 catMethod = c(0,10,30,50,70,100),
                 mapTitle = "Anteil Wald an Gesamtlandesfläche, 2010", 
                 xlim = c(-160, 160), 
                 ylim = c(-80,90)
                 )

#'add modified legend to the map#'
#' mapParams$legendText <- c(’antarctic’,’africa’,’oceania’,’americas’,’s.asia’,’eurasia’)
do.call( addMapLegendBoxes, c(mapParams, x='bottom',title="(Prozent)",horiz=TRUE))
box(which = "plot", lty = "solid", col = 'red')


#'#' Karte der absoluten jaehrlichen Waldflaechenaenderung plotten

#'#' Farbvektor fuer negative Werte (Entwaldung) in 4 Stufen
rc1 <- colorRampPalette(colors = c("#'A50F15", "#'FCAE91"), space="Lab")(4)
#' rc1 <- rev(brewer.pal(4,"Reds"))
#'#' Farbvektor fuer positive Werte (Aufforstung) in 4 Stufen
rc2 <- colorRampPalette(colors = c("#'BAE4B3", "#'006D2C"), space="Lab", bias = 5)(4)
#' rc2 <- rev(brewer.pal(4,"Greens"))
#' Merge the two vectors to get color vector for whole range of values
rampcols <- c(rc1,"#'FFFFFF", rc2)

#' Define breaks by which the observed values are categorised
rb1 <- c(-3000, -1000, -500, -250, -50)
rb2 <- c(50, 250, 500, 1000, 3000)
rampbreaks <- c(rb1, rb2)

x11(width = 14, height = 10)
mapParams <- mapCountryData( sPDF
                             , nameColumnToPlot="ANDIFF.00.10"
                             , addLegend ='FALSE'
                             , colourPalette = rampcols
                             , missingCountryCol = "lightgrey"
                             , oceanCol = "lightblue"
                             , catMethod = rampbreaks
                             , mapTitle ="Durchschnittliche jährliche Änderung der Waldfläche, 2000-2010"
                             , xlim = c(-160, 160)
                             , ylim = c(-80,90)
)

#'add modified legend to the map#'
do.call( addMapLegendBoxes, c(mapParams, x='bottom',title="(1000 ha)",horiz=TRUE))
box(which = "plot", lty = "solid", col = 'red')
