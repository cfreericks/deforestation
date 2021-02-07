###############################################################################
###########################     FACTOR ANALYSIS     ###########################
###############################################################################


# Global settings ---------------------------------------------------------

# Default graphic settings
opar <- par(no.readonly = TRUE)


# Load libraries ----------------------------------------------------------

library(psych) # Faktoranalyse

library(GPArotation) # Rotation bei PCA

library(corrplot) # Schoene Korrelationsmatrizen

library(xtable) # Tabellen zu LaTeX exportieren

library(fpc) # Flexible procedures for clustering

library(RColorBrewer) # Farbpaletten fuers Plotten

library(rworldmap) # Karten plotten (GIS)

library(Hmisc) # for correlation matrix

library(cluster) # Cluster analysis


# Load data ---------------------------------------------------------------

masterdata <- here("data/processed/", "MASTERDATANOOUTLIER.csv")
items <- read.table(masterdata
                       , header = TRUE
                       , sep = "\t"
                       , quote = "\"'"
                       , dec = "."
                       , check.names = TRUE
                       , na.strings = c("NA", "#VALUE!")
)


# Transform data ----------------------------------------------------------

# nur unabhaengige Variablen und ISO3 als Zeilenbezeichner
items.short <- items[,-c(2,3)]

x <- items[,-c(1:3)]

# Z-Transformation
items.ztransf <- apply(x, 2, scale)

sdxtransf <- apply(items.ztransf, 2, sd)
meanxtransf <- round(apply(items.ztransf, 2, mean), 0)
sdx <- apply(x, 2, sd)
meanx <- round(apply(x, 2, mean), 0)
new.df <- cbind(meanx, sdx, meanxtransf, sdxtransf)

## in neue Datei exportieren
write.table(new.df,
            file = here("output/data/", "MEANandSDafterTransf.csv"),
            row.names = TRUE,
            quote = TRUE,
            sep = "\t",
            dec = ".",
            append = FALSE
            )


# Plot some data ----------------------------------------------------------

## Plotte AGRIPOPDIF.00.10.rel ~ SOLIDFUELUSE.2010 normal und standardisiert
postscript(file = here("output/plots/", "corrCattPop.eps")
           , horizontal = FALSE
           , width = 7, height = 2.5
           , pointsize = 12)

layout(matrix(c(1:3),1,3))

attach(items)
plot(CATT.DIFF.00.10.rel, POPDIF.00.10.rel, cex.lab = .8)
abline(h=0); abline(v=0)

plot(CATT.DIFF.00.10.rel, POPDIF.00.10.rel, cex.lab = .8, ylim = c(-50,50))
abline(h=0); abline(v=0)

detach(items)
plot(x.ztransf[,4], x.ztransf[,5], xlab = "CATT.DIFF.00.10.rel (z-transf.)"
     , ylab = "POPDIF.00.10.rel (z-transf.)", cex.lab = .8, xlim = c(-2,2))
abline(h=0); abline(v=0)

dev.off()
colnames(x.ztransf)


# FACTOR ANALYSIS =========================================================


# Correlation of variables ------------------------------------------------

# Scatter plot / Correlation Matrix

setEPS() # sets ps defaults
postscript(file = here("output/plots/", "scatterCorrPlot.eps")
           , horizontal = FALSE
           , width = 7, height = 5.5
           , pointsize = 10)
pairs.panels(x, gap = 0 ,scale = FALSE
             , cex = 1, cex.labels = .5
             , smooth = FALSE, ellipses = FALSE)
dev.off()

# Korrelationsmatrix

my.corr <- cor(items.ztransf, use = "everything", method = "pearson")

corrplot(my.corr, method = "square", type = "upper", order = "original"
         , tl.pos="lt", tl.col="black", tl.cex=0.6, tl.srt=45
         , addCoef.col="black", addCoefasPercent = TRUE
         , p.mat = 1-abs(my.corr), sig.level=0.50, insig = "blank")

# Reset auf default Grafikeinstellungen
par(opar)

x <- as.matrix(items[,-c(1:3)])
cor.mat <- rcorr(x, type="pearson")
str(cor.mat)
cor.mat.pval <- cor.mat$P
## in neue Datei exportieren
write.table(cor.mat.pval, 
            file = here("output/data/", "CorrMatSignifikanz.csv"),
            row.names = TRUE, 
            quote = TRUE, 
            sep = "\t", 
            dec = ".", 
            append = FALSE
            )


# Normality test ----------------------------------------------------------

shapiro.test(x[,1])
apply(x,2,shapiro.test)
## Ausgabe in dataframe
sh.test <- data.frame(
  VAR = colnames(x),
  W = apply(x, 2, function(x) {shapiro.test(x)$statistic}),
  PVALUE = apply(x, 2, function(x) {shapiro.test(x)$p.value})
)
sh.test[,2] <- format(sh.test[,2], scientific = F, digits = 4)
sh.test[,3] <- format(sh.test[,3], scientific = T, digits = 4)

significance <- gtools::stars.pval(as.numeric(sh.test$PVALUE))

sh.test <- cbind(sh.test, significance)
print(xtable::xtable(sh.test[,-1]), file = here("output/tex/", "shapirotest.tex"))

# Bartlett's Spherizitaets-Test -------------------------------------------

(my.bartlett <- cortest.bartlett(my.corr, n = nrow(items.ztransf)))

# Als LaTeX-Tabelle ausgeben
print(xtable::xtable(t(as.matrix(my.bartlett)), label = "tab:bartlettTest"), 
      file = here("output/tex/", "bartlett.tex"))

# Vergleichswert der Chiquadratverteilung
qchisq(0.99, 91)


# Kaiser-Meyer-Olkin Kriterium --------------------------------------------
(my.kmo <- KMO(my.corr))

# Anti-Image-Korrelationsmatrix
my.kmo$Image

# Als LaTeX-Tabelle ausgeben
print(xtable::xtable(as.matrix(my.kmo$Image), 
                     label = "tab:antiImageKorrMatrix", 
                     digits = 2), 
      file = here("output/tex/", "kmo_image.tex"))

# Measure of sampling adequacy (MSA)
my.kmo$MSAi

print(xtable::xtable(as.matrix(my.kmo$MSAi), 
                     label = "tab:kmoKriterium", 
                     digits = 5), 
      file = here("output/tex/", "kmo_kriterium.tex"))


# Number of factors to extract --------------------------------------------

# Hierzu Faktoranalyse zunaechst OHNE Rotation durchfuehren
# Fuer Screeplot Anzahl der Faktoren auf max., also 14
max.iter <- 200  # max. Anzahl Iterationen
nfactors <- 14  # Anzahl der Faktoren 

# Faktoranalyse mit Hauptachsenmethode OHNE Rotation
items.fa.norot <- fa(items.ztransf,
                       max.iter = max.iter,
                       nfactors = nfactors,
                       SMC = FALSE,
                       fm = "pa",
                       rotate = "none"
)

print(items.fa.norot)

# Scree-Plots
postscript(file = here("output/plots/", "myScreeplot.eps")
           , horizontal = FALSE
           , width = 5, height = 4
           , pointsize = 10)

plot(c(1:length(items.fa.norot$values)), items.fa.norot$values, type = "b", pch = 19
     , xlab = "Faktornummer", ylab = "Eigenwerte")
abline(h = 1)
legend("topright", c("Eigenwerte der Faktorlösung")
       , cex = .8, lty = c(1), pch = c(19))

dev.off()


# 3-Faktorloesung OHNE Rotation
nfactors <- 3  # Anzahl der Faktoren 
items.fa.norot <- fa(items.ztransf,
                     max.iter = max.iter,
                     nfactors=nfactors,
                     SMC=FALSE,
                     fm="pa",
                     rotate= "none"
)

# Ausgabe der Ergebnisse
print(items.fa.norot)

# Nur Ausgabe der Faktorladungen
cutoff <- .0
my.loadings <- (print(items.fa.norot$loadings, cutoff= cutoff))
my.loadings <- as.list.data.frame(my.loadings)
my.loadings <- data.frame(my.loadings, h2 = items.fa.norot$communality
                          , u2 = items.fa.norot$uniquenesses)

print(xtable::xtable(my.loadings), 
      file = here("output/tex/", "factorloadings_3norot.tex"))


# 4-Faktorloesung OHNE Rotation

nfactors <- 4  # Anzahl der Faktoren 
items.fa.norot <- fa(items.ztransf,
                     max.iter = max.iter,
                     nfactors=nfactors,
                     SMC=FALSE,
                     fm="pa",
                     rotate= "none"
)

# Ausgabe der Ergebnisse
print(items.fa.norot)

# Nur Ausgabe der Faktorladungen
cutoff <- .0
my.loadings <- (print(items.fa.norot$loadings, cutoff= cutoff))
my.loadings <- as.list.data.frame(my.loadings)
my.loadings <- data.frame(my.loadings, h2 = items.fa.norot$communality
                          , u2 = items.fa.norot$uniquenesses)

print(xtable::xtable(my.loadings), 
      file = here("output/tex/", "factorloadings_4norot.tex"))


# 4-Faktorloesung Hauptachsenmethode mit ROTATION (Varimax)

nfactors <- 4 # Anzahl der Faktoren
items.fa.varimax <- fa(items.ztransf,
                       max.iter = max.iter,
                       nfactors=nfactors,
                       SMC=FALSE,
                       fm="pa",
                       rotate= "varimax"
)

# Ausgabe der gesamten Ergebnisse
print(items.fa.varimax)

# Nur Ausgabe der Faktorladungen
cutoff <- .0
my.loadings <- (print(items.fa.varimax$loadings, cutoff= cutoff))
my.loadings <- as.list.data.frame(my.loadings)
my.loadings <- data.frame(my.loadings, h2 = items.fa.varimax$communality
                          , u2 = items.fa.varimax$uniquenesses)

print(xtable::xtable(my.loadings), 
      file = here("output/tex/", "factorloadings_4varimax.tex"))


# 3-Faktorloesung Hauptachsenmethode mit ROTATION (Varimax)

nfactors <- 3 # Anzahl der Faktoren
items.fa.varimax <- fa(items.ztransf,
                       max.iter = max.iter,
                       nfactors=nfactors,
                       SMC=FALSE,
                       fm="pa",
                       rotate= "varimax"
)

# Ausgabe der gesamten Ergebnisse
print(items.fa.varimax)

# Nur Ausgabe der Faktorladungen
cutoff <- .0
my.loadings <- (print(items.fa.varimax$loadings, cutoff= cutoff))
my.loadings <- as.list.data.frame(my.loadings)
my.loadings <- data.frame(my.loadings, h2 = items.fa.varimax$communality
                     , u2 = items.fa.varimax$uniquenesses)

print(xtable::xtable(my.loadings), 
      file = here("output/tex/", "factorloadings_3varimax.tex"))


# Bestimmung der Faktorwerte ----------------------------------------------

# Es gilt: Z = P*A' >>> P = Z*A*(A'*A)^-1
# Faktorwertematrix P soll bestimmt werden
# Koeffizientenmatrix A*(A'*A)^-1 wird mittels 
# Thurstone's least squares regression bestimmt

(my.weights <- items.fa.varimax$weights) # Koeffizientenmatrix
(my.scores <- items.fa.varimax$scores) # Faktorwerte-Matrix
my.scores <- data.frame(ISO3 = items[,1], my.scores)

print(xtable::xtable(my.weights), 
      file = here("output/tex/", "factorweights.tex"))

print(xtable::xtable(my.scores), 
      file = here("output/tex/", "factorscores.tex"))


# CLUSTER ANALYSIS ========================================================

cluster <- my.scores[,-1]

# MATRIX DER EUKLIDISCHEN DISTANZEN
dist.euclid <- daisy(cluster,metric="euclidean",stand=FALSE)


# Dendogramm --------------------------------------------------------------

dendogramm <- hclust(dist.euclid,method="average")

postscript(file = here("output/plots/", "dendogramm.eps")
           , horizontal = FALSE
           , width = 6, height = 4
           , pointsize = 7.5)

plot(dendogramm,xlab="Objekte",ylab="Distanzen",
     main="")

dev.off()

# Ausreisser entfernen
cluster.nooutlier <- cluster[-c(2, 15,63, 4,35),]
dist.euclid2 <- daisy(cluster.nooutlier,metric="euclidean",stand=FALSE)

# Neues Dendogramm plotten
dendogramm2 <- hclust(dist.euclid2,method="average")

postscript(file = here("output/plots/", "dendogramm2.eps")
           , horizontal = FALSE
           , width = 6, height = 4
           , pointsize = 7.5)

plot(dendogramm2,xlab="Objekte",ylab="Distanzen",
     main="")

# 5 Clusterloesung einzeichnen
rect.hclust(dendogramm2, k=5, border="red") 

dev.off()


# K-Means Clustering ------------------------------------------------------

attach(cluster.nooutlier)
# Standardisierung muss nach entfernen der Ausreisser erneut durchgefuehrt werden
pa1.stand <- as.numeric(scale(PA1))
pa2.stand <- as.numeric(scale(PA2))
pa3.stand <- as.numeric(scale(PA3))

cluster.k <- cbind(pa1.stand, pa2.stand, pa3.stand)

my.scores <- my.scores[-c(2, 15,63, 4,35),]
my.scores[,2] <- scale(my.scores[,2])
my.scores[,3] <- scale(my.scores[,3])
my.scores[,4] <- scale(my.scores[,4])

write.table(
  my.scores,
  file = here("output/data/", "FACTORVALUES.csv"),
  row.names = FALSE,
  quote = TRUE,
  sep = "\t",
  dec = ".",
  append = FALSE
)

# Anzahl der optimalen Clusterzahl ermitteln

(n <- nrow(cluster.k)) # Anzahl der Objekte
(m <- n-1) # max. Anz. moegl. Cluster
(k <- c(1:m)) # Vektor mit Anzahl moeglicher Cluster

# 1-Clusterloesung ausgeben
# Streuungsquadratsumme entspricht in dem Fall Anz. Var. * (Anz.Obj.-1)
(single.cluster <- kmeans(cluster.k, centers = 1))
(sqges <- single.cluster$totss)

# Streuungsquadratsumme fuer alle Clusterloesungen bestimmen
sq <- as.numeric(c())
for (i in 1:m) {
  c.sumsquares <- kmeans(cluster.k, centers = i)$tot.withinss
  sq[i] <- c.sumsquares
}

# Aenderung der Streuungsquadratsumme
sqdiff <- c(NA, sq[1:(m-1)] - sq[2:m])

# Erklaerte Streuung ETA_q^2
eta.q.2 <- 1-(sq/sqges)
diff.eta.q.2 <- c(NA, NA, abs(eta.q.2[2:(m-1)] - eta.q.2[3:m]))

# Proportionale Fehlerverbesserung PRE_q^2
pre.q.2 <- c(NA, 1 - sq[2:m] / sq[1:(m-1)])
diff.pre.q.2 <- c(NA, abs(pre.q.2[1:(m-1)] - pre.q.2[2:m]))

# F-MAX-Statistik F-MAX_q
fmax.q <- ((sqges-sq)/(k-1)) / (sq/(n-k))

# Dataframe mit Pruefstatistiken anlegen
pruefstatistik <- data.frame(k,sq,sqdiff, eta.q.2, pre.q.2, fmax.q
                             , diff.eta.q.2, diff.pre.q.2
)

# Ausgabe der ersten 15 Zeilen der Pruefstatistik
(pruefstatistik.15 <- head(pruefstatistik, 15))

# Save as Latex table
print(xtable::xtable(pruefstatistik.15), 
      file = here("output/tex/", "kmeans_teststatistics.tex"))

# SQ, SQDIFF plotten
postscript(file = here("output/plots/", "streuungsquadrate.eps")
           , horizontal = FALSE
           , width = 6, height = 4
           , pointsize = 10)

plot(k, sq, type = "l", pch = 19, col = "black"
     , xlab = "Clusterzahl", ylab = "" #, xlim = c(0,300)
     )
lines(sqdiff, col = "red")
legend("topright", c("SQ(k)", "Diff SQ(k)")
       , cex = .8, lty = 1, col = c("black", "red"))

dev.off()


# 5 Cluster solution ------------------------------------------------------

# als optimal ermittelte Anzahl der Cluster: 5
clusterzentren <- kmeans(cluster.k[,1:2], centers = 5)

#  Clusterzuordnungen an standardisierten Datensatz anhaengen
clusterzentren$cluster
cluster.k<-data.frame(cbind(cluster.k,clusterzentren$cluster))

# Clusterzuordnungen an urspruenglichen Datensatz 'items' anhaengen
# Dazu zunaechst Gruppierungsvariable erzeugen
(wald.gruppe <- factor(clusterzentren$cluster,
                                   labels=c("A", "B", "C", "D", "E")))

# Die identifizierten Ausreisser muessen noch entfernt werden
items.grouped <- items[-c(2, 15,63, 4,35),]

# dann Gruppierungsvariable anhaengen
items.grouped <- cbind(items.grouped, wald.gruppe)

head(items.grouped)

# in neue Datei exportieren

write.table(
  items.grouped,
  file = here("output/data/", "ITEMSGROUPED.csv"),
  row.names = FALSE,
  quote = TRUE,
  sep = "\t",
  dec = ".",
  append = FALSE
)

# Merge variables with cluster allocation and factor values

items_grouped_factors <- merge(items.grouped, my.scores, by = "ISO3")

write.table(
  items_grouped_factors,
  file = here("output/data/", "ITEMSGROUPEDWITHFACTORS.csv"),
  row.names = FALSE,
  quote = TRUE,
  sep = "\t",
  dec = ".",
  append = FALSE
)


# Grouped scatter plot ----------------------------------------------------

# Deforestation ~ allVars, Gruppierung farblich hervorgehoben

attach(items.grouped)

# display.brewer.all() # zum Anzeigen der verfuegbaren Paletten
my.col <- brewer.pal(5, "Set1")#"Accent")
my.lab <- colnames(items.grouped)
my.ylim <- c(min(ANDIFF.00.10.rel), max(ANDIFF.00.10.rel)+1)

postscript(file = here("output/plots/", "groupedScatter.eps")
           , horizontal = FALSE
           , width = 7, height = 7
           , pointsize = 7.5)

par(mar=c(2.1,2.3,1.1,0), oma=c(1,1,1,1), xpd=TRUE)
layout(matrix(c(1:14,0,rep(15,3)),6,3, byrow=TRUE))
for (i in 4:17) {
plot(items.grouped[,i], ANDIFF.00.10.rel
       , pch = 19, col=my.col, ann = TRUE, ylim = my.ylim)
mtext(my.lab[i], side = 3, line = -1.4, cex = .8)
}

plot.new()

group.names <- levels(wald.gruppe)
group.counts <- as.vector(table(wald.gruppe))
my.legend <- paste(group.names, " (n = ", group.counts, ")", sep="")

legend("center", legend=my.legend, title="Cluster"
       , col=my.col, pch= 19, horiz = TRUE)

dev.off()
par(opar)


# Plot worldmap -----------------------------------------------------------

postscript(file = here("output/plots/", "groupedWorldMap.eps")
           , horizontal = FALSE
           , width = 7, height = 5.5
           , pointsize = 7.5)

par(xpd=TRUE)

# Daten mit spatial Polygon zusammenfuehren
sPDF <- joinCountryData2Map(items.grouped
                            , joinCode = "ISO3"
                            , nameJoinColumn = "ISO3"
                            , mapResolution="coarse"
                            , verbose = TRUE
)

mapParams <- mapCountryData( sPDF
                             , nameColumnToPlot="wald.gruppe"
                             , addLegend ='FALSE'
                             , colourPalette = my.col
                             , missingCountryCol = "lightgrey"
                             , oceanCol = "lightblue"
                             , catMethod = "categorical"
                             , mapTitle =""
                             , xlim = c(-160, 160)
                             , ylim = c(-80,90)
                             , mapRegion = "world"
)

legend("bottom", legend=my.legend, title="Cluster"
       , pt.bg=my.col, pch= 22, horiz = TRUE, inset=c(0,0)
       , bg= "white", pt.cex=1.7)

dev.off()
par(opar)


# # Lage- und Streumasse je Cluster -----------------------------------------
# 
# # Mittelwerte je Cluster fuer alle Variablen
# cluster.means <- aggregate(items.grouped[,-c(1,2,18)], by=list(Cluster=wald.gruppe)
#           , mean, na.rm = TRUE)
# cluster.means <- format(cluster.means, digits=2, nsmall = 3)
# 
# print(xtable::xtable(t(cluster.means)), 
#       file = here("output/tex/", "clustermeans.tex"))
# 
# # Variationskoeffizient
# varko <- function(dat){
#   sd(dat, na.rm = T) / mean(dat, na.rm = T) *100
# }
# varko(items.grouped$ANDIFF.00.10.rel)
# sd(ANDIFF.00.10.rel)/mean(ANDIFF.00.10.rel)*100
# 
# cluster.varko <- aggregate(items.grouped[,-c(1,2,18)], by=list(Cluster=wald.gruppe)
#                            , varko)
# cluster.varko <- format(cluster.varko, digits=1, nsmall = 1)
# 
# cluster.varko.t <- t(cluster.varko)
# 
# print(xtable::xtable(t(cluster.varko)), 
#       file = here("output/tex/", "clustervarko.tex"))
