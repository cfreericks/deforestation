
# Global settings ---------------------------------------------------------

# Default graphic settings
opar <- par(no.readonly = TRUE)

# Load libraries ----------------------------------------------------------

library(RColorBrewer) # Farbpaletten fuers Plotten

library(rworldmap) # Karten plotten (GIS)

library(xtable) # Tabellen zu LaTeX exportieren

# Load data ---------------------------------------------------------------

data <- read.table(here("output/data/", "ITEMSGROUPEDWITHFACTORS.csv")
                   , header = TRUE
                   , sep = "\t"
                   , quote = "\"'"
                   , dec = "."
                   , check.names = TRUE
                   , na.strings = c("NA", "#VALUE!")
)


# Cluster statistics ------------------------------------------------------

## data in den Suchpfad einhaengen
attach(data)

## Mittelwerte je Cluster für alle Variablen (Zentroid)

cluster.means <- aggregate(data[,-c(1,2,18:21)], by=list(Cluster=wald.gruppe)
                           , mean, na.rm = TRUE)
var.means <- apply(data[,-c(1,2,18:21)], 2, mean)
cluster.means.all <- t(rbind(cluster.means[,-1], var.means))
colnames(cluster.means.all) <- c("A", "B", "C", "D", "E", "gesamt")
print(xtable::xtable(cluster.means.all, digits=2), include.rownames=TRUE, 
      file = here("output/tex/", "clustermeans.tex"))

## Standardabweichung je Cluster für alle Variablen
cluster.sd <- aggregate(data[,-c(1,2,18:21)], by=list(Cluster=wald.gruppe)
                           , sd, na.rm = TRUE)

## Variationskoeffizient
varko <- function(dat){
  sd(dat, na.rm = T) / mean(dat, na.rm = T) *100
}
var.varko <- apply(data[,-c(1,2,18:21)], 2, varko)
cluster.varko <- aggregate(data[,-c(1,2,18:21)], by=list(Cluster=wald.gruppe)
                           , varko)
cluster.varko <- t(rbind(cluster.varko[,-1], var.varko))
colnames(cluster.varko) <- c("A", "B", "C", "D", "E", "gesamt")
print(xtable::xtable(cluster.varko, digits=2), include.rownames=TRUE, 
      file = here("output/tex/", "clustervarko.tex"))

## Mittelwerte der Variablen im gesamten Datensatz
var.means <- apply(data[,-c(1,2,18:21)], 2, mean)
var.means <- matrix(rep(var.means,5), nrow=5, ncol= 15, byrow = TRUE)

## Standardabweichung der Variablen im gesamten Datensatz
var.sd <- apply(data[,-c(1,2,18:21)], 2, sd)
var.sd <- matrix(rep(var.sd,5), nrow=5, ncol= 15, byrow = TRUE)

## z-Werte fuer die Cluster nach BACHER(S.317) ermitteln
cluster.zvalues <- (cluster.means[,-1] - var.means) / cluster.sd[,-1]
cluster.zvalues <- t(cluster.zvalues)
colnames(cluster.zvalues) <- t(cluster.means[,1])
print(xtable::xtable(cluster.zvalues, digits=4), include.rownames=TRUE, 
      file = here("output/tex/", "cluster_zvalues.tex"))

## Homogenitaetsindex nach BACHER(318) berechnen
cluster.homo <- 1-(cluster.sd[,-1]^2 / var.sd^2)
cluster.homo <- t(cluster.homo)
colnames(cluster.homo) <- t(cluster.means[,1])
print(xtable::xtable(cluster.homo, digits=4), include.rownames=TRUE, 
      file = here("output/tex/", "cluster_homo.tex"))


# Get cluster assignment --------------------------------------------------

## Welche Laender sind in welchem Cluster?
cl <- c("A", "B", "C", "D", "E")
for (i in 1:5) {print(data[wald.gruppe == cl[i],c(1:3,18)])
                print(nrow(data[wald.gruppe == cl[i],c(1:3,18)]))
}
for (i in 1:5) {
print(paste(data[wald.gruppe == cl[i],c(1)], collapse=", "))}

(dat <- data[wald.gruppe == "E",]$ANDIFF.00.10.rel)
quantile(dat, seq(0,1,.25))
mean(dat)
dat[which(dat<=0)]
summary(dat)
names(which.max(table(data$CPI2012)))
detach(data)
