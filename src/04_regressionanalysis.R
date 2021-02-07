# Run regression analysis on the clustered data


# Global settings ---------------------------------------------------------

# Default graphic settings
opar <- par(no.readonly = TRUE)


# Load libraries ----------------------------------------------------------

library(lmtest) # Regressionsanalyse

library(car) # besserer DW-Test

library(xtable) # Tabellen zu LaTeX exportieren

library(scatterplot3d) # 3D Plots

library(RColorBrewer) # Farbpaletten fuers Plotten


# Load data ---------------------------------------------------------------

data <- read.table(here("output/data/", "ITEMSGROUPEDWITHFACTORS.csv")
                   , header = TRUE
                   , sep = "\t"
                   , quote = "\"'"
                   , dec = "."
                   , check.names = TRUE
                   , na.strings = c("NA", "#VALUE!")
)


# Transform data ----------------------------------------------------------

# Auswahl: Nur Entwaldungsrate, Faktorwerte und Gruppierungsvariable (+ISO3)
# Entwaldung muss noch standardisiert werden

attach(data)
data <- data.frame(ISO3, wald.gruppe, ANDIFF.00.10.rel, PA1, PA2, PA3)

# Standardisieren
data[,3] <- scale(data[,3])

# Mittelwert muss 0, SD = 1 sein
apply(data[,-c(1:2)], 2, mean)
apply(data[,-c(1:2)], 2, sd)

detach(data)


# Linear regression model -------------------------------------------------

attach(data)

# Build linear model for each cluster
lm.list <- list(
  lm(ANDIFF.00.10.rel ~ PA1 + PA2 + PA3, data = data),
  lm(ANDIFF.00.10.rel ~ PA1 + PA2 + PA3, data = data[wald.gruppe == "A", ]),
  lm(ANDIFF.00.10.rel ~ PA1 + PA2 + PA3, data = data[wald.gruppe == "B", ]),
  lm(ANDIFF.00.10.rel ~ PA1 + PA2 + PA3, data = data[wald.gruppe == "C", ]),
  lm(ANDIFF.00.10.rel ~ PA1 + PA2 + PA3, data = data[wald.gruppe == "D", ]),
  lm(ANDIFF.00.10.rel ~ PA1 + PA2 + PA3, data = data[wald.gruppe == "E", ])
)

detach(data)


# Regression results ------------------------------------------------------

my.lm.coefficients <- matrix(ncol = 6)

cluster <- c("all", "A", "B", "C", "D", "E")

for (i in 1:6) {
  coeff <- summary(lm.list[[i]])$coefficients
  my.rownames <- rownames(coeff)
  my.lm.coefficients <- rbind(my.lm.coefficients,
                              cbind(
                                summary(lm.list[[i]])$coefficients,
                                rep(cluster[i], 4),
                                my.rownames
                              ))
}

my.lm.coefficients <- my.lm.coefficients[-1,c(5,6,1,2,3,4)]
my.lm.coefficients.bak <- my.lm.coefficients
head(my.lm.coefficients)

my.lm.coefficients[, c(3:6)] <-
  sprintf("%.6G", as.numeric(my.lm.coefficients[, c(3:6)]))

print(xtable::xtable(my.lm.coefficients), include.rownames = FALSE, 
      file = here("output/tex/", "lm_results.tex"))


# Metrics and test statistics ---------------------------------------------

my.lm.test <- matrix(ncol = 5)

# R^2, adj. R^2, F-Statistik mit p-Wert ermitteln
for (i in 1:6) {
  (x <- summary(lm.list[[i]]))
  r.sq.mult <- x$r.squared
  r.sq.adj <- x$adj.r.squared
  f.val <- x$fstatistic
  p.val <-
    pf(x$fstatistic[1], x$fstatistic[2], x$fstatistic[3], lower.tail = FALSE)
  newrow <- cbind(cluster[i], r.sq.mult, r.sq.adj, f.val[1], p.val)
  my.lm.test <- rbind(my.lm.test, newrow)
}

my.lm.test <- my.lm.test[-1,]
my.lm.test[,c(2:3)] <- sprintf("%.3G", as.numeric(my.lm.test[,c(2:3)]))
my.lm.test[,c(4)] <- sprintf("%.4G", as.numeric(my.lm.test[,c(4)]))
my.lm.test[,c(5)] <- sprintf("%.6G", as.numeric(my.lm.test[,c(5)]))

print(xtable::xtable(my.lm.test), include.rownames = FALSE, 
      file = here("output/tex/", "lm_rsquared.tex"))


# Plot residuals ----------------------------------------------------------

postscript(file = here("output/plots/", "residualPlots.eps")
           , horizontal = FALSE
           , width = 7, height = 7
           , pointsize = 7.5)

par(mar = c(4.1, 4.1, 2.1, 1.1),
    oma = c(1, 1, 1, 1),
    cex.lab = 1
    )
layout(matrix(1:12, 6, 2, byrow = TRUE))

my.lab <- c("ANDIFF.00.10.rel ~ PA1 + PA2 + PA3 (alle)",
            "ANDIFF.00.10.rel ~ PA1 + PA2 + PA3 (A)",
            "ANDIFF.00.10.rel ~ PA1 + PA2 + PA3 (B)",
            "ANDIFF.00.10.rel ~ PA1 + PA2 + PA3 (C)",
            "ANDIFF.00.10.rel ~ PA1 + PA2 + PA3 (D)",
            "ANDIFF.00.10.rel ~ PA1 + PA2 + PA3 (E)"
            )

for (i in 1:6) {
  my.lm <- lm.list[[i]]
  
  # Histogramm der Residuen
  res <- my.lm$residuals
  h <- hist(res,
            xlab = "Residuen",
            ylab = "Häufigkeit",
            main = my.lab[i]
            )
  
  xfit <- seq(min(res), max(res), length = 40)
  yfit <- dnorm(xfit, mean = mean(res), sd = sd(res))
  yfit <- yfit * diff(h$mids[1:2]) * length(res)
  lines(xfit, yfit, lwd = 1)
  
  # Plot der Residuen gegen die fitted values
  plot(my.lm$residuals ~ my.lm$fitted.values,
       ylab = "Residuen",
       xlab = "Geschätzte Werte",
       main = my.lab[i]
       )
  
  abline(h = 0)
}

par(opar)
dev.off()


# Check BLUE criteria -----------------------------------------------------

blue.test <- data.frame(matrix(vector(), 
                               nrow = 0, 
                               ncol = 8,
                               dimnames = list(
                                 c(),
                                 c("Cluster", "n", "W", "pval1", "BP", "pval2", 
                                   "DW", "pval3")
                                 )
                               ), 
                        stringsAsFactors = FALSE
                        )

for (i in 1:6) {
  my.lm <- lm.list[[i]]
  n <- length(my.lm$fitted.values)
  
  #  Normaliitaet der Residuen
  (my.shapiro <- shapiro.test(my.lm$residuals))
  
  # Breusch-Pagan Test auf Heteroskedastizitaet
  (my.bp <- bptest(my.lm))
  
  # Durbin-Watson Test auf Autokorrelation
  # (my.dw <- dwtest(my.lm, alternative = "two.sided"))
  (my.dw <- durbinWatsonTest(my.lm, alternative = "two.sided"))
  
  # in dataframe schreiben
  blue.test[i,] <- cbind(cluster[i],
                         n,
                         my.shapiro$statistic,
                         my.shapiro$p.value,
                         my.bp$statistic,
                         my.bp$p.value,
                         my.dw$dw,
                         my.dw$p
                         )
}

blue.test[, 3] <-
  format(as.numeric(blue.test[, 3]), digits = 2, nsmall = 3)
blue.test[, 5] <-
  format(as.numeric(blue.test[, 5]), digits = 2, nsmall = 3)
blue.test[, 7] <-
  format(as.numeric(blue.test[, 7]), digits = 2, nsmall = 3)

blue.test[, 4] <-
  format(as.numeric(blue.test[, 4]), digits = 5, nsmall = 3)
blue.test[, 6] <-
  format(as.numeric(blue.test[, 6]), digits = 5, nsmall = 3)
blue.test[, 8] <-
  format(as.numeric(blue.test[, 8]), digits = 5, nsmall = 3)


print(xtable::xtable(blue.test), file = here("output/tex/", "lm_blue.tex"))


# 3D Scatter plot ---------------------------------------------------------

postscript(file = here("output/plots/", "groupedScatter3D.eps")
           , horizontal = FALSE
           , width = 7, height = 7
           , pointsize = 7.5)

my.col <- brewer.pal(5, "Set1")[data$wald.gruppe]
s3d <- with(data, scatterplot3d(x = data[,4], y = data[,5], z = data[,6]
                                , type = "h", pch = 19
                                , color = my.col
                                , xlab = "Bevölkerungsentwicklung"
                                , ylab = "Viehwirtschaft"
                                , zlab = "Bioenergie")) 

group.names <- levels(data$wald.gruppe)
group.counts <- as.vector(table(data$wald.gruppe))
my.legend <- paste(group.names, " (n = ", group.counts, ")", sep = "")

leg.col <- c(levels(as.factor(my.col))[4], levels(as.factor(my.col))[1]
             , levels(as.factor(my.col))[2], levels(as.factor(my.col))[3]
             , levels(as.factor(my.col))[5])


legend(s3d$xyz.convert(3, 0, 1),
       xjust = 0,
       yjust = 0,
       legend = my.legend,
       title = "Cluster",
       col = leg.col,
       pch = 19,
       horiz = FALSE,
       cex = 0.95
       )

dev.off()
