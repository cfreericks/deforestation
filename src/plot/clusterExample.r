###############################################################################

opar <- par(no.readonly = TRUE)
# par(opar)

###############################################################################

library(cluster)

###############################################################################
## Normalverteilte Zufallszahlen

cl1 <- matrix(rnorm(200, -6, 1), 100,2)
cl3 <- matrix(rnorm(200, 4, 2), 100,2)
cl2 <- matrix(c(rnorm(100, -7, .5), rnorm(100, 6, 1.5)), 100,2)

data <- rbind(cl1, cl2, cl3)

###############################################################################
## Punkte plotten

outfile <- here("output/plots/", "clusterExample.eps")
postscript(file = outfile
                      , horizontal = FALSE
                      , width = 4, height = 4
                      , pointsize = 9)

plot(cl1, xlim = c(-10,10), ylim = c(-10,10), xlab ="x", ylab = "y", col = "black", 
     cex= .5, cex.lab = .8, cex.axis = .8)
points(cl2, col="red", pch = 2, cex = .5)
points(cl3, col="blue", pch = 3, cex = .5)

## Koordinatenkreuz
abline(h=0, v=0)

## Elisoide um die Cluster
lines(predict(ellipsoidhull(cl1)))
lines(predict(ellipsoidhull(cl2)))
lines(predict(ellipsoidhull(cl3)))

## Legende
legend("bottomright", legend = c("Cluster 1", "Cluster 2", "Cluster 3"), 
       pch = c(1, 2, 3), col = c("black", "red", "blue"), cex = .8)

dev.off()