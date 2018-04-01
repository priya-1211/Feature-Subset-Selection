# For Normality Test
library( MVN )
D1.stat<-mvn(D1.RRE.result[[3]],
             mvnTest = "mardia", 
             univariateTest = "SW",
             univariatePlot = "qq",multivariatePlot = "qq",
             multivariateOutlierMethod = "quan",
             showOutliers = "TRUE",
             showNewData = "FALSE")
library( roahd )
x<-Dataset
N = nrow(x)
P = ncol(x)
t0 = 0
t1 = 1
grid = seq(t0, t1, length.out = N)
mfD = mfData( grid, x )
fbplot(mfD, Depths = "MBD", Fvalue = 2.5, adjust = FALSE,
       display = TRUE, xlab = NULL, ylab = NULL, main = NULL)
outliergram(fD,display = TRUE)
fbplot(mfD,adjust = FALSE,display = TRUE, xlab = NULL, ylab = NULL, main = NULL)
