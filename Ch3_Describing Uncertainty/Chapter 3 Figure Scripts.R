# R Code for Chapter 3, Describing Uncertainty

#######################################################################
# Code for Figure 3.1 and Table 3.1
# Ten randomly selected data sets of 12 samples each
# from a normal distribution with mean 5, variance 1
# 
######################################################################
set.seed(3789)
n <- 12
df <- n - 1
outMatrix <- as.data.frame(matrix(ncol = 4, nrow = 10))
colnames(outMatrix) <- c("xbar", " sd ", "lcl", "ucl")
for (i in 1:10) {
  x <- rnorm(n, mean = 5, sd = 1)
  xbar <- mean(x)
  xvar <- var(x)
  stdev <- sqrt(xvar)
  lcl <- xbar + qt(0.05, df) * sqrt(xvar / n)
  ucl <- xbar + qt(0.95, df) * sqrt(xvar / n)
  outMatrix[i,] <- c(xbar, stdev, lcl, ucl)
}
print(outMatrix, digits = 3)
##########################################################################
# Figure 3.1
##########################################################################

pdf("Figure 3.1.pdf", height = 8, width = 8, pointsize = 10)
y <- seq(10, 1, -1)
x <- outMatrix[,1]
plot(x = x, y = y, xlim = c(4, 6), xaxs = "i", yaxs = "i", ylim = c(0, 11), 
     pch = 19, cex = 1.5, axes = FALSE, xlab = "Chloride concentration, in milligrams per liter", 
     ylab = "", cex.lab = 1.2)
abline(v = 5, lty = "dashed", lwd = 2)
axis(1, at = seq(4, 6, 0.2), labels = TRUE, tck = 0.02, cex.axis = 1.2)
axis(2, at = 0:11, labels = NA, tck = 0)
axis(3, at = seq(4, 6, 0.2), labels = FALSE, tck = 0.02)
axis(4, at = 0:11, labels = NA, tck = 0)
for (i in 1:10) {
  segments(outMatrix[i, 3], y[i], outMatrix[i, 4], y[i], lwd = 3)
}
dev.off()

##########################################################################
# Figure 3.2
##########################################################################

pdf("Figure 3.2.pdf", height = 6, width = 6, pointsize = 10)
set.seed(7634)
# mean of 1, coefficient of variation of 1.0
# that is equivalent to a mean log value of -0.5 and 
# standard deviation of the log values of 1.0
x <- rlnorm(1000, meanlog = -0.125, sdlog = 0.5)
xmax <- ceiling(max(x))
par(tck = 0.02)
boxplot(x, ylim = c(0, xmax), yaxs = "i", las = 1,ylab = "", xlab = "")
abline(h = 1)
axis(4, at = seq(0, 4, 1), labels = NA, tck = 0.02)
dev.off()

##########################################################################
# Figure 3.3
##########################################################################

# start by defining a function for the confidence interval
meanCI <- function(xbar, sd, n, alpha){
  tval <- qt(alpha/2, n - 1)
  oneSide <- tval * sd / sqrt(n)
  low <- xbar + oneSide
  high <- xbar - oneSide
  ciVals <- c(low, high)
  return(ciVals)
}
##
# now generate 10 sets of 12 values from a log normal
# mean of 1, coefficient of variation of 1.0
# that is equivalent to a mean log value of -0.5 and 
# standard deviation of the log values of 1.0
# produces a table version of the information shown in fig 3.3, not shown in text
set.seed(100105)
cat("\n   Mean  St.Dev.  90% Confidence Interval")
xLow <- rep(NA, 10)
xHigh <- rep(NA, 10)
xMean <- rep(NA, 10)
for (i in 1:10) {
  x <- rlnorm(12, meanlog = -0.5, sdlog = 1)
  xbar <- mean(x)
  xMean[i] <- xbar
  sd <- sd(x)
  ciVals <- meanCI(xbar, sd, n = 12, alpha = 0.1)
  xLow[i] <- ciVals[1]
  xHigh[i] <- ciVals[2]
  fmean <- format(xbar, digits = 3, width = 7)
  fsd <- format(sd, digits = 3, width = 7)
  symbol <- if (ciVals[2] < 1 | ciVals[1] > 1) "*" else " "
  fci <- paste("  ", symbol, "(", format(ciVals[1], digits = 3, width = 5),"  to  ",
               format(ciVals[2], digits = 3, width = 5), ")", sep = "")
  cat("\n",fmean,fsd,fci)
}

pdf("Figure 3.3.pdf", height = 6, width = 6, pointsize = 10)
y <- seq(10, 1, -1)
plot(x = xMean, y = y, xlim = c(0.0, 2.5), xaxs = "i", ylim = c(0, 11), 
     axes = FALSE, pch = 19, cex = 1.5, xlab = "Chloride concentration, in milligrams per liter", 
     ylab = "", yaxs = "i")
abline(v = 1, lty = "dashed", lwd = 2)
axis(1, at = seq(0.0, 2.5, 0.5), labels = TRUE, tck = 0.02)
axis(2, at = 0:11, labels = FALSE, tck = 0)
axis(3, at = seq(0.0, 2.5, 0.5), labels = FALSE, tck = 0.02)
axis(4, at = 0:11, labels = FALSE, tck = 0)
for (i in 1:10) {
  segments(xLow[i], y[i], xHigh[i], y[i], lwd = 3)
}
dev.off()


##########################################################################
# Figure 3.5
##########################################################################

pdf("Figure 3.5.pdf", height = 5, width = 8, pointsize = 12)
x <- c(1.3, 1.5, 1.8, 2.6, 2.8, 3.5, 4.0, 4.8, 8.0, 9.5, 12, 14, 19, 23, 
       41, 80, 100, 110, 120, 190, 240, 250, 300, 340, 580)
y <- c(rep(2, 25))
plot( y ~ x , log = "x", xlim = c(1, 1000), ylim = c(0, 6), bty = "n",  
      yaxt = "n", ylab = "", xlab = "Arsenic concentration, in parts per billion", 
      pch = 20, main = "",  xaxs = "i", yaxs = "i", tck = 0.02, axes = FALSE)
verticies.x1 <- c(0.1, 0.1, 4.79, 4.79)
verticies.y1 <- c(0.01, 6, 6, 0.01)
verticies.x2 <- c(100.1, 100.1, 1,000, 1,000)
verticies.y2 <- c(0.01, 6, 6, 0.01)
polygon(verticies.x1, verticies.y1, col = grey(0.90), border = NA)
polygon(verticies.x2, verticies.y2, col = grey(0.90), border = NA)
axis(1, at = c(1, 5, 10, 50, 100, 500, 1000), labels = c(1, 5, 10, 50, 100, 500, "1,000"), 
     tck = 0.02)
axis(2, at = c(0, 6), labels = NA, tck = 0)
axis(3, at = c(1, 5, 10, 50, 100, 500, 1000), labels = NA, tck = 0.02)
axis(4, at = c(0, 6), labels = NA, tck = 0)
points(x, y, pch = 20)
segments(x0 = 4.8, y0 = -0.5, x1 = 4.8, y1 = 2.75)
segments(x0 = 4.8, y0 = 3.25, x1 = 4.8, y1 = 6.5)
segments(x0 = 100, y0 = -0.5, x1 = 100, y1 = 2.75)
segments(x0 = 100, y0 = 3.25, x1 = 100, y1 = 6.5)
arrows(x0 = 1, y0 = 4, x1 = 4.8, y1 = 4, code = 2, length = 0.1)
arrows(x0 = 4.8, y0 = 4, x1 = 100, y1 = 4, code = 3, lwd = 3)
arrows(x0 = 100, y0 = 4, x1 = 1000, y1 = 4, code = 1, length = 0.1)
segments(x0 = 19, y0 = -0.5, x1 = 19, y1 = 2.5)
text(4.8, 3, expression("c"["low"]))
text(19, 3, "Sample\nmedian")
text(100, 3, expression("c"["up"]))
text(2, 5, expression(paste(frac(alpha, "2"))))
text(19, 5, expression(paste("1 - ", alpha)))
text(300, 5, expression(paste(frac(alpha, "2"))))
dev.off()

##########################################################################
# Example 3.2 confidence limits on the median.
##########################################################################

x <- c(1.2, 1.5, 1.8, 2.6, 2.8, 3.5, 4.0, 4.8, 8.0, 9.5, 12, 14, 19, 23, 
       41, 80, 100, 110, 120, 190, 240, 250, 300, 340, 580)
indexRanks <- qbinom(c(0.025, 0.975), length(x), prob = 0.5)
indexRanks
x <- sort(x) # not actually needed here because values are sorted already
x[indexRanks]
sum(dbinom(8:17, 25, 0.5))

##########################################################################
# Figure 3.4 Boxplots of arsenic data and logs of arsenic data
##########################################################################
pdf("Figure 3.4.pdf", width = 10, height = 6, pointsize = 10)
par(mfrow = c(1, 2))
par(las = 1, tck = 0.02, xaxs = "i", yaxs = "i", oma = c(5.1, 2.1, 0.1, 2.5)) 
#c(bottom, left, top, right) 
x <- c(1.2, 1.5, 1.8, 2.6, 2.8, 3.5, 4.0, 4.8, 8.0, 9.5, 12, 14, 19, 23, 
       41, 80, 100, 110, 120, 190, 240, 250, 300, 340, 580)
boxplot(x, ylim = c(0, 600), ylab = "Arsenic concentration, in parts per billion")
myText <- expression(paste(bolditalic("A."), bold(" Concentration")))
mtext(text = myText, line = 0.7, adj = 0, cex = 1.2, 
      font = 2)      
axis(2)
axis(4, labels = FALSE)
y <- log(x)
boxplot(y, ylim = c(0, 7), ylab = "", tck = 0.02)
myText <- expression(paste(bolditalic("B."), bold(" ln(Concentration)")))
mtext(text = myText, line = 0.7, adj = 0, cex = 1.2, 
      font = 2)      
ytickLabs <- c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)
yticks <- log(ytickLabs)
axis(2)
axis(4, at = yticks, labels = formatC(ytickLabs, big.mark = ",", format = "d"),
     las = 1, cex.axis = 1.0, tck = 0.02)
mtext("ln(Arsenic concentration, in parts per billion)", side = 2, las = 3, 
      line = 2.5)
mtext("Arsenic concentration, in parts per billion", side = 4, las = 3, line = 3)
dev.off()
##########################################################################
# Example  first the symmetric interval on the geometric mean
##########################################################################

y <- log(x)
ybar <- mean(y)
sy <- sd(y)
GM <- exp(ybar)
cat("\nmean of y is ", ybar, "standard deviation of y is ", 
    sy,"\nGeometric Mean is ", GM)
clow <- exp(ybar - qt(0.975, 24) * sqrt((sy^2 / 25)))
cup <- exp(ybar + qt(0.975, 24) * sqrt((sy^2 / 25)))
cat("\nclow is ", clow, " cup is ", cup)


##########################################################################
# Example  continued for the symmetric interval for the mean
##########################################################################

xbar <- mean(x)
sx <- sd(x)
cat("\nmean of x is ", xbar, " standard deviation of x is ", sx)
clow <- xbar - qt(0.975, 24) * sqrt((sx^2 / 25))
cup <- xbar + qt(0.975, 24) * sqrt((sx^2 / 25))
cat("\nclow is ", clow, " cup is ", cup)


##########################################################################
# Figure 3.6 - Bootstrap confidence intervals
##########################################################################

pdf("Figure 3.6.pdf", height = 8, width = 8, pointsize = 12)
x <- c(1.3, 1.5, 1.8, 2.6, 2.8, 3.5, 4.0, 4.8, 8, 9.5, 12, 14, 19, 23, 41, 80, 100, 
       110, 120, 190, 240, 250, 300, 340, 580)
library(boot)
mean1 <- function(x, i){mean(x[i])
}
set.seed(1)
boot1 <- boot(x, statistic = mean1, R = 2000)
boot.ci(boot1, conf = 0.95, type = "perc") # note, result will not be identical to text
hist(boot1$t, xlab = "Bootstrap mean values", xaxs = "i", las = 1, yaxs = "i",
     xlim = c(0, 250), ylim = c(0, 600), main = "", density = 50, tck = 0)
axis(1, at = c(0, 250), labels = NA, tck = 0)
axis(2, at = seq(0, 600, 100), labels = NA, tck = 0.02)
axis(3, at = c(0, 250), labels = NA, tck = 0.02)
axis(4, at = seq(0, 600, 100), labels  = NA, tck = 0.02)
dev.off()


##########################################################################
# Figure 3.7 - Show a hypothetical distution and the prediction intervals, two sided
##########################################################################

pdf("Figure 3.7.pdf", height = 6, width = 8, pointsize = 15)
xvals <- seq(0.01, 6, 0.01)
density <- dlnorm(xvals, meanlog = 0, sdlog = 0.6) # probability density function
#  for the log normal with mean log of 0 and standard deviation of logs of 0.6
plot(xvals, density, type = "l", lwd = 2, xlim = c(0, 5), xaxs = "i", ylim = c(0, 1),
     yaxs = "i", axes = FALSE, xlab = "Values", ylab = "Probability density", las = 1)
axis(side = 1, labels = FALSE, lwd.ticks = 0.0)
axis(side = 2, at = c(0, 1), labels = FALSE, lwd.ticks = 0.0)
axis(side = 3, labels = FALSE, lwd.ticks = 0.0)
axis(side = 4, at = c(0, 1), labels = FALSE, lwd.ticks = 0.0)
lowPI <- qlnorm(0.05, meanlog = 0, sdlog = 0.6)
highPI <- qlnorm(0.95, meanlog = 0, sdlog = 0.6)
xy <- data.frame(xvals,density)
lower <- subset(xy, xvals <= lowPI)
x <- c(0, lower$xvals, lowPI, 0)
y <- c(0, lower$density, 0, 0)
polygon(x, y, density = 30)
upper <- subset(xy, xvals >= highPI)
x <- c(upper$xvals, highPI, highPI)
y <- c(upper$density, 0, upper$density[1])
polygon(x, y, density = 30)
mid <- (lowPI + highPI) / 2
arrows(lowPI, 0.85, highPI, 0.85, lwd = 2, code = 3)
text(mid, 0.91, labels = "90-percent prediction interval", cex = 0.9)
dev.off()


##########################################################################
# Figure 3.8
##########################################################################

pdf("Figure 3.8.pdf", height = 6, width = 8, pointsize = 10)
load("James.Q.RData")
par(tck = 0.02)
hist(Q, main = "", xlab = "Annual mean discharge, in cubic meters per second",
     ylab = "Frequency", xlim = c(0, 400), xaxs = "i", ylim = c(0, 50), yaxs = "i", 
     las = 1, cex.axis = 1.2, cex.lab = 1.1, cex = 1.2)
predInt <- quantile(Q, prob = c(0.01, 0.99), type = 6)
predInt
segments(predInt[1], 0, predInt[1], 35, lwd = 3, lty = "dashed")
segments(predInt[2], 0, predInt[2], 35, lwd = 3, lty = "dashed")
qMedian <- median(Q)
qMedian
segments(qMedian, 0, qMedian, 35, lwd = 3)
arrows(predInt[1], 38, predInt[2], 38, lwd = 3, code = 3)
midInt <- (predInt[1] + predInt[2]) / 2
text(midInt, 41, labels = "98-percent prediction interval", cex = 1.2)
axis(3, at = seq(0, 400, 100), labels = NA)
axis(4, at = seq(0, 50, 10), labels = NA)
par(tck = NA)
dev.off()


##########################################################################
# Figure 3.9 - Show a hypothetical distribution and the prediction intervals, one sided
##########################################################################

pdf("Figure 3.9.pdf", height = 6, width = 8, pointsize = 15)
xvals <- seq(0.01, 6, 0.01)
density <- dlnorm(xvals, meanlog = 0, sdlog = 0.6) # probability density function
# for the log normal with mean log of 0 and standard deviation of logs of 0.6
plot(xvals, density, type = "l", lwd = 2, xlim = c(0, 5), xaxs = "i", ylim = c(0, 1),
     yaxs = "i", axes = FALSE, xlab = "Values", ylab = "Probability density", las = 1)
axis(side = 1, labels = FALSE, lwd.ticks = 0.0)
axis(side = 2, at = c(0,1), labels = FALSE, lwd.ticks = 0.0)
axis(side = 3, labels = FALSE, lwd.ticks = 0.0)
axis(side = 4, at = c(0, 1), labels = FALSE, lwd.ticks = 0.0)
highPI <- qlnorm(0.90, meanlog = 0, sdlog = 0.6)
xy <- data.frame(xvals, density)
upper <- subset(xy, xvals >= highPI)
x <- c(upper$xvals, highPI, highPI)
y <- c(upper$density, 0, upper$density[1])
polygon(x, y, density = 30)
mid <- (highPI) / 2
arrows(0.05, 0.85, highPI, 0.85, lwd = 2, code = 3)
text(mid, 0.91, labels = "90-percent prediction interval", cex = 0.9)
dev.off()



##########################################################################
# Figure 3.10
##########################################################################

library(EnvStats)
load("CuyahogaRiverTDS.RData")
pdf("Figure 3.10.pdf", height = 6, width = 8, pointsize = 10)
tds <- cuya.tds$tds_mgL
name <- "Total dissolved solids, in milligrams per liter"
cover <- 0.90
conf <- 0.95
ret.list1 <- tolIntLnorm(tds,coverage = cover,conf.level = conf, ti.type = "two-sided")
LTL <- as.numeric(ret.list1$interval$limits[1])
UTL <- as.numeric(ret.list1$interval$limits[2])

ret.list2 <- eqlnorm(tds, p = cover, ci = TRUE, ci.type = "two-sided", 
                     conf.level = conf)
LCL <- as.numeric(ret.list2$interval$limits[1])
UCL <- as.numeric(ret.list2$interval$limits[2])
PCTL <- as.numeric(ret.list2$"quantiles")

X <- c(LCL,UCL)
Y <- c(.25,.25)
Z <- c(.75,.75)
TI <- c(LTL,UTL)

par(las = 1, tck = 0.02, xaxs = "i", yaxs = "i")
plot(X, Y, yaxt = 'n', xlim = c(0, 700), ylim = c(0, 1), type = "l", lwd = 1 ,
     lty = "dashed", ylab = "", xlab = name)
abline(v = PCTL, lwd = 3)
par(new = T)
plot(TI, Z, axes = F, xlim = c(0, 700), ylim = c(0, 1), type = "l", lwd =  1, 
     ylab = " ", xlab = " ")
text(LCL, 0.25, "95-percent confidence interval\n on the on 90th percentile    ", pos = 2, cex = 1.1)
text(LTL, 0.75, "90-percent tolerance\n interval with\n 95-percent confidence", pos = 2, cex = 1.1)
text(PCTL, 0.5, "        90th  percentile", pos = 1, cex = 1.1)
par(new = F)
dev.off()

##########################################################################
# Figure 3.11
##########################################################################

pdf("Figure 3.11.pdf", height = 6, width = 8, pointsize = 10)
par(mfrow = c(2, 1), xaxs = "i", yaxs = "i")
plot(1000, 1000 , xlim = c(0, 60), ylim = c(0, 6), bty = "l", 
     yaxt = "n", xaxt = "n", ylab = "", xlab = "Data value", 
     main = "",  xaxs = "i", yaxs = "i")
verticies.x1 <- c(0.1, 0.1, 9.9, 9.9)
verticies.y1 <- c(0.01, 4, 4, 0.01)
verticies.x2 <- c(50.1, 50.1, 60, 60)
verticies.y2 <- c(0.01, 4, 4, 0.01)
polygon(verticies.x1, verticies.y1, col = grey(0.90), border = NA)
polygon(verticies.x2, verticies.y2, col = grey(0.90), border = NA)
segments(x0 = 10.1, y0 = 1, x1 = 49.9, y1 = 1, lwd = 7, col = grey(0.50))
segments(x0 = 10, y0 = -0.5, x1 = 10, y1 = 2.5)
segments(x0 = 16, y0 = -0.5, x1 = 16, y1 = 2.5)
segments(x0 = 27, y0 = -0.5, x1 = 27, y1 = 2.5)
segments(x0 = 50, y0 = -0.5, x1 = 50, y1 = 2.5)
text(30, 3.25, "Interval estimate")
text(5, 5, expression(paste(frac(alpha, "2"))))
text(30, 5, expression(paste("95 percent = 1 - ", alpha)))
text(55, 5, expression(paste(frac(alpha,"2"))))
axis(1, at = c(0, 10, 16, 27, 50), labels = c("0", expression("X"["I"]), 
                                          expression("X"["0"]), 
                                          expression(hat(X)["p"]), 
                                          expression("X"["u"])), tcl = 0)
myText <- expression(paste(bolditalic("A.")))
axis(2, at = c(3), labels = myText, las = 1, tcl = 0)
axis(3, at = c(0, 60), labels = NA, las = 1, tcl = 0)
axis(4, at = c(0, 6), labels = NA, las = 1, tcl = 0)
plot(1000, 1000 , xlim = c(0, 90), ylim = c(0, 6), bty = "l", 
     yaxt = "n", xaxt = "n", ylab = "", xlab = "Data value", 
     main = "", xaxs = "i", yaxs = "i")
verticies.x1 <- c(0.1, 0.1, 23.9, 23.9)
verticies.y1 <- c(0.01, 4, 4, 0.01)
verticies.x2 <- c(78.1, 78.1, 90, 90)
verticies.y2 <- c(0.02, 4, 4, 0.02)
polygon(verticies.x1, verticies.y1, col = grey(0.90), border = NA)
polygon(verticies.x2, verticies.y2, col = grey(0.90), border = NA)
segments(x0 = 24.1, y0 = 1, x1 = 77.9, y1 = 1, lwd = 7, col = grey(0.50))
segments(x0 = 20, y0 = -0.5, x1 = 20, y1 = 2)
segments(x0 = 24, y0 = -0.5, x1 = 24, y1 = 2)
segments(x0 = 47, y0 = -0.5, x1 = 47, y1 = 2)
segments(x0 = 78, y0 = -0.5, x1 = 78, y1 = 2)
text(51, 3.25, "Interval estimate")
text(12, 5, expression(paste(frac(alpha, "2"))))
text(51, 5, expression(paste("95 percent = 1 - ", alpha)))
text(84, 5, expression(paste(frac(alpha, "2"))))
axis(1, at = c(0, 24, 20, 47, 78), labels = c("0", expression("X"["I"]), 
                                          expression("X"["0"]), 
                                          expression(hat(X)["p"]), 
                                          expression("X"["u"])), tcl = 0)
myText <- expression(paste(bolditalic("B.")))
axis(2, at = c(3), labels = myText, las = 1, tcl = 0)
axis(3, at = c(0, 90), labels = NA, las = 1, tcl = 0)
axis(4, at = c(0, 6), labels = NA, las = 1, tcl = 0)
par(mfrow = c(1, 1))
dev.off()


##########################################################################
# Figure 3.12
##########################################################################

pdf("Figure 3.12.pdf", height = 6, width = 8, pointsize = 10)
par(mfrow = c(2, 1), xaxs = "i", yaxs = "i")
plot(1000, 1000 , xlim = c(0, 15), ylim = c(0, 6), bty = "l", 
     yaxt = "n", xaxt = "n", ylab = "", xlab = "Data value", 
     main = "", xaxs = "i", yaxs = "i")
verticies.x1 <- c(0.04, 0.04, 4.65, 4.65)
verticies.y1 <- c(0.01, 4, 4, 0.01)
polygon(verticies.x1, verticies.y1, col = grey(0.90), border = NA)
arrows(x0 = 4.67, y0 = 1, x1 = 14.9, y1 = 1, lwd = 7, col = grey(0.50), code = 2, 
       length = 0.1)
segments(x0 = 4.66, y0 = -0.5, x1 = 4.66, y1 = 2)
segments(x0 = 7.33, y0 = -0.5, x1 = 7.33, y1 = 2)
segments(x0 = 12.2, y0 = -0.5, x1 = 12.2, y1 = 2)
text(8, 3.25, "Interval estimate")
text(2.33, 5, expression(alpha))
text(8, 5, expression(paste("1 - ", alpha)))
axis(1, at = c(0, 4.66, 7.33, 12.2), tcl = 0, labels = c("0", expression("X"["I"]), 
                                                   expression("X"["0"]), 
                                                   expression(hat(X)["p"])))
myText <- expression(paste(bolditalic("A.")))
axis(2, at = c(3), labels = myText, las = 1, tcl = 0)
axis(3, at = c(0, 15), labels = NA, las = 1, tcl = 0)
axis(4, at = c(0, 6), labels = NA, las = 1, tcl = 0)
plot(1000, 1000 , xlim = c(0, 19), ylim = c(0, 6), bty = "l", 
     yaxt = "n", xaxt = "n", ylab = "", xlab = "Data value", 
     main = "", xaxs = "i", yaxs = "i")
verticies.x1 <- c(0.04, 0.04, 8.79, 8.79)
verticies.y1 <- c(0.01, 4, 4, 0.01)
polygon(verticies.x1, verticies.y1, col = grey(0.90), border = NA)
arrows(x0 = 8.81, y0 = 1, x1 = 18.8, y1 = 1, lwd = 7, col = grey(0.50), code = 2, 
       length =  0.1)
segments(x0 = 7.2, y0 = -0.5, x1 = 7.2, y1 = 2)
segments(x0 = 8.8, y0 = -0.5, x1 = 8.8, y1 = 2)
segments(x0 = 15.25, y0 = -0.5, x1 = 15.25, y1 = 2)
text(14, 3.25, "Interval estimate")
text(4.4, 5, expression(alpha))
text(14, 5, expression(paste("1 - ", alpha)))
axis(1, at = c(0, 8.8, 7.2, 15.25), labels = c("0", expression("X"["I"]), 
                                           expression("X"["0"]), 
                                           expression(hat(X)["p"])), tcl = 0)
myText <- expression(paste(bolditalic("B.")))
axis(2, at = c(3), labels = myText, las = 1, tcl = 0)
axis(3, at = c(0,19), labels = NA, las = 1, tcl = 0)
axis(4, at = c(0,6), labels = NA, las = 1, tcl = 0)
par(mfrow = c(1, 1))
dev.off()


##########################################################################
# Figure 3.13
##########################################################################

pdf("Figure 3.13.pdf", height = 6, width = 8, pointsize = 10)
par(mfrow = c(2,1), xaxs = "i", yaxs = "i")
plot(1000, 1000 , xlim = c(0,17), ylim = c(0,6), bty = "l", 
     yaxt = "n", xaxt = "n", ylab = "", xlab = "Data value", 
     main = "", xaxs = "i", yaxs = "i")
verticies.x1 <- c(13.51, 13.51, 17, 17)
verticies.y1 <- c(0.01, 4, 4, 0.01)
polygon(verticies.x1, verticies.y1, col = grey(0.90), border = NA)
segments(x0 = 0.1, y0 = 1, x1 = 13.49, y1 = 1, lwd = 7, col = grey(0.50))
segments(x0 = 5.8, y0 = -0.5, x1 = 5.8, y1 = 2.5)
segments(x0 = 10.2, y0 = -0.5, x1 = 10.2, y1 = 2.5)
segments(x0 = 13.5, y0 = -0.5, x1 = 13.5, y1 = 2.5)
text(5.8, 3.25, "Interval estimate")
text(5.8, 5, expression(paste("1 - ", alpha)))
text(15.25, 5, expression(alpha))
axis(1, at = c(0, 10.2, 5.8, 13.5), labels = c("0", expression("X"["0"]), 
                                           expression(hat(X)["p"]), 
                                           expression("X"["u"])), tcl = 0)
myText <- expression(paste(bolditalic("A.")))
axis(2, at = c(3), labels = myText, las = 1, tcl = 0)
axis(3, at = c(0,17), labels = NA, las = 1, tcl = 0)
axis(4, at = c(0,6), labels = NA, las = 1, tcl = 0)
plot(1000, 1000 , xlim = c(0, 17), ylim = c(0, 6), bty = "l", 
     yaxt = "n", xaxt = "n", ylab = "", xlab = "Data value", 
     main = "", xaxs = "i", yaxs = "i")
verticies.x1 <- c(9.11, 9.11, 17, 17)
verticies.y1 <- c(0.02, 4, 4, 0.02)
polygon(verticies.x1, verticies.y1, col = grey(0.90), border = NA)
segments(x0 = 0.1, y0 = 1, x1 = 9.09, y1 = 1, lwd = 7, col = grey(0.50))
segments(x0 = 4.5, y0 = -0.5, x1 = 4.5, y1 = 2.5)
segments(x0 = 9.1, y0 = -0.5, x1 = 9.1, y1 = 2.5)
segments(x0 = 10.2, y0 = -0.5, x1 = 10.2, y1 = 2.5)
text(4.5, 3.25, "Interval estimate")
text(4.5, 5, expression(paste("1 - ", alpha)))
text(13.05, 5, expression(alpha))
axis(1, at = c(0, 10.2, 4.5, 9.1), labels = c("0", expression("X"["0"]), 
                                          expression(hat(X)["p"]), 
                                          expression("X"["u"])), tcl = 0)
myText <- expression(paste(bolditalic("B.")))
axis(2, at = c(3), labels = myText, las = 1, tcl = 0)
axis(3, at = c(0, 19), labels = NA, las = 1, tcl = 0)
axis(4, at = c(0, 6), labels = NA, las = 1, tcl = 0)
par(mfrow = c(1,1))
dev.off()

############################
# Figure 3.14
############################
set.seed(35)
x <- rnorm(25, mean = 0.1, sd = 0.7)
x <- exp(x)
xbar <- mean(x)
s <- sd(x)
sSquared <- s^2
piLow <- xbar + qt(0.025, 24) * sqrt((sSquared) + (sSquared / 25))
piHigh <- xbar + qt(0.975, 24) * sqrt((sSquared) + (sSquared / 25))
PI95 <- c(piLow, piHigh)
PI95
pdf(file = "Figure 3.14.pdf", height = 6, width = 5)
par(las = 1, tck = 0.02)
boxplot(x, ylim = c(-5, 15), yaxs = "i", ylab = "Lognormal random variable")
abline(h = PI95, lty = "dashed", lwd = 2)
abline(h = 0, lwd = 2)
dev.off()

