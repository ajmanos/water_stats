# R Code for Chapter 2, Graphical Data Analysis

##########################################################################
# Figure 2.1
#########################################################################

# Anscombe's quartet
# Famous datasets developed by F.J. Anscombe in 1973. 
# These four datasets each produce the same summary 
# statistics, including n, mean of the xs, mean of the ys, correlation, 
# regression coefficient, standard error.
# The dataset is in base R.

# setup data relations and compare how close the
# summary statistics are to each other

ff <- y ~ x
mods <- setNames(as.list(1:4), paste0("lm", 1:4))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y", "x"), i), as.name)
  mods[[i]] <- lmi <- lm(ff, data = anscombe)
  print(anova(lmi))
}
sapply(mods, coef)
lapply(mods, function(fm) coef(summary(fm)))

pdf("Figure 2.1.pdf", width = 6, height = 6, pointsize = 10)
# Now, do what should have done in first, plot the data
par(mfrow = c(2, 2), mar = 0.1 + c(4, 4, 1, 1), oma =  c(0, 0, 2, 0),
    xaxs = "i", yaxs = "i", tck = 0.02, las =1)
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y", "x"), i), as.name)
  plot(ff, data = anscombe, pch = 16,cex = 1.2,
       xlim = c(0, 20), ylim = c(0, 14))
  abline(mods[[i]], col = "grey50")
}
dev.off()


##########################################################################
# Figure 2.2
##########################################################################

load("James.Q.RData")

pdf("Figure 2.2.pdf", width = 8, height = 6, pointsize = 10)
hist(Q, main = "", xlab  = " Annual mean discharge, in cubic meters per second", 
     ylab = "Frequency", xlim = c(0, 400), xaxs = "i", ylim = c(0, 35), 
     yaxs = "i", las = 1, cex.axis = 1.2, cex.lab = 1.4, cex = 1.2, tck = 0, 
     bty = "o")
axis(1, at = c(0, 400), labels = NA, tck = 0)
axis(2, at = c(seq(0, 35, 5)), labels = NA, tck = 0.02)
axis(3, at = c(0, 400), labels = NA, tck = 0)
axis(4, at = c(seq(0, 35, 5)), labels = NA, tck = 0.02)
dev.off()

##########################################################################
# Figure 2.3
##########################################################################

# uses the data loaded for Figure 2.2

pdf("Figure 2.3.pdf", width = 8, height = 6, pointsize = 10)
breaks <- seq(50, 400, 10)
hist(Q, main = "", breaks = breaks, xlim = c(0, 400), xaxs = "i", 
     xlab = "Annual mean discharge, in cubic meters per second", 
     ylab = "Frequency", ylim = c(0, 12), yaxs = "i", las = 1, cex.axis = 1.2, 
     cex.lab = 1.4, cex = 1.2, tck = 0.02)
axis(1, at = c(0, 400), labels = NA, tck = 0)
axis(2, at = c(seq(0, 12, 2)), labels = NA, tck = 0.02)
axis(3, at = c(0, 400), labels = NA, tck = 0)
axis(4, at = c(seq(0, 12, 2)), labels = NA, tck = 0.02)
dev.off()

##########################################################################
# Figure 2.4
##########################################################################

# uses the data loaded for Figure 2.2

pdf("Figure 2.4.pdf", width = 8, height = 6, pointsize = 10)
x <- sort(Q)
n <- length(x)
i <- seq(1, n)
# note that we are using the Weibull plotting position
# this is equivalent to type = 6 in the quantile function
p <- i / (n + 1)
plot(x, p, pch = 20, xlim = c(0, 400), xaxs = "i", ylim = c(0, 1), yaxs = "i", 
     xlab = "Annual mean discharge, in cubic meters per second", 
     ylab = "Cumulative frequency", main = "", las = 1, cex = 0.8, 
     cex.main = 1.2, cex.axis = 1.2, cex.lab = 1.2, tck = 0.02)
axis(3, at = c(seq(0, 400, 100)), labels = NA, tck = 0.02)
axis(4, at = c(seq(0, 1, 0.2)), labels = NA, tck = 0.02)
dev.off()

##########################################################################
# Figure 2.5
##########################################################################

# uses the data loaded for Figure 2.2

pdf("Figure 2.5.pdf", width = 8, height = 6, pointsize = 10)
par(tck = 0.02, pty = "s")
boxplot(annualQ$Q, ylim = c(0, 400), yaxs = "i", 
        ylab = "Annual mean discharge, in cubic meters per second\n", 
        las = 1, cex.axis = 1.2, cex.lab = 1.2)
axis(4, at = c(seq(0, 400, 100)), labels = NA, tck = 0.02)
par(tck = NA, pty = "m")
dev.off()

##########################################################################
# Figure 2.6
##########################################################################

pdf("Figure 2.6.pdf", width = 8, height = 6, pointsize = 10)
# enter the data
nf <- c(0.001, 0.003, 0.007, 0.02, 0.03, 0.04, 0.041, 0.077, 0.1, 0.454, 0.49, 1.02)
par(tck = 0.02, pty = "s")
boxplot(nf, ylab = "Unit well yields, in gallons per minute per foot", 
        ylim = c(-0.2, 1.2), yaxs = "i", las = 1, cex.axis = 1.2, cex.lab = 1.2)
axis(4, at = c(seq(-0.2, 1.2, 0.2)), labels = NA, tck = 0.02)
par(tck = NA, pty = "m")
dev.off()

##########################################################################
# Figure 2.7
##########################################################################

pdf("Figure 2.7.pdf", width = 8, height = 6, pointsize = 10)
# transform the data
lnf <- log(nf)
par(mar = c(5, 7, 4, 8) + 0.1, tck = 0.02, pty = "s")
boxplot(lnf, ylab = "ln(Unit well yields, in gallons per minute per foot)", 
        yaxs = "i", ylim = c(-8, 1), las = 1, cex.lab = 1.2)
par(tck = NA, pty = "m")
vals <- c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1, 2)
lvals <- log(vals)
axis(4, at = lvals, labels = vals, las = 1, cex.lab = 1.2, cex.axis = 1.2, 
     tck = 0.02)
mtext("Unit well yields, in gallons per minute per foot", side = 4, line = 4, 
      cex = 1.2)
dev.off()

##########################################################################
# Figure 2.8
##########################################################################

# uses the data loaded for Figure 2.2

pdf("Figure 2.8.pdf", width = 8, height = 6, pointsize = 10)
# note that the code starts with the same instructions used in figure 2.4
x <- sort(Q)
n <- length(x)
i <- seq(1, n)
# note that we are using the Weibull plotting position
# this is equivalent to type = 6 in the quantile function
p <- i / (n + 1)
plot(x, p, pch = 20, xlim = c(0, 400), xaxs = "i", ylim = c(0, 1), yaxs = "i", 
     xlab = "Annual mean discharge, in cubic meters per second", 
     ylab = "Cumulative frequency", main = "", las = 1, cex = 0.7, cex.main = 1.2, 
     cex.axis = 1.2, cex.lab = 1.2, tck = 0.02)
Qbar <- mean(Q)
Qsd <- sd(Q)
# z is the normal quantile given this mean and standard deviation
zvals <- seq(-3, 3)
qvals <- (zvals * Qsd) + Qbar
axis(3, at = qvals, labels = zvals, las = 1, cex.lab = 1.2, cex.axis = 1.2, 
     tck = 0.02)
mtext("Normal quantiles", side = 3, line = 2.5, cex = 1.2)
qvals <- seq(0, 400)
pvals <- pnorm(qvals, mean = Qbar, sd = Qsd)
par(new = TRUE)
plot(qvals, pvals, type = "l", lwd = 2, xlim = c(0, 400), xaxs = "i", ylim = c(0, 1), 
     yaxs = "i", xlab = "", ylab = "", main = "", axes = FALSE)
axis(4, at = c(seq(0, 1, 0.2)), labels = NA, tck = 0.02)
dev.off()

##########################################################################
# Figure 2.9
##########################################################################

# uses the data loaded for Figure 2.2

pdf("Figure 2.9.pdf", width = 6, height = 6, pointsize = 10)
x <- sort(Q)
n <- length(x)
i <- seq(1, n)
# note that we are using the Weibull plotting position
# this is equivalent to type = 6 in the quantile function
p <- i / (n + 1)
Qbar <- mean(Q)
Qsd <- sd(Q)
zvals <- seq(-2.5, 2.5, 0.02)
qvals <- (zvals * Qsd) + Qbar
plot(zvals, qvals, type = "l", lwd = 2, xlim = c(-2.5, 2.5), xaxs = "i", 
     ylim = c(0, 400), yaxs = "i", xlab = "Normal quantiles", 
     ylab = "Annual mean discharge, in cubic meters per second", las = 1, 
     main = "", cex.axis = 1.2, cex.lab = 1.2, tck = 0.02)
z <- qnorm(p)
par(new = TRUE)
plot(z, x, pch = 20, xlim = c(-2.5, 2.5), xaxs = "i", ylim = c(0, 400), 
     yaxs = "i", axes = FALSE, ylab = "", xlab = "", main = "", cex = 0.8)
axis(3, at = c(seq(-2, 2, 1)), labels = NA, tck = 0.02)
axis(4, at = c(seq(0, 400, 100)), labels = NA, tck = 0.02)
dev.off()

##########################################################################
# Figure 2.10
##########################################################################

# uses the data loaded for Figure 2.2

pdf("Figure 2.10.pdf", width = 6, height = 6, pointsize = 10)
x <- sort(Q)
n <- length(x)
i <- seq(1, n)
# note that we are using the Weibull plotting position
# this is equivalent to type = 6 in the quantile function
p <- i / (n + 1)
Qbar <- mean(Q)
Qsd <- sd(Q)
zvals <- seq(-3.5, 3.5, 0.02)
qvals <- (zvals * Qsd) + Qbar
plot(zvals, qvals, type = "l", lwd = 2, xlim = c(-3.5, 3.5), xaxs = "i", 
     ylim = c(0, 400), yaxs = "i", xlab = "Normal quantiles", 
     ylab = "Annual mean discharge, in cubic meters per second", las = 1, 
     main = "", cex.axis = 1.2, cex.lab = 1.2, tck = 0.02)
z <- qnorm(p)
par(new = TRUE)
plot(z, x, pch = 20, xlim = c(-3.5, 3.5), xaxs = "i", ylim = c(0, 400), yaxs = "i", 
     axes = FALSE, ylab = "", xlab = "", main = "", cex = 0.8)
pvals <- c(0.999, 0.99, 0.90, 0.50, 0.10, 0.01, 0.001)
zvals <- qnorm((1 - pvals))
pLabs <- c("99.9", "99", "90", "50", "10", "1", "0.1")
axis(3, at = zvals, labels = pLabs, las = 1, cex.axis = 1.3, tck = 0.02)
mtext("Annual probability of exceedance, in percent", side = 3, line = 3, cex = 1.2)
abline(v = zvals, lty = "dashed", lwd = 0.5)
axis(4, at = c(seq(0, 400, 100)), labels = NA, tck = 0.02)
dev.off()


##########################################################################
# Figure 2.11
##########################################################################
load("PotomacPeaks.RData")

pdf("Figure 2.11.pdf", width = 6, height = 6, pointsize = 10)
Q <- peakData$Q
#  This data set is Potomac River at Point of Rocks, Maryland
#  Annual peak streamflow in cubic meters per second
#  USGS streamgage 01638500, Water Years 1895 - 2016
x <- sort(Q)
n <- length(x)
i <- seq(1, n)
# note that we are using the Weibull plotting position
# this is equivalent to type = 6 in the quantile function
p <- i / (n + 1)
Qbar <- mean(Q)
Qsd <- sd(Q)
zvals <- seq(-3, 3, 0.5)
qvals <- (zvals * Qsd) + Qbar
xticks <- seq(-3, 3, 1)
yticks <- seq(-5000,15000,5000)
plot(zvals, qvals, type = "l", lwd = 2, xlim = c(-3, 3), xaxs = "i", 
     ylim = c(-5000, 15000), yaxs = "i", xlab = "Normal quantiles", 
     ylab = "Annual peak discharge, in cubic meters per second",  main = "", 
     cex.axis = 0.8, cex.lab = 1.2, axes = FALSE)
axis(1, at = xticks, tck = 0.02)
axis(2, at = yticks, labels = formatC(yticks, big.mark = ",", format = "d"), las = 1, tck = 0.02, hadj = 0.64)
z <- qnorm(p)
par(new = TRUE)
plot(z, x, pch = 20, xlim = c(-3, 3), xaxs = "i", ylim = c(-5000, 15000), yaxs = "i",
     axes = FALSE, ylab = "", xlab = "", main = "", cex = 0.8)
abline(h = 0)
axis(3, at = c(seq(-3, 3, 1)), labels = NA, tck = 0.02)
axis(4, at = c(seq(-5000,15000,5000)), labels = NA, tck = 0.02)
dev.off()

##########################################################################
# Figure 2.12
##########################################################################

# uses the data loaded for Figure 2.11

pdf("Figure 2.12.pdf", width = 8, height = 6, pointsize = 10)
logQ <- log(Q)
#  This data set is Potomac River at Point of Rocks, Maryland
#  Annual peak streamflow in cubic meters per second
#  USGS streamgage 01638500, Water Years 1895 - 2016
x <- sort(logQ)
n <- length(x)
i <- seq(1, n)
# note that we are using the Weibull plotting position
# this is equivalent to type = 6 in the quantile function
p <- i / (n + 1)
Qbar <- mean(logQ)
Qsd <- sd(logQ)
zvals <- seq(-3, 3, 0.5)
qvals <- (zvals * Qsd) + Qbar
par(mar = c(5, 7, 4, 8) + 0.1)
plot(zvals, qvals, type = "l", lwd = 2, xlim = c(-3, 3), xaxs = "i", 
     ylim = c(6, 10), yaxs = "i", xlab = "Normal quantiles", 
     ylab = "ln(Discharge, in cubic meters per second)", las = 1, 
     main = "", cex.axis = 1.2, cex.lab = 1.2, tck = 0.02, axes = FALSE)
z <- qnorm(p)
par(new = TRUE)
plot(z, x, pch = 20, xlim = c(-3, 3), xaxs = "i", ylim = c(6, 10), yaxs = "i", 
     axes = FALSE, ylab = "", xlab = "", main = "", cex = 0.8)
box()
ticks <- c(500, 1000, 2000, 5000, 10000, 20000)
atY <- log(ticks)
axis(4, at = atY, labels = formatC(ticks, big.mark = ",", format = "d"),
     las = 1, cex.axis = 0.8, cex.lab = 1.2, tck = 0.02, hadj = 0.1)
mtext("Discharge, in cubic meters per second", side = 4, line = 3, cex = 1.2)
axis(3, at = c(seq(-3, 3, 1)), labels = NA, tck = 0.02)
axis(1, at = c(seq(-3, 3, 1)), labels = TRUE, tck = 0.02)
axis(2, at = c(seq(6, 10)), labels = TRUE, tck = 0.02, las = 1)
dev.off()
#############

##########################################################################
# Figure 2.13
##########################################################################

# uses the data loaded for Figure 2.11

pdf("Figure 2.13.pdf", width = 8, height = 6, pointsize = 10)
logQ <- log(Q)
#  This data set is Potomac River at Point of Rocks, Maryland
#  Annual peak streamflow in cubic meters per second
#  USGS streamgage 01638500, Water Years 1895 - 2016
par(mar = c(5, 7, 4, 8) + 0.1, pty = "s", tck = 0.02)
boxplot(logQ, ylim = c(6,10), yaxs = "i", xlab = "", 
        ylab = "ln(Discharge, in cubic meters per second)", las = 1, 
        main = "", cex.axis = 1.2, cex.lab = 1.2, axes = FALSE)
box()
par(tck = NA, pty = "m")
ticks <- c(500, 1000, 2000, 5000, 10000, 20000)
atY <- log(ticks)
axis(2, at = seq(6, 10), labels = TRUE, tck = 0.02, las = 1)
axis(4, at = atY, labels = formatC(ticks, big.mark = ",", format = "d"),
     las = 1, cex.axis = 0.8, tck = 0.02, hadj = 0.1)
mtext("Discharge, in cubic meters per second", side = 4, line = 3, cex = 1.2)
dev.off()

##########################################################################
# Figure 2.14
##########################################################################

pdf("Figure 2.14.pdf", width = 6, height = 6, pointsize = 10)
# enter the data
nf <- c(0.001, 0.003, 0.007, 0.02, 0.03, 0.04, 0.041, 0.077, 0.1, 0.454, 0.49, 
        1.02)
f <- c(0.02, 0.031, 0.086, 0.130, 0.160, 0.160, 0.180, 0.30, 0.40, 0.44, 0.51, 
       0.72, 0.95)
par(mfrow = c(2, 1))
breaks <- seq(0, 1.2, 0.2)
hist(f, breaks = breaks, right = TRUE, freq = TRUE, density = 5, 
     xlab = "Unit well yields, in gallons per minute per foot", las = 1, 
     cex = 1.2, cex.axis = 1.2, cex.lab = 1.2, yaxs = "i", tck = 0.02, 
     ylim = c(0, 10), axes = FALSE, main = "")
myText <- expression(paste(bolditalic("A."), bold(" Valleys with fractures")))
mtext(text = myText, line = 0.7, adj = 0, cex = 1.2, 
      font = 2)
axis(1, at = c(seq(-0.2, 1.4, 0.2)), labels = c(seq(-0.2, 1.4, 0.2)), tck = 0)
axis(2, at = c(seq(0, 10, 1)), labels = c(0, NA, 2, NA, 4, NA, 6, NA, 8, NA, 10), 
     tck = 0.02, las = 1)
axis(3, at = c(seq(-1, 2, 1)), labels = NA, tck = 0)
axis(4, at = c(seq(0, 10, 1)), labels = NA, tck = 0.02)
hist(nf, breaks = breaks, right = TRUE, freq = TRUE, density = 5, 
     xlab = "Unit well yields, in gallons per minute per foot", las = 1, 
     cex = 1.2, cex.axis = 1.2, cex.lab = 1.2, yaxs = "i", tck = 0.02, 
     ylim = c(0, 10), axes = FALSE, main = "")
myText <- expression(paste(bolditalic("B."), bold(" Valleys without fractures")))
mtext(text = myText, line = 0.7, adj = 0, cex = 1.2, 
      font = 2)
axis(1, at = c(seq(-0.2, 1.4, 0.2)), labels = c(seq(-0.2, 1.4, 0.2)), tck = 0)
axis(2, at = c(seq(0, 10, 1)), labels = c(0, NA, 2, NA, 4, NA, 6, NA, 8, NA, 10), 
     tck = 0.02, las = 1)
axis(3, at = c(seq(-1, 2, 1)), labels = NA, tck = 0)
axis(4, at = c(seq(0, 10, 1)), labels = NA, tck = 0.02)
par(mfrow = c(1, 1))
dev.off()

##########################################################################
# Figure 2.15
##########################################################################

pdf("Figure 2.15.pdf", width = 6, height = 6, pointsize = 10)
# enter the data
nf <- c(0.001, 0.003, 0.007, 0.02, 0.03, 0.04, 0.041, 0.077, 0.1, 0.454, 0.49, 
        1.02)
f <- c(0.02, 0.031, 0.086, 0.130, 0.160, 0.160, 0.180, 0.30, 0.40, 0.44, 0.51, 
       0.72, 0.95)
plot(c(1, 2), c(mean(f), mean(nf)), xlim = c(0, 3), pch = 20, xaxs = "i", 
     xlab = "", ylim = c(-0.2, 1.2), yaxs = "i",
     ylab = "Unit well yield, in gallons per minute per foot", main = "", 
     cex = 1.5, axes = FALSE)
axis(1, at = c(0, 1, 2, 3), labels = c("", "With \nfractures", "Without \nfractures", ""), 
     cex.axis = 1.1, padj = 0.8, tck = 0)
axis(2, at = seq(-0.2, 1.2, 0.2), labels = TRUE, cex.axis = 1.2, cex.lab = 1.2, 
     las = 1, tck = 0.02)
axis(3, at = c(0, 3), tck = 0, labels = FALSE)
axis(4, at = seq(-0.2, 1.2, 0.2), tck = 0.02, labels = FALSE)
segments(1, mean(f) - sd(f), 1, mean(f) + sd(f), lwd = 2)
segments(2, mean(nf) - sd(nf), 2, mean(nf) + sd(nf), lwd = 2)
abline(h = 0, lwd = 2, lty = "dashed")
dev.off()

##########################################################################
# Figure 2.16
##########################################################################

pdf("Figure 2.16.pdf", width = 6, height = 6, pointsize = 10)
# enter the data
nf <- c(0.001, 0.003, 0.007, 0.02, 0.03, 0.04, 0.041, 0.077, 0.1, 0.454, 0.49, 
        1.02)
f <- c(0.02, 0.031, 0.086, 0.130, 0.160, 0.160, 0.180, 0.30, 0.40, 0.44, 0.51, 
       0.72, 0.95)
numNf <- length(nf)
numF <- length(f)
all <- c(f, nf)
group <- c(rep(1, numF), rep(2, numNf))
par(bty = "o")
boxplot(all ~ group, xlim = c(0, 3), xaxs = "i", ylim = c(0, 1.2), yaxs = "i", 
        ylab = "Unit well yield, in gallons per minute per foot", xlab = "", 
        main = "", cex = 1.5, axes = FALSE)
axis(1, at = c(1, 2), labels = c("With \nfractures", "Without \nfractures"), 
     cex.axis = 1.1, padj = 0.8, tck = 0.02)
axis(2, at = seq(0, 1.2, 0.2), labels = TRUE, cex.axis = 1.2, cex.lab = 1.2, 
     las = 1, tck = 0.02)
axis(4, at = seq(0, 1.2, 0.2), labels = FALSE, tck = 0.02)
box(which = "plot")
dev.off()

##########################################################################
# Figure 2.18
##########################################################################

pdf("Figure 2.18.pdf", width = 6, height = 6, pointsize = 10)
# uses USGS Nitrate plus Nitrite data from
# Illinois River at Valley City, IL
# Water Years 2000 - 2015
# Downloaded from USGS Web services
# on 2015-12-31  12:22 PM EST
# Stored in IllinoisNitrate.RData
# also contains metadata in data frame "INFO"
load("IllinoisNitrate.RData")
ls() # identify what objects are in the file
summary(ILLNO23) # check on the names and values in the data frame
par(bty = "o")
boxplot(ILLNO23$Conc ~ ILLNO23$Month, xlim = c(0, 13), xaxs = "i", ylim = c(0, 8), 
        yaxs = "i", axes = FALSE, xlab = "Month", 
        ylab = "Concentration, in milligrams per liter")
axis(1, at = 1:12, , cex.lab = 1.2, tck = 0.02, 
     labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"), 
     cex.axis = 1.2)
axis(2, at = 0:8, labels = TRUE, las = 1, cex.lab = 1.2, cex.axis = 1.2, tck = 0.02)
axis(3, at = 1:12, labels = FALSE, tick = TRUE, tck = 0.02)
axis(4, at = 0:8, labels = FALSE, tick = TRUE, tck = 0.02)
box(which = "plot")
dev.off()

##########################################################################
# Figure 2.19
##########################################################################

pdf("Figure 2.19.pdf", width = 6, height = 6, pointsize = 10)
nf <- c(0.001, 0.003, 0.007, 0.02, 0.03, 0.04, 0.041, 0.077, 0.1, 0.454, 0.49, 
        1.02)
f <- c(0.02, 0.031, 0.086, 0.130, 0.160, 0.160, 0.180, 0.30, 0.40, 0.44, 0.51, 
       0.72, 0.95)
#  first we will do the non-fractured set, note that it is already sorted
x <- nf
n <- length(x)
i <- seq(1, n)
# note that we are using the Weibull plotting position
# this is equivalent to type = 6 in the quantile function
p <- i / (n + 1)
z <- qnorm(p)
plot(z, x, xlim = c(-2, 2),  ylim = c(-0.01, 1.2),  xlab = "Normal quantiles", 
     ylab = "Well yield, in gallons per minute per foot", las = 1, main = "", 
     cex.axis = 1.2, cex.lab = 1.2, cex = 1.2, tck = 0.02, xaxs = "i", yaxs = "i")
# now the same for the fractured set
x <- f
n <- length(x)
i <- seq(1, n)
p <- i / (n + 1)
z <- qnorm(p)
par(new = TRUE)
plot(z, x, pch = 20, xlim = c(-2, 2),  ylim = c(0, 1.2),  axes = FALSE, 
     ylab = "", xlab = "", main =  "", cex = 1.4)
axis(3, at = -2:2, labels = FALSE, tick = TRUE, tck = 0.02)
axis(4, at = seq(0, 1.2, 0.2), labels = FALSE, tick = TRUE, tck = 0.02)
dev.off()

##########################################################################
# Figure 2.20
##########################################################################

pdf("Figure 2.20.pdf", width = 6, height = 6, pointsize = 10)
nf <- c(0.001, 0.003, 0.007, 0.02, 0.03, 0.04, 0.041, 0.077, 0.1, 0.454, 0.49, 
        1.02)
f <- c(0.02, 0.031, 0.086, 0.130, 0.160, 0.160, 0.180, 0.30, 0.40, 0.44, 0.51, 
       0.72, 0.95)
qqplot(nf, f, xlim = c(-0.01, 1.2), ylim = c(0, 1.2), 
       xlab = "Yields without fractures, in gallons per minute per foot", 
       ylab = "Yields with fractures, in gallons per minute per foot", cex = 1.4, 
       cex.axis = 1.4, cex.lab = 1.4, las = 1, tck = 0.02, xaxs = "i", yaxs = "i")
abline(a = 0, b = 1, lwd = 2)
axis(3, at = seq(0, 1.2, 0.2), labels = NA, tck = 0.02)
axis(4, at = seq(0, 1.2, 0.2), labels = NA, tck = 0.02)
dev.off()

##########################################################################
# Figure 2.21
##########################################################################

load("IowaWapNO23.RData")
# the next two lines just summarizes what the two data frames are
summary(warm)
summary(cold)

pdf("Figure 2.21.pdf", width = 6, height = 6, pointsize = 10)
# for this figure only the data frame warm is used
#  to see the metaData give the command INFO
warm$logConc <- log(warm$Conc)
warm$logQ <- log(warm$Q)
# next command re-orders the data set by Q value rather than by DecYear
warm <- warm[order(warm$Q), ]
par(mar = c(5, 6, 5, 6) + 0.1, pty = "s")
plot(warm$logQ, warm$logConc, xlim = c(3, 9), xaxs = "i", ylim = c(-3, 3), 
     yaxs = "i", xlab = "ln(Discharge, in cubic meters per second)", 
     ylab = "ln(Concentration, in milligrams per liter)", las = 1, pch = 19, 
     cex.lab = 1.2, cex.axis = 1.2, cex = 0.8, tck = 0.02)
xtickLabs <- c(50, 100, 200, 500, 1000, 2000, 5000)
xticks <- log(xtickLabs)
axis(3, at = xticks, labels = prettyNum(xtickLabs, big.mark = ","), tick = TRUE, 
     cex.axis = 0.92, tck = 0.02)
ytickLabs <- c(0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20)
yticks <- log(ytickLabs)
axis(4, at = yticks, labels = ytickLabs, tick = TRUE, las = 1, cex.axis = 1.2, 
     tck = 0.02)
mtext("Concentration, in milligrams per liter", side = 4, line = 3, cex = 1.2)
mtext("Discharge, in cubic meters per second", side = 3, line = 3, cex = 1.2)
dev.off()

##########################################################################
# Figure 2.22
##########################################################################

# uses the data loaded for Figure 2.21

pdf("Figure 2.22.pdf", width = 6, height = 6, pointsize = 10)
# now for figure 2.22, do the loess smooth
mod <- loess(logConc ~ logQ, data = warm)
# repeat the original graph
par(mar = c(5, 6, 5, 6) + 0.1, pty = "s")
plot(warm$logQ, warm$logConc, xlim = c(3, 9), xaxs = "i", ylim = c(-3, 3), 
     yaxs = "i", xlab = "ln(Discharge, in cubic meters per second)", 
     ylab = "ln(Concentration, in milligrams per liter)", las = 1, pch = 19, 
     cex.lab = 1.2, cex.axis = 1.2, cex = 0.8, tck = 0.02)
xtickLabs <- c(50, 100, 200, 500, 1000, 2000, 5000)
xticks <- log(xtickLabs)
axis(3, at = xticks, labels = formatC(xtickLabs, big.mark = ",", format = "d"),
     las = 1, cex.axis = 0.92, tck = 0.02)
# axis(3, at = xticks, labels = xtickLabs, tick = TRUE, cex.axis = 1.2, tck = 0.02)
ytickLabs <- c(0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20)
yticks <- log(ytickLabs)
axis(4, at = yticks, labels = ytickLabs, tick = TRUE, las = 1, cex.axis = 1.2, 
     tck = 0.02)
mtext("Concentration, in milligrams per liter", side = 4, line = 3, cex = 1.2)
mtext("Discharge, in cubic meters per second", side = 3, line = 3, cex = 1.2)
# add the loess smooth
par(new = TRUE)
plot(warm$logQ, mod$fit, type = "l", lwd = 3, xlim = c(3, 9), xaxs = "i", 
     ylim = c(-3, 3), yaxs = "i", xlab = "", ylab = "", axes = FALSE)
dev.off()

##########################################################################
# Figure 2.26
##########################################################################

# uses the data loaded for Figure 2.21

pdf("Figure 2.26.pdf", width = 6, height = 6, pointsize = 10)
# now for figure 2.26 we add it the cold season data
cold$logConc <- log(cold$Conc)
cold$logQ <- log(cold$Q)
cold <- cold[order(cold$Q), ]
# repeat the original graph
par(mar = c(5, 6, 5, 6) + 0.1, pty = "s")
plot(warm$logQ, warm$logConc, xlim = c(3, 9), xaxs = "i", ylim = c(-3, 3), 
     yaxs = "i", xlab = "ln(Discharge, in cubic meters per second)", 
     ylab = "ln(Concentration, in milligrams per liter)", las = 1, pch = 19, 
     cex.lab = 1.2, cex.axis = 1.2, cex = 0.8, tck = 0.02)
xtickLabs <- c(50, 100, 200, 500, 1000, 2000, 5000)
xticks <- log(xtickLabs)
axis(3, at = xticks, labels = formatC(xtickLabs, big.mark = ",", format = "d"),
     las = 1, cex.axis = 0.92, tck = 0.02)
ytickLabs <- c(0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20)
yticks <- log(ytickLabs)
axis(4, at = yticks, labels = ytickLabs, tick = TRUE, las = 1, cex.axis = 1.2, 
     tck = 0.02)
mtext("Concentration, in milligrams per liter", side = 4, line = 3, cex = 1.2)
mtext("Discharge, in cubic meters per second", side = 3, line = 3, cex = 1.2)
# add the loess smooth
par(new = TRUE)
plot(warm$logQ, mod$fit, type = "l", lwd = 3, xlim = c(3, 9), xaxs = "i", 
     ylim = c(-3, 3), yaxs = "i", xlab = "", ylab = "", axes = FALSE)
# now add the cold season, first the data
par(new = TRUE)
plot(cold$logQ, cold$logConc, xlim = c(3, 9), xaxs = "i", ylim = c(-3, 3), 
     yaxs = "i", xlab = "", ylab = "", las = 1, pch = 1, cex = 0.8, axes = FALSE)
modCold <- loess(logConc ~ logQ, data = cold)
par(new = TRUE)
plot(cold$logQ, modCold$fit, type = "l", lty = "dashed", lwd = 3, xlim = c(3, 9), 
     xaxs = "i", ylim = c(-3, 3), yaxs = "i", xlab = "", ylab = "", axes = FALSE)
dev.off()

##########################################################################
# Figure 2.27
##########################################################################

# uses the data loaded for Figure 2.21

pdf("Figure 2.27.pdf", width = 6, height = 6, pointsize = 10)
# now the absolute residuals for figure 2.27
warm$absResid <- abs(mod$residuals)
modAbsResid <- loess(absResid ~ logQ, data = warm)
par(mar = c(5, 6, 5, 6) + 0.1, pty = "s")
plot(warm$logQ, warm$absResid, xlim = c(3, 9), xaxs = "i", ylim = c(0, 1.2), 
     yaxs = "i", xlab = "ln(Discharge, in cubic meters per second)", 
     ylab = "Absolute residual", las = 1, pch = 19, cex.lab = 1.2, 
     cex.axis = 1.2, cex = 0.8, tck = 0.02)
xtickLabs <- c(50, 100, 200, 500, 1000, 2000, 5000)
xticks <- log(xtickLabs)
axis(3, at = xticks, labels = formatC(xtickLabs, big.mark = ",", format = "d"),
     las = 1, cex.axis = 0.92, tck = 0.02)
axis(4, at = seq(0, 1.2, 0.2), labels = NA, tck = 0.02)
mtext("Discharge, in cubic meters per second", side = 3, line = 3, cex = 1.2)
par(new = TRUE)
plot(warm$logQ, modAbsResid$fit, xlim = c(3, 9), xaxs = "i", ylim = c(0, 1.2), 
     yaxs = "i", xlab = "", ylab = "", type = "l", lwd = 3, cex.lab = 1.2, 
     cex.axis = 1.2, axes = FALSE)
dev.off()

##########################################################################
# Figure 2.28
##########################################################################

load("Exenv.RData")
attach(Exenv)
head(Exenv)
library("lattice")
library("car")

pdf("Figure 2.28.pdf", width = 8, height = 6, pointsize = 10)
Estd = scale(Exenv)
E <- Estd[5:6, ]
parallelplot(E, lty = c(1:2), col = "black")
dev.off()

##########################################################################
# Figure 2.29
##########################################################################

# uses the Exenv data and Estd computed for Figure 2.28

pdf("Figure 2.29.pdf", width = 8, height = 6, pointsize = 10)
group3 <- c(1, 1, 1, 1, 3, 1, 1, 1, 1, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2)
parallelplot(Estd, groups = group3, lty = c(3, 2, 1), col = 1)
dev.off()

##########################################################################
# Figure 2.31
##########################################################################

# uses the Exenv data loaded for Figure 2.28

pdf("Figure 2.31.pdf", width = 8, height = 6, pointsize = 10)
stars(Exenv, key.loc = c(12, 1.7), nrow = 4, ncol = 6, lwd = 2, main = "")
dev.off()

##########################################################################
# Figure 2.32
##########################################################################

pdf("Figure 2.32.pdf", width = 8, height = 6, pointsize = 10)
load("GAMA.RData")
library(vcd)
ternaryplot(GAMASierraNev[, 18:20], grid = TRUE, pch = GAMASierraNev$sym, 
            cex = 0.5, col = 1, main = "")
dev.off()

##########################################################################
# Figure 2.33
##########################################################################

pdf("Figure 2.33.pdf", width = 6, height = 6, pointsize = 10, colormodel = "grey")
load("G2.RData")
library(hydrogeo)
G2p <- piper(G2)
plot(G2p)
dev.off()

##########################################################################
# Figure 2.34
##########################################################################

# uses the Exenv data loaded for Figure 2.28

pdf("Figure 2.34.pdf", width = 6, height = 6, pointsize = 10)
pairs(Exenv, tck = 0.02, las = 1)
dev.off()

##########################################################################
# Figure 2.35
##########################################################################

# uses the Exenv data loaded for Figure 2.28

pdf("Figure 2.35.pdf", width = 6, height = 6, pointsize = 10, colormodel = "gray")
e1 <- princomp(x = Exenv, cor = TRUE)
biplot(e1, xlim = c(-0.6, 0.6), ylim = c(-0.6, 0.6), xlab = "1st component", 
       ylab = "2nd component", tck = 0.02, las = 1, xaxs = "i", yaxs = "i")
dev.off()

##########################################################################
# Figure 2.35_alt1
##########################################################################

# uses the Exenv data loaded for Figure 2.28

pdf("Figure 2.35_alt1.pdf", width = 9, height = 6, pointsize = 10, colormodel = "gray")
e1 <- princomp(x = Exenv, cor = TRUE)
biplot(e1, xlim = c(-0.6, 0.6), ylim = c(-0.6, 0.6), xlab = "First component", 
        ylab = "Second component", tck = 0.02, las = 1, xaxs = "i", yaxs = "i")
dev.off()

##########################################################################
# Figure 2.35_alt2
##########################################################################
# note this figure requires the vegan package
# uses the Exenv data loaded for Figure 2.28

library("vegan")
pdf("Figure 2.35_alt2.pdf", width = 9, height = 6, pointsize = 10, colormodel = "gray")
e1 <- princomp(x = Exenv, cor = TRUE)
o1 <- ordiplot(e1, choices = c(1,2), type = "none", ylab= "Second component", 
         xlab = "First component", xlim = c(-4, 4), ylim = c(-3, 2), xaxs = "i", 
         yaxs = "i", tck = 0.02, las = 1)
text(o1, "sites", cex = 0.7)
text(o1, "species", arrows = TRUE, cex = 0.7, length = 0.05)
dev.off()

##########################################################################
# Figure 2.36
##########################################################################
# note this figure requires the vegan package
# uses the Exenv data loaded for Figure 2.28

library("vegan")
xtick <- c(-0.8, 0.8, 8)
xlimit <- c(-0.8,0.8)
pdf("Figure 2.36.pdf", width = 8.5, height = 6, pointsize = 10, colormodel = "gray")
e1.mds <- metaMDS(Exenv)
e1.p <- ordiplot(e1.mds, type = "t", xlim = xlimit, tck = 0.02, xaxp = xtick, las = 1)
axis(3, at = seq(-0.8, 0.8, 0.2), labels = NA, tck = 0.02)
axis(4, at = seq(-0.6, 0.6, 0.2), labels = NA, tck = 0.02)
dev.off()

##########################################################################
# Figure 2.37
##########################################################################
# note this code draws a 3-D graph in an interactive window,
# it does not produce a static plot.  Figure 2.37 shows
# two views into the 3-D plot, which can be rotated by hand.
# the rgl and car packages must be loaded.
# uses the Exenv data loaded for Figure 2.28.
# this 3D figure requires XQuartz installation on a macintosh. 
# Not needed under Windows OS.

library(rgl)
library(car)  # No need to reload if loaded above.
scatter3d(X.Organics ~ Interstit.Salinity + Shore.height, data = Exenv, 
          fit = "linear", residuals = TRUE, bg = "white", axis.scales = TRUE, 
          grid = TRUE, ellipsoid = FALSE)

##########################################################################
# Figure 2.38
##########################################################################
# uses the GAMA data loaded for Figure 2.32
percentmeqs <- GAMASierraNev[, 18:28]
percentmeqs <- percentmeqs[, c(-5,-6,-8,-9)]
meanpct <- aggregate(percentmeqs[, -4], by = list(percentmeqs$Unit), 
                     FUN = mean, na.rm = TRUE)
meanpct2 <- as.data.frame(meanpct[, -1])
row.names(meanpct2) <- meanpct$Group.1
meanpct3 <- t(meanpct2)
row.names(meanpct3) <- c("Percent Ca", "Percent Mg", "Percent K+Na", "Percent HCO3+CO3",
                 "Percent Cl", "Percent SO4")
pdf("Figure 2.38.pdf", width = 8, height = 6, pointsize = 10)
barplot(as.matrix(meanpct3), yaxs = "i", las = 1, cex = 0.8, cex.axis = 0.8, tck = 0.02,
        density = c(120, 95, 75, 50, 30, 0), xlab = "Lithologic zones", ylab =
        "Percent cations plus percent anions",legend.text = TRUE, args.legend = 
        list(ncol = 2, bty = "n", cex = 1, x = 3, xjust = 0.5, y = 200, yjust = 0, 
        x.intersp = 9, y.intersp = 1.1, adj = c(0.8,0.5)))
dev.off()

##########################################################################
