# R code for Chapter 1, Summarizing Data

##############################################################################
# Figure 1.1
##############################################################################

# this code produces a probability density function (in the upper panel) for a 
# lognormal distribution
# the lower panel is the cumulative distribution function
#
# the mean of the logs (natural logs) is 0
# and the standard deviation of the logs is 1
# it shows the mean of the distribution which is 1.648721
# (That is computed as exp(V/2) where V is the variance of the logs
# so we have mean = exp(0.5))
# it also shows the median of the distribution which is 1.0
# (That is computed as exp(m) where m is the mean of the logs
# so we have median = exp(0))

pdf("Figure 1.1.pdf", width = 8, height = 10, pointsize = 10)
par(mfrow = c(2, 1))
x <- seq(0.01, 8, 0.01)
y <- dlnorm(x) # dlnorm is the density function for a lognormal distribution
plot(x, y, type = "l", lwd = 2, xlim = c(0, 8),xaxs = "i",ylim = c(0, 0.7), 
     yaxs = "i", las = 1, xlab = "Value", ylab=" Probability density", 
     cex.axis = 1.4, cex.lab = 1.2, tck = 0.02)
abline(v = 1.648721, lty = "dashed", lwd = 2)
abline(v = 1.0, lty = "dotted", lwd = 2)
text(x = 2, y = 0.45,labels = "Mean",cex = 1.5)
text(x = 0.6, y = 0.15, labels = "Median", cex = 1.5)
axis(side = 3, at = seq(0, 8, 2), labels = FALSE, tck = 0.02)
axis(side = 4, at = seq(0.0, 0.7, 0.1), labels = FALSE, tck = 0.02)
myText <- expression(paste(bolditalic("A."), bold(" Probability density function")))
mtext(text = myText, line = 0.7, adj = 0, cex = 1.2, 
      font = 2)
y <- plnorm(x)
plot(x, y, type = "l", lwd = 2, xlim = c(0, 8),xaxs = "i",ylim = c(0, 1), 
     yaxs = "i", las = 1, xlab = "Value", ylab="Cumulative probability", 
     cex.axis = 1.4, cex.lab = 1.2, tck = 0.02)
abline(v = 1.648721, lty = "dashed", lwd = 2)
abline(v = 1.0, lty = "dotted", lwd = 2)
text(x = 2, y = 0.45,labels = "Mean",cex = 1.5)
text(x = 0.6, y = 0.8, labels = "Median", cex = 1.5)
axis(side = 3, at = seq(0, 8, 2), labels = FALSE, tck = 0.02)
axis(side = 4, at = seq(0.0, 1.0, 0.2), labels = FALSE, tck = 0.02)
myText <- expression(paste(bolditalic("B."), bold(" Cumulative distribution function")))
mtext(text = myText, line = 0.7, adj = 0, 
      cex = 1.2, font = 2)
dev.off()

#############################################################################
# Figure 1.2
#############################################################################

# this code produces a probability density function for a normal distribution
# the mean is 1 and the standard deviation is 0.25
# because it is symmetric the mean and median are identical

pdf("Figure 1.2.pdf", width = 8, height = 10, pointsize = 10)
par(mfrow = c(2, 1))
x <- seq(0, 2, 0.01)
y <- dnorm(x, mean = 1, sd = 0.25) # dnorm is the density function for a normal distribution
plot(x, y, type = "l", lwd = 2, xlim = c(0, 2), xaxs = "i", ylim = c(0, 2), 
     yaxs = "i", las = 1, xlab = "Value", ylab = "Probability density", 
     cex.axis = 1.4, cex.lab = 1.2, tck = 0.02)
abline(v = 1, lty = "dotdash", lwd = 2)
text(x = 0.965, y = 1.8, labels = "Mean and median", cex = 1.5)
axis(side = 3, at = seq(0, 2, 0.5), labels = c(rep(NA ,5)), tck = 0.02)
axis(side = 4, at = seq(0, 2, 0.5), labels = c(rep(NA, 5)), tck = 0.02)
myText <- expression(paste(bolditalic("A."), bold(" Probability density function")))
mtext(text = myText, line = 0.7, adj = 0, cex = 1.2, 
      font = 2)
y <- pnorm(x, mean = 1, sd = 0.25)
plot(x, y, type = "l", lwd = 2, xlim = c(0, 2), xaxs = "i", ylim = c(0, 1), 
     yaxs = "i", las = 1, xlab = "Value", ylab = "Cumulative probability", 
     cex.axis = 1.4, cex.lab = 1.2, tck = 0.02)
abline(v = 1, lty = "dotdash", lwd = 2)
text(x = 0.965, y = 0.9, labels = "Mean and median", cex = 1.5)
axis(side = 3, at = seq(0, 2, 0.5), labels = FALSE, tck = 0.02)
axis(side = 4, at = seq(0, 1, 0.2), labels = FALSE, tck = 0.02)
myText <- expression(paste(bolditalic("B."), bold(" Cumulative distribution function")))
mtext(text = myText, line = 0.7, adj = 0, 
      cex = 1.2, font = 2)
dev.off()

#############################################################################
# Figure 1.3
#############################################################################

# this code produces a set of points on a line
# balanced on a triangle at the mean value 

pdf("Figure 1.3.pdf", width = 8, height = 3, pointsize = 10)
x <- c(2, 4, 8, 9, 10, 13, 38)
y <- rep(0.1, 7)
yminus <- y - 0.0002
plot(x, y ,pch = 15, type = "p", axes = FALSE, bty = "o", xlab = "", ylab = "", 
     xlim = c(0, 40), xaxs = "i", ylim = c(0.094, 0.106), cex.axis = 1.6)
par(new = TRUE)
plot(x, yminus, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "", 
     xlim = c(0, 40), xaxs = "i", ylim = c(0.094, 0.106))
par(new = TRUE)
plot(mean(x), 0.0985, type = "p", pch = 17 ,axes = FALSE, xlab = "", ylab = "", 
     xlim = c(0, 40), xaxs = "i", ylim = c(0.094, 0.106), cex = 2)
axis(side = 1, at = seq(0, 40, 5), pos = 0.095, tck = 0.1)
axis(side = 2, at = seq(0.095, 0.105, 0.005), labels = c(rep(NA, 3)), tck = 0.08)
axis(side = 3, at = seq(0, 40, 5), labels = c(rep(NA, 9)), pos = 0.105, tck = 0.1)
axis(side = 4, at = seq(0.095, 0.105, 0.005), labels = c(rep(NA, 3)), tck = 0.08)
dev.off()

#############################################################################
# Figure 1.4
#############################################################################

# this code produces a figure identical to Figure 1.3 except that 
# the high outlier value has been removed

pdf("Figure 1.4.pdf", width = 8, height = 3, pointsize = 10)
x <- c(2, 4, 8, 9, 10, 13)
y <- rep(0.1, 6)
yminus <- y - 0.0002
plot(x, y, pch = 15, type = "p", axes = FALSE, xlab = "", ylab = "", 
     xlim = c(0, 40), xaxs = "i", ylim = c(0.094, 0.106), cex.axis = 1.6)
par(new = TRUE)
plot(x, yminus, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "", 
     xlim = c(0, 40), xaxs = "i", ylim = c(0.094, 0.106))
par(new = TRUE)
plot(mean(x), 0.0985, type = "p", pch = 17, axes = FALSE, xlab = "", ylab = "", 
     xlim = c(0, 40), xaxs = "i", ylim = c(0.094, 0.106), cex = 2)
axis(side = 1, at = seq(0, 40, 5), pos = 0.095, tck = 0.1)
axis(side = 2, at = seq(0.095, 0.105, 0.005), labels = c(rep(NA, 3)), tck = 0.1)
axis(side = 3, at = seq(0, 40, 5), labels = c(rep(NA, 9)), pos = 0.105, tck = 0.1)
axis(side = 4, at = seq(0.095, 0.105, 0.005), labels = c(rep(NA, 3)), tck = 0.1)
dev.off()
