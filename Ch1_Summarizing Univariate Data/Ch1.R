# Make some data:
x <- c(2,4,8,9,11,11,12)

# Compute the mean:
xBar <- mean(x)

# Compute the median:
med <- median(x)

# Compute the trimmed mean (50%, same as median):
trimMean <- mean(x, trim = 0.5)

# Compute the mode:
y <- table(as.vector(x))
modeX <- as.numeric(names(y) [y == max(y)])

# Compute the standard deviation:
stdDev <- sd(x)

# Compute the variance:
varX <- var(x)

# Calculate the interquartile range:
quant <- as.numeric(quantile(x, type = 6))
IQR <- quant[4] - quant[2]
IQR <- quantile(x, probs = 0.75, type = 6) - quantile(x, probs = 0.25, type = 6)
IQR <- IQR(x, type = 6)


# Calculate the median absolute deviation:
MAD <- median(abs(x-med))

cat("IQR = ",IQR, ", MAD = ", MAD, "Standard Deviation = ", stdDev)

# IQR =  7 , MAD =  2 Standard Deviation =  3.804759

#------------------------------------------------------------------------------

# Repeat steps with different dataset:
x <- c(2,4,8,9,11,11,120)

# Calculate the quantiles
quant <- as.numeric(quantile(x, type = 6))

# Calculate the interquartile range
IQR <- quant[4] - quant[2]

# Calculate the mean
xBar <- mean(x)

# Calculate the standard deviation
stdDev <- sd(x)

# Calculate the median
med <- median(x)

# calculate the median absolute deviation
MAD <- median(abs(x-med))

cat("IQR = ",IQR, ", MAD = ", MAD, "Standard Deviation = ", stdDev)

# IQR =  7 , MAD =  2 Standard Deviation =  42.65699



