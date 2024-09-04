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


# Calculate quartile skew:
pvals <- as.numeric(quantile(x, probs = c(0.25, 0.5, 0.75), type = 6))
qs <- ((pvals[3] - pvals[2]) - (pvals[2] - pvals[1])) / (pvals[3] - pvals[1])




#----------------------------------EXERCISES------------------------------------

#1)

dat <- c(0.001,0.03,0.1,0.003,0.040,0.454,0.007,0.51,0.49,0.02,0.077,1.02)
mean <- mean(dat)
mean10 <- mean(dat, trim = 0.1)
mean20 <- mean(dat, trim = 0.2)
geoMean <- exp(mean(log(dat)))
med <- median(dat)

cat("Mean = ", mean, ", Trimmed Mean (10%) = ", mean10, ", Trimmed Mean (20%) = ", 
    mean20, ", Geometric Mean = ", geoMean, ", Median = ", med)

# Mean =  0.2293333 , Trimmed Mean (10%) =  0.1731 , Trimmed Mean (20%) =  0.15225 , 
# Geometric Mean =  0.05260519 , Median =  0.0585

#2)

stdDev <- sd(dat)
IQR <- IQR(dat)
MAD <- median(abs(dat - med))

# Function to return coefficient of skewness
skew <- function(x){
  n <- length(x)
  y <- sum(((x-mean(x))^3)/(sd(x)^3))
  g <- (n/((n-1)*(n-2)))*y
  return(g)
}

# Calculate the quartile skew:
pvals <- as.numeric(quantile(dat, probs = c(0.25, 0.5, 0.75), type = 6))
qs <- ((pvals[3] - pvals[2]) - (pvals[2] - pvals[1])) / (pvals[3] - pvals[1])

cat("Standard Deviation = ", stdDev, ", IQR = ", IQR, ", MAD = ", MAD,
    ", Coefficient of Skewness = ", skew(dat), ", Quartile Skew = ", qs)

# 3)

dat2 <- c(0.3,0.9,0.36,0.92,0.5,1,0.7,9.7,0.7,1.3)

cat('Mean =', mean(dat2), '\nTrimmed Mean (10%) =', mean(dat2, trim = 0.1), 
    '\nTrimmed Mean (20%) =', mean(dat2, trim = 0.2), '\nGeometric Mean =', 
    exp(mean(log(dat2))), '\nMedian =', median(dat2),
    '\nStandard Deviation =', sd(dat2), '\nIQR =', IQR(dat2),
    '\nMAD =', median(abs(dat2 - median(dat2))), '\nCoefficient of Skewness =', skew(dat2))

# Mean = 1.638 
# Trimmed Mean (10%) = 0.7975 
# Trimmed Mean (20%) = 0.7866667 
# Geometric Mean = 0.8792928 
# Median = 0.8 
# Standard Deviation = 2.849085 
# IQR = 0.43 
# MAD = 0.25 
# Coefficient of Skewness = 3.094764
