# Make some data:
x <- c(0.3, 0.9, 0.36, 0.92, 0.5, 1.0, 0.7, 9.7, 0.7, 1.3)

# Compute the mean
xBar <- mean(x)

# Compute the median:
Med <- median(x)

# Compute the trimmed mean (50%, same as median):
trimMean <- mean(x, trim = 0.5)

# Compute the mode:
y <- table(as.vector(x))
modeX <- as.numeric(names(y) [y == max(y)])
