# BootMedian(x1, conf = 95, R = 10000, TYPE = "perc")
#  requires the boot package
# Computes bootstrap estimates and 2-sided confidence interval
# on the median.
# default conf = 95 (95% confidence interval)
# default R  = 10000 (number of bootstrap replicates)
# default TYPE = "perc" (that is the percentile method )
#  other possible values for TYPE = "norm", "basic", "stud", and "bca"
#
BootMedian <- function(x1, conf = 95, R = 10000,TYPE = "perc"){
xname = deparse(substitute(x1))
cat('\n')
cat("Bootstrap Confidence Intervals of the Median of", xname, "\n") 
cat("Using boot package in R", "\n") 
cat("\n")
Conf <- conf/100
f <- function(data,i)median(data[i])
B <- boot(x1, f, R = R)
BCIout = boot.ci(B, type = TYPE, conf = Conf )
print(BCIout)
 }
