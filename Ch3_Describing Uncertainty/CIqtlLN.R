CIqtlLN <- function(x, pct,conf, type ="two-sided"){
# This function computes the confidence intervals on a quantile
# assuming log normality of the data
# three options
# default is "two-sided"
# also "lower" one sided Confidence Interval
# or "upper" one sided Confidence Interval
dataname = deparse(substitute(x))
CONF = 100 * conf
PCT = 100 * pct
y  = log(x)
n = length(y)
ybar = mean(y)
s = sd(y)
quantile = exp(ybar + qnorm(pct) * s)
options(warn = -1)
NCP = -sqrt(n) * qnorm(pct)
cat('\n')
cat("Data is ", dataname, '\n', '\n')
cat("Estimated", PCT, "th", "%tile = ", quantile, "assuming lognormality", '\n', '\n')
cat(" Confidence Level = ", CONF, "%", '\n', '\n') 
if(type == "two-sided"){
Kup = qt((1 - conf) / 2, n - 1, ncp = NCP) / sqrt(n)
Klow = qt((1 + conf) /2, n - 1, ncp = NCP) / sqrt(n)
lcl = exp(ybar - Klow * s)
ucl = exp(ybar - Kup * s)
cat("Two-sided Confidence Interval is (",lcl,",",ucl,")", '\n')}
if (type == "upper"){
Kup = qt((1 - conf), n - 1, ncp = NCP) / sqrt(n)
ucl = exp(ybar - Kup * s)
cat("Upper Confidence Interval is (","-inf ,",ucl,")", '\n')}
if (type == "lower"){     
Klow = qt(conf, n - 1, ncp = NCP) / sqrt(n)
lcl = exp(ybar - Klow * s)
cat("Lower Confidence Interval is (",lcl,",inf)", '\n')}
options(warn = 1)}
