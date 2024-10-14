# BootTOL(x, p = coverage, conf = 95, R = 10000, TYPE = "perc")
# Computes bootstrap estimates and 2-sided confidence interval
# on the pth percentile
# default conf = 95
#default R  = 10000
#
 BootTOL<- function(x1, p = coverage, conf = 95, R = 10000, TYPE = "perc"){
xname = deparse(substitute(x1))
pname = deparse(substitute(p))
cat('\n')
cat("Bootstrap Confidence Intervals of the ",pname,"-th percentile of ",xname,'\n') 
cat("Using boot in R",'\n') 
cat('\n')
Conf= conf / 100
f= function(data, i){quantile(data[i], p / 100, type = 6)}
B=boot(x1, f, R= R)
BCIout = boot.ci(B,type = TYPE,conf = Conf )
print(BCIout)
 }
