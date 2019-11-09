mat <- read.csv(file="I:/New Project/Stocks/akcie_diff_red.csv",head=TRUE,sep=",")

 # library(tseries)

p1 = mat[,1]
p2 = mat[,2]
p3 = mat[,3]
p4 = mat[,4]
p5 = mat[,5]
p = cbind(p1,p2,p3,p4,p5)
y = p
y[,1] = y[,1]-mean(y[,1])
y[,2] = y[,2]-mean(y[,2])
y[,3] = y[,3]-mean(y[,3])
y[,4] = y[,4]-mean(y[,4])
y[,5] = y[,5]-mean(y[,5])


T = length(y[,1])

library(ccgarch)
library(fGarch)

f1 = garchFit(~ garch(1,1), data=y[,1],include.mean=FALSE)
f1 = f1@fit$coef
f2 = garchFit(~ garch(1,1), data=y[,2],include.mean=FALSE)
f2 = f2@fit$coef
f3 = garchFit(~ garch(1,1), data=y[,3],include.mean=FALSE)
f3 = f3@fit$coef
f4 = garchFit(~ garch(1,1), data=y[,4],include.mean=FALSE)
f4 = f4@fit$coef
f5 = garchFit(~ garch(1,1), data=y[,5],include.mean=FALSE)
f5 = f5@fit$coef

a = c(f1[1],f2[1],f3[1],f4[1],f5[1])
A = diag(c(f1[2],f2[2],f3[2],f4[2],f5[2])
B = diag(c(f1[3],f2[3],f3[3],f4[3],f5[3])
dccpara = c(0.1,0.8)
dccresults = dcc.estimation(inia=a, iniA=A, iniB=B, ini.dcc=dccpara,dvar=y,
model="extended")

dccresults$out
A=dccresults$DCC
write.table(A,file="I:/New Project/Filtered/dcc_stocks_red_ext.csv",sep = ",",col.names = NA,qmethod = "double")


