mat <- read.csv(file="I:/Stocks/v4 change.csv",head=TRUE,sep=",")
options("scipen"=100, "digits"=4)
 # library(tseries)
format(scientific=FALSE)
p1 = mat[,1]
p2 = mat[,2]
p3 = mat[,3]


p = cbind(p1,p2,p3)
y = p
y[,1] = y[,1]-mean(y[,1])
y[,2] = y[,2]-mean(y[,2])
y[,3] = y[,3]-mean(y[,3])



T = length(y[,1])

library(ccgarch)
library(fGarch)

f1 = garchFit(~ garch(1,1), data=y[,1],include.mean=FALSE)
f1 = f1@fit$coef
f2 = garchFit(~ garch(1,1), data=y[,2],include.mean=FALSE)
f2 = f2@fit$coef
f3 = garchFit(~ garch(1,1), data=y[,3],include.mean=FALSE)
f3 = f3@fit$coef
print(f1)
print(f2)
print(f3)
#print(p1)
#print(p2)
#print(p3)



a = c(f1[1],f2[1],f3[1])
A = diag(c(f1[2],f2[2],f3[2]))
B = diag(c(f1[3],f2[3],f3[3]))
dccpara = c(0.1,0.8)
dccresults = dcc.estimation(inia=a, iniA=A, iniB=B, ini.dcc=dccpara,dvar=y,
model="diagonal")

dccresults$out
A=dccresults$DCC
B=dccresults$h
write.table(A,file="I:/Stocks/dcc_v4.csv",sep = ",",col.names = NA,qmethod = "double")
write.table(B,file="I:/Stocks/variance_v4.csv",sep = ",",col.names = NA,qmethod = "double")



