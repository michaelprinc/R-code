mat <- read.csv(file="I:/New Project/Stocks/short file 15 stocks.csv",head=TRUE,sep=",")

library(tseries)

p1 = mat[,1]
p2 = mat[,2]
p3 = mat[,3]
p4 = mat[,4]
p5 = mat[,5]
p6 = mat[,6]
p7 = mat[,7]
p8 = mat[,8]
p9 = mat[,9]



print("1")
p = cbind(p1,p2,p3,p4,p5,p6,p7,p8,p9)
print("2")
y = p
y[,1] = y[,1]-mean(y[,1])
y[,2] = y[,2]-mean(y[,2])
y[,3] = y[,3]-mean(y[,3])
y[,4] = y[,4]-mean(y[,4])
y[,5] = y[,5]-mean(y[,5])
y[,6] = y[,6]-mean(y[,6])
y[,7] = y[,7]-mean(y[,7])
y[,8] = y[,8]-mean(y[,8])
y[,9] = y[,9]-mean(y[,9])





print("3")


T = length(y[,1])
print("4")
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
f6 = garchFit(~ garch(1,1), data=y[,6],include.mean=FALSE)
f6 = f6@fit$coef
f7 = garchFit(~ garch(1,1), data=y[,7],include.mean=FALSE)
f7 = f7@fit$coef
f8 = garchFit(~ garch(1,1), data=y[,8],include.mean=FALSE)
f8 = f8@fit$coef
f9 = garchFit(~ garch(1,1), data=y[,9],include.mean=FALSE)
f9 = f9@fit$coef



print("5")

a = c(f1[1],f2[1],f3[1],f4[1],f5[1],f6[1],
      f7[1],f8[1],f9[1])
A = diag(c(f1[1],f2[1],f3[1],f4[1],f5[1],f6[1],
      f7[1],f8[1],f9[1]))
B = diag(c(f1[1],f2[1],f3[1],f4[1],f5[1],f6[1],
      f7[1],f8[1],f9[1]))
print("6")
dccpara = c(0.1,0.8)
dccresults = dcc.estimation(inia=a, iniA=A, iniB=B, ini.dcc=dccpara,dvar=y,
model="diagonal")

dccresults$out
A=dccresults$DCC
B=dccresults$h
write.table(A,file="I:/15Stocks/dcc_stocks_shortfilefinal_9.csv",sep = ",",col.names = NA,qmethod = "double")
write.table(B,file="I:/15Stocks/varianceshortfilefinal_9.csv",sep = ",",col.names = NA,qmethod = "double")



