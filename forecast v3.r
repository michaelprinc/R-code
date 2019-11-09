mat <- read.csv(file="I:/New Project/Stocks/akcie_diff_6.csv",head=TRUE,sep=",")
# outputmat=mat[1,]
ret = mat[,1]
library(fGarch)
# ret <- diff(log(x))*100
fit = garchFit(~arma(1,0,0)+garch(1, 1), data =ret)
# predict(fit, n.ahead = 200)
output=predict(fit, n.ahead = 200)
#print(output)
outputmat= cbind(outputmat,output[,3])
outputmat= cbind(outputmat,output[,3])
print(outputmat)

