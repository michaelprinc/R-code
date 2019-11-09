mat <- read.csv(file="I:/New Project/Stocks/dcc_stocks_red_v2.csv",head=TRUE,sep=",")

library(tseries)
library(changepoint)

mat2 <- t(mat)
p1 = mat[,1]
p2 = mat[,2]
p3 = mat[,3]
p4 = mat[,4]
p5 = mat[,5]
p6 = mat[,6]
p7 = mat[,7]
p8 = mat[,8]
p9 = mat[,9]
p10 = mat[,10]
p11 = mat[,11]
p12 = mat[,12]
p13 = mat[,13]
p14 = mat[,14]
p15 = mat[,15]
multiple.mean.cusum(mat2,mul.method="SegNeigh",penalty="SIC",value=0,Q=20,class=TRUE,param.estimates=TRUE)
