mat <- read.csv(file="I:/New Project/Stocks/akcie_diff.csv",head=TRUE,sep=",")

library(tseries)
library(changepoint)

mat2 <- t(mat)

ans=multiple.mean.cusum(mat2,mul.method="BinSeg",penalty="SIC",Q=100)
cpts(ans[[1]])  #  "None", "SIC", "BIC", "AIC", "Hannan-Quinn", "Asymptotic" and "Manual" 
cpts(ans[[2]])  #  same  results  as  for  the  SegNeigh  method.
cpts(ans[[3]])  #  same  results  as  for  the  SegNeigh  method.
cpts(ans[[4]])  #  same  results  as  for  the  SegNeigh  method.
cpts(ans[[5]])  #  same  results  as  for  the  SegNeigh  method.
cpts(ans[[6]])  #  same  results  as  for  the  SegNeigh  method.