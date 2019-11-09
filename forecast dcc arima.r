library (forecast)
mat2 <- read.csv(file="I:/New Project/Stocks/dcc_stocks_red_v2.csv",head=TRUE,sep=",")
x = mat2[,5]
fit <- auto.arima(x, d=NA, D=NA, max.p=5, max.q=5,
max.P=2, max.Q=2, max.order=5, start.p=2, start.q=2,
start.P=1, start.Q=1, stationary=FALSE,
ic="bic", stepwise=TRUE, trace=TRUE,
approximation=(length(x)>100 | frequency(x)>12), xreg=NULL,
test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
allowdrift=TRUE, lambda=NULL )
plot(forecast(fit,h=100))
mat[,]