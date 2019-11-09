mat2 <- read.csv(file="I:/New Project/Stocks/dcc_stocks_red_v2.csv",head=TRUE,sep=",")
mat = mat2[,i]
fit <- auto.arima(mat)
plot(forecast(fit,h=20))