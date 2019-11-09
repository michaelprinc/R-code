library(fGarch)
> ret <- diff(log(x))*100
> fit = garchFit(~arma(1,0,0)+garch(1, 1), data =ret)
> predict(fit, n.ahead = 10)