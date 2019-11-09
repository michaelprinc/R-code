x = arima.sim(list(order=c(1,0,1), ar=.9, ma=-.5), n=100) # simulate some data
(x.fit = arima(x, order = c(1, 0, 1)))   # fit the model and print the results
tsdiag(x.fit, gof.lag=20)
x.fore = predict(x.fit, n.ahead=10)  
# plot the forecasts
U = x.fore$pred + 2*x.fore$se
L = x.fore$pred - 2*x.fore$se
minx=min(x,L)
maxx=max(x,U)
ts.plot(x,x.fore$pred,col=1:2, ylim=c(minx,maxx))
lines(U, col="blue", lty="dashed")
lines(L, col="blue", lty="dashed") 