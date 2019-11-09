arima.estim <- function(TS) {
best.model <- arima(TS, order = c(1, 0, 0), seasonal =
list(order = c(0, 0, 0), period = frequency(TS)) )
# Start value
# I continue with brute force- p, q, r, s are nested from 0 to 3 and i
and j are nested from 0 to 2. p and q are not both allowed to be 0.
for (p in 0:3){
for( q in 0:3){
if(p==0 && q==0) {}
else {
for(r in 0:3) {
for(s in 0:3) {
for (j in 0:2) {
for(i in 0:2) {
# test, if series works
if(inherits(try(arima(TS, order = c(p, i, q), seasonal = list(order
= c(r, j, s), period = frequency(TS)) ), TRUE), 'try-error')){
 print(c(p,i,q))} #shows, which parameters didn't work -> will be removed by
else {
tmp <- arima(TS, order = c(p, i, q), seasonal =
list(order = c(r, j, s), period = frequency(TS))) # calculate again :(
if(best.model$aic > tmp$aic)
{
best.model <- tmp
}
}
}
}
}
} } } }

 best.model} 