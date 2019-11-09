library(strucchange)
mat2 <- read.csv(file="I:/New Project/Stocks/akcie_diff.csv",head=TRUE,sep=",")
mat = mat2[,1]

##  Load  dataset  "mat"  with  average  yearly  temperatures  in  New  Haven

##  plot  the  data
plot(mat)
##  test  the  model  null  hypothesis  that  the  average  temperature  remains  constant
##  over  the  years
##  compute  OLS-CUSUM  fluctuation  process
temp.cus  <-  efp(mat  ~  1,  type  =  "OLS-CUSUM")
##  plot  the  process  without  boundaries
plot(temp.cus,  alpha  =  0.1,  boundary  =  FALSE)
##  add  the  boundaries  in  another  colour
bound  <-  boundary(temp.cus,  alpha  =  0.1)
lines(bound,  col=4)
lines(-bound,  col=4)