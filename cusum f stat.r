library(strucchange)
for (i in 1:20)
 {
mat2 <- read.csv(file="I:/New Project/Stocks/dcc_stocks.csv",head=TRUE,sep=",")
mat = mat2[,i]

##  Load  dataset  "mat"  with  average  yearly  temperatures  in  New  Haven

##  plot  the  data
plot(mat)
##  test  the  model  null  hypothesis  that  the  average  temperature  remains  constant
##  over  the  years
##  compute  OLS-CUSUM  fluctuation  process
temp.cus  <-  efp(mat  ~  1,  type  =  "OLS-CUSUM")
##  plot  the  process  without  boundaries
plot(temp.cus,  alpha  =  0.01,  boundary  =  FALSE)
##  add  the  boundaries  in  another  colour
bound  <-  boundary(temp.cus,  alpha  =  0.01)
lines(bound,  col=4)
lines(-bound,  col=4)
fs <- Fstats(mat ~ 1, from = 4, to = length(mat)-4)

k <- fs$nreg
  n <- fs$nobs
  x <- fs$Fstats
  pvals <- 1 - pf(x, k, (n - 2 * k))

s1 <- "I:/test"
s2 <- as.character(i)
s3 <- ".pdf"
name <- paste(s1, s2, s3, sep = "")

pdf(name, bg = "white")
plot(fs,pval=TRUE)
dev.off()

        
       } 