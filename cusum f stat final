library(strucchange)
# mat2 <- read.csv(file="I:/New Project/Stocks/dcc_stocks_red_v2.csv",head=TRUE,sep=",")
for (i in 1:15)
 {
mat2 <- read.csv(file="I:/New Project/Stocks/dcc_stocks_red_v2.csv",head=TRUE,sep=",")
mat = mat2[,i]

##  Load  dataset  "mat"  with  average  yearly  temperatures  in  New  Haven

##  plot  the  data
s1 <- "I:/dcc"
s2 <- as.character(i)
# s3 <- ".pdf"
s3 <- ".jpeg"
name <- paste(s1, s2, s3, sep = "")

# pdf(name, bg = "white")
jpeg(name, quality = 100, bg = "white",res = 200, width = 7, height = 7, units = "in")
plot(mat)
dev.off()

##  test  the  model  null  hypothesis  that  the  average  temperature  remains  constant
##  over  the  years
##  compute  OLS-CUSUM  fluctuation  process
temp.cus  <-  efp(mat  ~  1,  type  =  "OLS-MOSUM")
##  plot  the  process  without  boundaries
s1 <- "I:/ols_mosum"
s2 <- as.character(i)
# s3 <- ".pdf"
s3 <- ".jpeg"
name <- paste(s1, s2, s3, sep = "")
jpeg(name, quality = 100, bg = "white",res = 200, width = 7, height = 7, units = "in")
plot(temp.cus,  alpha  =  0.01,  boundary  =  FALSE)
dev.off()
##  add  the  boundaries  in  another  colour
bound  <-  boundary(temp.cus,  alpha  =  0.01)
lines(bound,  col=4)
lines(-bound,  col=4)
fs <- Fstats(mat ~ 1, from = 4, to = length(mat)-4)

k <- fs$nreg
  n <- fs$nobs
  x <- fs$Fstats
  pvals <- 1 - pf(x, k, (n - 2 * k))

s1 <- "I:/ols_mosum_pval"
s2 <- as.character(i)
# s3 <- ".pdf"
s3 <- ".jpeg"
name <- paste(s1, s2, s3, sep = "")

# pdf(name, bg = "white")
jpeg(name, quality = 100, bg = "white",res = 200, width = 7, height = 7, units = "in")
plot(fs,pval=TRUE)
dev.off()

        
       } 