mat <- read.csv(file="I:/New Project/Stocks/akcie_diff_red_big.csv",head=TRUE,sep=",")

 # library(tseries)

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
p16 = mat[,16]
p17 = mat[,17]
p18 = mat[,18]
p19 = mat[,19]
p20 = mat[,20]
p21 = mat[,21]
p22 = mat[,22]
p23 = mat[,23]
p24 = mat[,24]
p25 = mat[,25]
p26 = mat[,26]
p27 = mat[,27]
p28 = mat[,28]
p29 = mat[,29]
p30 = mat[,30]
p31 = mat[,31]
p32 = mat[,32]
p33 = mat[,33]
p34 = mat[,34]
p35 = mat[,35]
p36 = mat[,36]
p37 = mat[,37]
p38 = mat[,38]

print("1")
p = cbind(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,
p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,
p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,
p31,p32,p33,p34,p35,p36,p37,p38)
print("2")
y = p
y[,1] = y[,1]-mean(y[,1])
y[,2] = y[,2]-mean(y[,2])
y[,3] = y[,3]-mean(y[,3])
y[,4] = y[,4]-mean(y[,4])
y[,5] = y[,5]-mean(y[,5])
y[,6] = y[,6]-mean(y[,6])
y[,7] = y[,7]-mean(y[,7])
y[,8] = y[,8]-mean(y[,8])
y[,9] = y[,9]-mean(y[,9])
y[,10] = y[,10]-mean(y[,10])
y[,11] = y[,11]-mean(y[,11])
y[,12] = y[,12]-mean(y[,12])
y[,13] = y[,13]-mean(y[,13])
y[,14] = y[,14]-mean(y[,14])
y[,15] = y[,15]-mean(y[,15])
y[,16] = y[,16]-mean(y[,16])
y[,17] = y[,17]-mean(y[,17])
y[,18] = y[,18]-mean(y[,18])
y[,19] = y[,19]-mean(y[,19])
y[,20] = y[,20]-mean(y[,20])
y[,21] = y[,21]-mean(y[,21])
y[,22] = y[,22]-mean(y[,22])
y[,23] = y[,23]-mean(y[,23])
y[,24] = y[,24]-mean(y[,24])
y[,25] = y[,25]-mean(y[,25])
y[,26] = y[,26]-mean(y[,26])
y[,27] = y[,27]-mean(y[,27])
y[,28] = y[,28]-mean(y[,28])
y[,29] = y[,29]-mean(y[,29])
y[,30] = y[,30]-mean(y[,30])
y[,31] = y[,31]-mean(y[,31])
y[,32] = y[,32]-mean(y[,32])
y[,33] = y[,33]-mean(y[,33])
y[,34] = y[,34]-mean(y[,34])
y[,35] = y[,35]-mean(y[,35])
y[,36] = y[,36]-mean(y[,36])
y[,37] = y[,37]-mean(y[,37])
y[,38] = y[,38]-mean(y[,38])
print("3")


T = length(y[,1])
print("4")
library(ccgarch)
library(fGarch)

f1 = garchFit(~ garch(1,1), data=y[,1],include.mean=FALSE)
f1 = f1@fit$coef
f2 = garchFit(~ garch(1,1), data=y[,2],include.mean=FALSE)
f2 = f2@fit$coef
f3 = garchFit(~ garch(1,1), data=y[,3],include.mean=FALSE)
f3 = f3@fit$coef
f4 = garchFit(~ garch(1,1), data=y[,4],include.mean=FALSE)
f4 = f4@fit$coef
f5 = garchFit(~ garch(1,1), data=y[,5],include.mean=FALSE)
f5 = f5@fit$coef
f6 = garchFit(~ garch(1,1), data=y[,6],include.mean=FALSE)
f6 = f6@fit$coef
f7 = garchFit(~ garch(1,1), data=y[,7],include.mean=FALSE)
f7 = f7@fit$coef
f8 = garchFit(~ garch(1,1), data=y[,8],include.mean=FALSE)
f8 = f8@fit$coef
f9 = garchFit(~ garch(1,1), data=y[,9],include.mean=FALSE)
f9 = f9@fit$coef
f10 = garchFit(~ garch(1,1), data=y[,10],include.mean=FALSE)
f10 = f10@fit$coef
f11 = garchFit(~ garch(1,1), data=y[,11],include.mean=FALSE)
f11 = f11@fit$coef
f12 = garchFit(~ garch(1,1), data=y[,12],include.mean=FALSE)
f12 = f12@fit$coef
f13 = garchFit(~ garch(1,1), data=y[,13],include.mean=FALSE)
f13 = f13@fit$coef
f14 = garchFit(~ garch(1,1), data=y[,14],include.mean=FALSE)
f14 = f14@fit$coef
f15 = garchFit(~ garch(1,1), data=y[,15],include.mean=FALSE)
f15 = f15@fit$coef
f16 = garchFit(~ garch(1,1), data=y[,16],include.mean=FALSE)
f16 = f16@fit$coef
f17 = garchFit(~ garch(1,1), data=y[,17],include.mean=FALSE)
f17 = f17@fit$coef
f18 = garchFit(~ garch(1,1), data=y[,18],include.mean=FALSE)
f18 = f18@fit$coef
f19 = garchFit(~ garch(1,1), data=y[,19],include.mean=FALSE)
f19 = f19@fit$coef
f20 = garchFit(~ garch(1,1), data=y[,20],include.mean=FALSE)
f20 = f20@fit$coef
f21 = garchFit(~ garch(1,1), data=y[,21],include.mean=FALSE)
f21 = f21@fit$coef
f22 = garchFit(~ garch(1,1), data=y[,22],include.mean=FALSE)
f22 = f22@fit$coef
f23 = garchFit(~ garch(1,1), data=y[,23],include.mean=FALSE)
f23 = f23@fit$coef
f24 = garchFit(~ garch(1,1), data=y[,24],include.mean=FALSE)
f24 = f24@fit$coef
f25 = garchFit(~ garch(1,1), data=y[,25],include.mean=FALSE)
f25 = f25@fit$coef
f26 = garchFit(~ garch(1,1), data=y[,26],include.mean=FALSE)
f26 = f26@fit$coef
f27 = garchFit(~ garch(1,1), data=y[,27],include.mean=FALSE)
f27 = f27@fit$coef
f28 = garchFit(~ garch(1,1), data=y[,28],include.mean=FALSE)
f28 = f28@fit$coef
f29 = garchFit(~ garch(1,1), data=y[,29],include.mean=FALSE)
f29 = f29@fit$coef
f30 = garchFit(~ garch(1,1), data=y[,30],include.mean=FALSE)
f30 = f30@fit$coef
f31 = garchFit(~ garch(1,1), data=y[,31],include.mean=FALSE)
f31 = f31@fit$coef
f32 = garchFit(~ garch(1,1), data=y[,32],include.mean=FALSE)
f32 = f32@fit$coef
f33 = garchFit(~ garch(1,1), data=y[,33],include.mean=FALSE)
f33 = f33@fit$coef
f34 = garchFit(~ garch(1,1), data=y[,34],include.mean=FALSE)
f34 = f34@fit$coef
f35 = garchFit(~ garch(1,1), data=y[,35],include.mean=FALSE)
f35 = f35@fit$coef
f36 = garchFit(~ garch(1,1), data=y[,36],include.mean=FALSE)
f36 = f36@fit$coef
f37 = garchFit(~ garch(1,1), data=y[,37],include.mean=FALSE)
f37 = f37@fit$coef
f38 = garchFit(~ garch(1,1), data=y[,38],include.mean=FALSE)
f38 = f38@fit$coef
print("5")

a = c(f1[1],f2[1],f3[1],f4[1],f5[1],f6[1],
      f7[1],f8[1],f9[1],f10[1],f11[1],f12[1],
	f13[1],f14[1],f15[1],f16[1],f17[1],f18[1],
	f19[1],f20[1],f21[1],f22[1],f23[1],f24[1],
	f25[1],f26[1],f27[1],f28[1],f29[1],f30[1],
	f31[1],f32[1],f33[1],f34[1],f35[1],f36[1],
      f37[1],f38[1])
A = diag(c(f1[1],f2[1],f3[1],f4[1],f5[1],f6[1],
      f7[1],f8[1],f9[1],f10[1],f11[1],f12[1],
	f13[1],f14[1],f15[1],f16[1],f17[1],f18[1],
	f19[1],f20[1],f21[1],f22[1],f23[1],f24[1],
	f25[1],f26[1],f27[1],f28[1],f29[1],f30[1],
	f31[1],f32[1],f33[1],f34[1],f35[1],f36[1],
      f37[1],f38[1]))
B = diag(c(f1[1],f2[1],f3[1],f4[1],f5[1],f6[1],
      f7[1],f8[1],f9[1],f10[1],f11[1],f12[1],
	f13[1],f14[1],f15[1],f16[1],f17[1],f18[1],
	f19[1],f20[1],f21[1],f22[1],f23[1],f24[1],
	f25[1],f26[1],f27[1],f28[1],f29[1],f30[1],
	f31[1],f32[1],f33[1],f34[1],f35[1],f36[1],
      f37[1],f38[1]))
print("6")
dccpara = c(0.1,0.8)
dccresults = dcc.estimation(inia=a, iniA=A, iniB=B, ini.dcc=dccpara,dvar=y,
model="extended")

dccresults$out
A=dccresults$DCC
B=dccresults$h
write.table(A,file="I:/New Project/Stocks/dcc_stocks_bigfiletest.csv",sep = ",",col.names = NA,qmethod = "double")
write.table(B,file="I:/New Project/Stocks/variancebigfiletest.csv",sep = ",",col.names = NA,qmethod = "double")



