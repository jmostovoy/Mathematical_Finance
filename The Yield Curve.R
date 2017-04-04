####Part 1####

#Install and load packages to be used

library(lubridate)


#Set working directory
setwd("~/Documents/Mathematical_Finance")

#Import data
bonds<-read.csv("Complete_Data.csv", header=F)

#Manipulate and select relevent data
bonds<-as.data.frame(bonds)
View(bonds)
bonds<-bonds[,c(1:6)]
bonds<-bonds[-c(1:3, 48:51, 96:99, 144:147, 192:195, 240:243, 
                288:292, 336:339, 384:387, 432:435),]
rownames(bonds) <- seq(length=nrow(bonds))
bonds[,3]<-as.Date(bonds[,3], "%Y-%B-%d")
bonds[,6]<-as.Date(bonds[,6], "%Y-%B-%d")
colnames(bonds) <- c("country","coupon", 
                     "effmaturity", "price", "ytm", "date")

#Find when dates change
chgdate<-c(0, 44, 44*2, 44*3, 44*4, 44*5, 44*6, 
           44*7, 44*8, 44*8+43, 44*8+43*2)
c((chgdate[10]+1):chgdate[11])

#Initialize a date vector
x<-rep(as.Date("2017-01-20"), 10)
X<-data.frame(x, x, x, x, x, x, x, x, x, x)

#Loop to set dates of interest
for( j in c(1:10)){
  for (i in c(1:10)) {
    month(X[i,j])<-month(X[i,j])+6*i
  }
}
colnames(X)<-unique(bonds$date)
View(X)

#Intialize another date vector, and perform a loop
#that selects the closest date to that of our interest
#In 6 month intervals
y<-rep(1, 10)
Y<-data.frame(y, y, y, y, y, y, y, y, y, y)
for (j in c(1:10)) {
  for (i in c(1:10)) {
    Y[i,j]<-which(abs(bonds[c((chgdate[j]+1):chgdate[j+1]),3]
                      -X[i,j]) == min(abs(bonds[c((chgdate[j]+1):
                                                    chgdate[j+1]),3]-X[i,j])))
  }
}
View(Y)


for (i in c(1:10)) {
  X[,i]<-bonds[(chgdate[i]+c(Y[,i])),3]
}
xtable(as.data.frame(as.character(X[,1])))


chgdatetotal<-c(chgdate[1]+c(Y[,1]), chgdate[2]+c(Y[,2]),
                chgdate[3]+c(Y[,3]), chgdate[4]+c(Y[,4]),
                chgdate[5]+c(Y[,5]), chgdate[6]+c(Y[,6]),
                chgdate[7]+c(Y[,7]), chgdate[8]+c(Y[,8]),
                chgdate[9]+c(Y[,9]), chgdate[10]+c(Y[,10]))
chgdatetotal

#reduce dataframe to data of interest

subbonds<-bonds[chgdatetotal,]
View(subbonds)
rownames(subbonds) <- seq(length=nrow(subbonds))

#Some quick alterations
subbonds[,1]<-as.character(subbonds[,1])
subbonds[,2]<-as.numeric(as.character(subbonds[,2]))
subbonds[,4]<-as.numeric(as.character(subbonds[,4]))
subbonds[,5]<-as.numeric(as.character(subbonds[,5]))


#Compute yields

r<-rep(1, 10)
R<-data.frame(r, r, r, r, r, r, r, r, r, r)
for (j in c(1:10)) {
  R[1,j]<-((subbonds[(10*j-9),4])/((subbonds[(10*j-9),2])/2+100))^-1-1
}
View(R)

for (j in c(1:10)) {
  for (i in c(2:10)) {
    R[i,j]<-((subbonds[((10*j-10)+i),4]-
                sum((subbonds[((10*j-10)+i),2]/2)
                    /((1+R[c(1:(i-1)),j])^(c(1:(i-1))))))
             *(100+subbonds[((10*j-10)+i),2]/2)^(-1))^(-1/i)-1
  }
}

R<-2*R
R<-as.data.frame(unlist(R))
colnames(R)<-c("yield")
rownames(R) <- seq(length=nrow(R))

subbonds<-data.frame(subbonds, R)


#### Plots ####

plot_colours <- c("blue", "red", "forestgreen", "yellow", rgb(0.3,0.3,.3),
                  rgb(0.6,0.3,0), rgb(.9,0,0), 
                  rgb(0.3,0.6,0), rgb(0.3,0,.6), rgb(0,0.3,.6))
plot_colours1 <- plot_colours[c(1,2)]

for (i in c(1:10)) {
  pdf(file=paste("~/Documents/Mathematical_Finance/", 
                 paste("Yield_Curve_", subbonds[10*i-9,6], 
                  ".pdf", sep=""), sep=""), width = 10, height = 6)
  plot(subbonds[c((10*i-9):(10*i)),3], 100*subbonds[c((10*i-9):(10*i)),7], 
       type="l", col=plot_colours1[1], ann=FALSE)
  title(main=paste("Yield Curve for ", subbonds[10*i-9,6], sep=""), 
        col.main="forestgreen", font.main=3)
  title(xlab="Date", col.lab=rgb(0,0.6,.7))
  title(ylab="Yield (in %)" , col.lab=rgb(0,0.6,.7))
  dev.off()
}

plot(subbonds[c((1):(10)),3], 100*subbonds[c(1:10),7], type="l", col=plot_colours[1], 
     ann=FALSE)
lines(subbonds[c((10*2-9):(10*2)),3], 100*subbonds[c((10*2-9):(10*2)),7], type="l", 
      lty=1, col=plot_colours[2])
lines(subbonds[c((10*3-9):(10*3)),3], 100*subbonds[c((10*3-9):(10*3)),7], type="l", 
      lty=1, col=plot_colours[3])
lines(subbonds[c((10*4-9):(10*4)),3], 100*subbonds[c((10*4-9):(10*4)),7], type="l", 
      lty=1, col=plot_colours[4])
lines(subbonds[c((10*5-9):(10*5)),3], 100*subbonds[c((10*5-9):(10*5)),7], type="l", 
      lty=1, col=plot_colours[5])
lines(subbonds[c((10*6-9):(10*6)),3], 100*subbonds[c((10*6-9):(10*6)),7], type="l", 
      lty=1, col=plot_colours[6])
lines(subbonds[c((10*7-9):(10*7)),3], 100*subbonds[c((10*7-9):(10*7)),7], type="l", 
      lty=1, col=plot_colours[7])
lines(subbonds[c((10*8-9):(10*8)),3], 100*subbonds[c((10*8-9):(10*8)),7], type="l", 
      lty=1, col=plot_colours[8])
lines(subbonds[c((10*9-9):(10*9)),3], 100*subbonds[c((10*i-9):(10*9)),7], type="l", 
      lty=1, col=plot_colours[9])
lines(subbonds[c((10*10-9):(10*10)),3], 100*subbonds[c((10*i-10):(10*10)),7], type="l", 
      lty=1, col=plot_colours[10])

title(main="All Yield Curves", 
      col.main="forestgreen", font.main=4)
title(xlab="Date", col.lab=rgb(0,0.6,.7))
title(ylab="Yield (in %)" , col.lab=rgb(0,0.6,.7))
legend(as.Date("2017-08-01"), 1.2, paste("Yield Curve for ", 
                                         subbonds[c(1,11,21,31,41,51,61,71,81,91),6],
                                         sep=""), lty=c(1,1), 
       lwd=c(2,2),cex=.8, bty = "n", col=plot_colours)

#### Forward Rates ####

Ryrs<-as.data.frame(R[c(c(1:100)%%2==0),])
View(Ryrs)
colnames(Ryrs)<-c("yield")
Ryrs<-as.vector(t(Ryrs))

f<-rep(1,10)
f<-data.frame(f,f,f,f,f,f,f,f,f,f)
View(f)

g<-rep(1,4)
f1<-data.frame(g,g,g,g,g,g,g,g,g,g)
for (j in c(1:10)) {
  for (i in c(1:4)) {
    f1[i,j]<-(1+Ryrs[j*5-4+i])^(i+1)*(1+Ryrs[j*5-5+i])^(-(i))-1
  }
}
h<-rep(1,3)
f2<-data.frame(h,h,h,h,h,h,h,h,h,h)
for (j in c(1:10)) {
  for (i in c(1,2,3)) {
    f2[i,j]<-((1+Ryrs[j*5-3+i])^(2+i)*(1+Ryrs[j*5-5+i])^(-(i)))^(1/2)-1
  }
}
k<-rep(1,2)
f3<-data.frame(k,k,k,k,k,k,k,k,k,k)
for (j in c(1:10)) {
  for (i in c(1,2)) {
    f3[i,j]<-((1+Ryrs[j*5-2+i])^(3+i)*(1+Ryrs[j*5-5+i])^(-(i)))^(1/3)-1
  }
}
f4<-data.frame(c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1),c(1))
for (j in c(1:10)) {
  f4[1,j]<-((1+Ryrs[j*5])^(5)*(1+Ryrs[j*5-4])^(-(1)))^(1/4)-1
}
f[c(1, 5, 8, 10),]<-f1
f[c(2,6,9),]<-f2
f[c(3,7),]<-f3
f[c(4),]<-f4

colnames(f)<-unique(subbonds[,6])
rownames(f)<-c("f12", "f13", "f14", "f15", "f23", "f24", "f25", "f34",
               "f35", "f45")
f<-t(f)
f<-100*f
xtable(f, digits = 3)

####Covariance Matrix & Eigen Calculations####

timing<-rep(1, 50)
for (i in c(1:50)) {
  timing[i]<-timing[i]+2*(i-1)+1
}
bondsyr<-subbonds[timing, ]
View(bondsyr)
rownames(bondsyr) <- seq(length=nrow(bondsyr))

diff1<-diff(bondsyr[c(c(1:50)%%5==1),7])
diff2<-diff(bondsyr[c(c(1:50)%%5==2),7])
diff3<-diff(bondsyr[c(c(1:50)%%5==3),7])
diff4<-diff(bondsyr[c(c(1:50)%%5==4),7])
diff5<-diff(bondsyr[c(c(1:50)%%5==0),7])

for (i in c(1:9)) {
  diff1[i]<-log(bondsyr[5*i+1,7]/bondsyr[5*i-4,7])
}

for (i in c(1:9)) {
  diff2[i]<-log(bondsyr[5*i+2,7]/bondsyr[5*i-3,7])
}

for (i in c(1:9)) {
  diff3[i]<-log(bondsyr[5*i+3,7]/bondsyr[5*i-2,7])
}

for (i in c(1:9)) {
  diff4[i]<-log(bondsyr[5*i+4,7]/bondsyr[5*i-1,7])
}

for (i in c(1:9)) {
  diff5[i]<-log(bondsyr[5*(i+1),7]/bondsyr[5*i,7])
}

dif<-data.frame(diff1, diff2, diff3, diff4, diff5)
View(dif)

covdif<-cov(dif, dif)
xtable(10^4*covdif, digits = 4)
eeg<-eigen(cov(dif, dif), symmetric='T')
xtable(1000*as.data.frame(eeg$values), digits = 4)

####Covariance Matrix & Eigen forward####
View(f)

diff1<-c(1:9)
diff2<-c(1:9)
diff3<-c(1:9)
diff4<-c(1:9)
diff5<-c(1:9)
diff6<-c(1:9)
diff7<-c(1:9)
diff8<-c(1:9)
diff9<-c(1:9)
diff10<-c(1:9)

for (i in c(1:9)) {
  diff1[i]<-log(f[i+1,1]/f[i,1])
}

for (i in c(1:9)) {
  diff2[i]<-log(f[i+1,2]/f[i,2])
}

for (i in c(1:9)) {
  diff3[i]<-log(f[i+1,3]/f[i,3])
}

for (i in c(1:9)) {
  diff4[i]<-log(f[i+1,4]/f[i,4])
}

for (i in c(1:9)) {
  diff5[i]<-log(f[i+1,5]/f[i,5])
}

for (i in c(1:9)) {
  diff6[i]<-log(f[i+1,6]/f[i,6])
}

for (i in c(1:9)) {
  diff7[i]<-log(f[i+1,7]/f[i,7])
}

for (i in c(1:9)) {
  diff8[i]<-log(f[i+1,8]/f[i,8])
}

for (i in c(1:9)) {
  diff9[i]<-log(f[i+1,9]/f[i,9])
}

for (i in c(1:9)) {
  diff10[i]<-log(f[i+1,10]/f[i,10])
}

dif<-data.frame(diff1, diff2, diff3, diff4, diff5, 
                diff6, diff7, diff8, diff9, diff10)
View(dif)
covdif<-cov(dif, dif)
xtable(10^4*covdif, digits = 3)
eeg<-eigen(cov(dif, dif), symmetric='T')
eeg
xtable(eeg$vectors, digits = 3)


####Forward Curves####

YY1<-c(seq(subbonds$yield[1], subbonds$yield[2], length.out=200),
       seq(subbonds$yield[2], subbonds$yield[3], length.out=200),
       seq(subbonds$yield[3], subbonds$yield[4], length.out=200),
       seq(subbonds$yield[4], subbonds$yield[5], length.out=200),
       seq(subbonds$yield[5], subbonds$yield[6], length.out=200),
       seq(subbonds$yield[6], subbonds$yield[7], length.out=200),
       seq(subbonds$yield[7], subbonds$yield[8], length.out=200),
       seq(subbonds$yield[8], subbonds$yield[9], length.out=200),
       seq(subbonds$yield[9], subbonds$yield[10], length.out=200))

YY2<-YY1
YY3<-YY1
YY4<-YY1
YY5<-YY1
YY6<-YY1
YY7<-YY1
YY8<-YY1
YY9<-YY1
YY10<-YY1
length(YY1)

for (i in c(1:9)) {
  YY2[c((200*(i-1)+1):(200*i))]<-seq(subbonds_c$yield[i+10], 
                                     subbonds_c$yield[i+11], length.out=200)
}
for (i in c(1:9)) {
  YY3[c((200*(i-1)+1):(200*i))]<-seq(subbonds_c$yield[i+20], 
                                     subbonds_c$yield[i+21], length.out=200)
}
for (i in c(1:9)) {
  YY4[c((200*(i-1)+1):(200*i))]<-seq(subbonds_c$yield[i+30], 
                                     subbonds_c$yield[i+31], length.out=200)
}
for (i in c(1:9)) {
  YY5[c((200*(i-1)+1):(200*i))]<-seq(subbonds_c$yield[i+40], 
                                     subbonds_c$yield[i+41], length.out=200)
}
for (i in c(1:9)) {
  YY6[c((200*(i-1)+1):(200*i))]<-seq(subbonds_c$yield[i+50], 
                                     subbonds_c$yield[i+51], length.out=200)
}
for (i in c(1:9)) {
  YY7[c((200*(i-1)+1):(200*i))]<-seq(subbonds_c$yield[i+60], 
                                     subbonds_c$yield[i+61], length.out=200)
}
for (i in c(1:9)) {
  YY8[c((200*(i-1)+1):(200*i))]<-seq(subbonds_c$yield[i+70], 
                                     subbonds_c$yield[i+71], length.out=200)
}
for (i in c(1:9)) {
  YY9[c((200*(i-1)+1):(200*i))]<-seq(subbonds_c$yield[i+80], 
                                     subbonds_c$yield[i+81], length.out=200)
}
for (i in c(1:9)) {
  YY10[c((200*(i-1)+1):(200*i))]<-seq(subbonds_c$yield[i+90], 
                                      subbonds_c$yield[i+91], length.out=200)
}

gucci2<-seq(1,5, length.out=2*899+1)
dYnorm1<--480*diff(log(YY1))
dYnorm2<--480*diff(log(YY2))
dYnorm3<--480*diff(log(YY3))
dYnorm4<--480*diff(log(YY4))
dYnorm5<--480*diff(log(YY5))
dYnorm6<--480*diff(log(YY6))
dYnorm7<--480*diff(log(YY7))
dYnorm8<--480*diff(log(YY8))
dYnorm9<--480*diff(log(YY9))
dYnorm10<--480*diff(log(YY10))

for (i in c(1:length(dYnorm1))) {
  dYnorm1[i]<- if(i%%200==0) {
    (dYnorm1[i-1]+dYnorm1[i+1])*.5
  } else {
    dYnorm1[i]
  }
}
for (i in c(1:length(dYnorm2))) {
  dYnorm2[i]<- if(i%%200==0) {
    (dYnorm2[i-1]+dYnorm2[i+1])*.5
  } else {
    dYnorm2[i]
  }
}
for (i in c(1:length(dYnorm3))) {
  dYnorm3[i]<- if(i%%200==0) {
    (dYnorm3[i-1]+dYnorm3[i+1])*.5
  } else {
    dYnorm3[i]
  }
}
for (i in c(1:length(dYnorm4))) {
  dYnorm4[i]<- if(i%%200==0) {
    (dYnorm4[i-1]+dYnorm4[i+1])*.5
  } else {
    dYnorm4[i]
  }
}
for (i in c(1:length(dYnorm5))) {
  dYnorm5[i]<- if(i%%200==0) {
    (dYnorm5[i-1]+dYnorm5[i+1])*.5
  } else {
    dYnorm5[i]
  }
}
for (i in c(1:length(dYnorm6))) {
  dYnorm6[i]<- if(i%%200==0) {
    (dYnorm6[i-1]+dYnorm6[i+1])*.5
  } else {
    dYnorm6[i]
  }
}
for (i in c(1:length(dYnorm7))) {
  dYnorm7[i]<- if(i%%200==0) {
    (dYnorm7[i-1]+dYnorm7[i+1])*.5
  } else {
    dYnorm7[i]
  }
}
for (i in c(1:length(dYnorm8))) {
  dYnorm8[i]<- if(i%%200==0) {
    (dYnorm8[i-1]+dYnorm8[i+1])*.5
  } else {
    dYnorm8[i]
  }
}
for (i in c(1:length(dYnorm9))) {
  dYnorm9[i]<- if(i%%200==0) {
    (dYnorm9[i-1]+dYnorm9[i+1])*.5
  } else {
    dYnorm9[i]
  }
}
for (i in c(1:length(dYnorm10))) {
  dYnorm10[i]<- if(i%%200==0) {
    (dYnorm10[i-1]+dYnorm10[i+1])*.5
  } else {
    dYnorm10[i]
  }
}

lonorm2 <- loess(dYnorm5~gucci2)
xl <- seq(min(gucci2),max(gucci2), (max(gucci2) - min(gucci2))/10000)

plot(gucci2, dYnorm1, type="p", cex=.2, col=plot_colours[1], ann=FALSE)

lines(gucci2, dYnorm2, type="p", cex=.2, 
      lty=1, col=plot_colours[2])
lines(gucci2, dYnorm3, type="p", cex=.2,
      lty=1, col=plot_colours[3])
lines(gucci2, dYnorm4, type="p", cex=.2,
      lty=1, col=plot_colours[4])
lines(gucci2, dYnorm5, type="p", cex=.2,
      lty=1, col=plot_colours[5])
lines(gucci2, dYnorm6, type="p", cex=.2,
      lty=1, col=plot_colours[6])
lines(gucci2, dYnorm7,type="p", cex=.2,
      lty=1, col=plot_colours[7])
lines(gucci2, dYnorm8, type="p", cex=.2,
      lty=1, col=plot_colours[8])
lines(gucci2, dYnorm9, type="p", cex=.2,
      lty=1, col=plot_colours[9])
lines(gucci2, dYnorm10, type="p", cex=.2,
      lty=1, col=plot_colours[10])

title(main="Forward Curves", 
      col.main="forestgreen", font.main=4)
title(xlab="Year", col.lab=rgb(0,0.6,.7))
title(ylab="f(t,T)" , col.lab=rgb(0,0.6,.7))
lines(xl, predict(lonorm2,xl), col='red', lwd=2)

legend(3.8, -.3, c(paste("Forward Curve for ", 
                         subbonds_c[c(1,11,21,31,41,51,61,71,81,91),6], 
                         sep=""),"Approx. Mean Frwd Curve"), lty=c(1,1), 
       lwd=c(2,2),cex=.75, bty = "n", col=c(plot_colours,"red"))


#### Tables ####

rsb<-round(100*subbonds$yield, 3)
rsb<-data.frame(rsb[c(1:10)], rsb[c(11:20)], rsb[c(21:30)], 
                  rsb[c(31:40)], rsb[c(41:50)], rsb[c(51:60)],
                  rsb[c(61:70)], rsb[c(71:80)], rsb[c(81:90)],
                  rsb[c(91:100)])
rsb<-t(rsb)
colnames(rsb)<-c("$r_{1/2}$", "$r_{1}$" , "$r_{3/2}$" , "$r_{2}$" ,
                   "$r_{5/2}$" , "$r_{3}$"  , "$r_{7/2}$" , 
                   "$r_{4}$" , "$r_{9/2}$" , "$r_{5}$")
rownames(rsb)<-as.character(unique(subbonds$date))
View(rsb)
xtable(rsb, digits = 3)


