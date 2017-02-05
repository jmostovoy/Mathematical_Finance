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
                      -X[i,j]) == min(abs(bonds[c((chgdate[j]+1):chgdate[j+1]),3]-X[i,j])))
  }
}
View(Y)


for (i in c(1:10)) {
  X[,i]<-bonds[(chgdate[i]+c(Y[,i])),3]
}

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
    R[i,j]<-((subbonds[((10*j-10)+i),4]-sum((subbonds[((10*j-10)+i),2]/2)/((1+R[c(1:(i-1)),j])^(c(1:(i-1))))))*(100+subbonds[((10*j-10)+i),2]/2)^(-1))^(-1/i)-1
  }
}

R<-2*R
R<-as.data.frame(unlist(R))
colnames(R)<-c("yield")
rownames(R) <- seq(length=nrow(R))

subbonds<-data.frame(subbonds, R)


#### Plots ####

plot_colours <- c("blue", "red", "forestgreen", "yellow", rgb(0.3,0.3,.3), rgb(0.6,0.3,0), rgb(.9,0,0), rgb(0.3,0.6,0), rgb(0.3,0,.6), rgb(0,0.3,.6))
plot_colours1 <- plot_colours[c(1,2)]

for (i in c(1:10)) {
  pdf(file=paste("~/Documents/Mathematical_Finance/", paste("Yield_Curve_", subbonds[10*i-9,6], ".pdf", sep=""), sep=""), width = 10, height = 6)
  plot(subbonds[c((10*i-9):(10*i)),3], 100*subbonds[c((10*i-9):(10*i)),7], type="l", col=plot_colours1[1], ann=FALSE)
  title(main=paste("Yield Curve for ", subbonds[10*i-9,6], sep=""), col.main="forestgreen", font.main=3)
  title(xlab="Date", col.lab=rgb(0,0.6,.7))
  title(ylab="Yield (in %)" , col.lab=rgb(0,0.6,.7))
  dev.off()
}

plot(subbonds[c((1):(10)),3], 100*subbonds[c(1:10),7], type="l", col=plot_colours[1], ann=FALSE)
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
legend(as.Date("2017-08-01"), 1.2, paste("Yield Curve for ", subbonds[c(1,11,21,31,41,51,61,71,81,91),6], sep=""), lty=c(1,1), 
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

####Working Area####

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

cov(dif, dif)
eigen(cov(dif, dif), symmetric='T')
