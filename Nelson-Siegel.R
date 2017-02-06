install.packages("YieldCurve")
library(YieldCurve)

x1<-100*subbonds$yield[c(1:10)]
x2<-100*subbonds$yield[c(11:20)]
x3<-100*subbonds$yield[c(21:30)]
x4<-100*subbonds$yield[c(31:40)]
x5<-100*subbonds$yield[c(41:50)]
x6<-100*subbonds$yield[c(51:60)]
x7<-100*subbonds$yield[c(61:70)]
x8<-100*subbonds$yield[c(71:80)]
x9<-100*subbonds$yield[c(81:90)]
x10<-100*subbonds$yield[c(91:100)]

xx<-data.frame(x1, x2, x3, x3, x5, x6, x7, x8, x9, x10)
colnames(xx)<-c("R_6M", "R_1Y", "R_1.5Y", "R_2Y", "R_2.5Y", "R_3Y", "R_3.5Y", "R_4Y", "R_4.5Y", "R_5Y")
xtssubbonds<-xts(x = t(xx), order.by = subbonds$date[c(1, 11, 21, 31, 41, 51, 61, 71, 81, 91)])

maturity.Fed <- c(.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)
NSParameters <- Nelson.Siegel( rate=xtssubbonds, maturity=maturity.Fed)
y1 <- NSrates(NSParameters[1,], maturity.Fed)
y2 <- NSrates(NSParameters[2,], maturity.Fed)
y3 <- NSrates(NSParameters[3,], maturity.Fed)
y4 <- NSrates(NSParameters[4,], maturity.Fed)
y5 <- NSrates(NSParameters[5,], maturity.Fed)
y6 <- NSrates(NSParameters[6,], maturity.Fed)
y7 <- NSrates(NSParameters[7,], maturity.Fed)
y8 <- NSrates(NSParameters[8,], maturity.Fed)
y9 <- NSrates(NSParameters[9,], maturity.Fed)
y10 <- NSrates(NSParameters[10,], maturity.Fed)
plot(maturity.Fed, xtssubbonds[4,], main="Fitted Nelson-Siegel Yield Curves",
     xlab=c("Pillars in months"), ylab=c("Yield"), type="o", col=1)
lines(maturity.Fed, y1, col=2)
lines(maturity.Fed, y2, col=3)
lines(maturity.Fed, y3, col=4)
lines(maturity.Fed, y4, col=5)
lines(maturity.Fed, y5, col=6)
lines(maturity.Fed, y6, col=7)
lines(maturity.Fed, y7, col=8)
lines(maturity.Fed, y8, col=9)
lines(maturity.Fed, y9, col=10)
lines(maturity.Fed, y10, col=11)
legend("topleft", c("Observed Avg. Yield Curve", 
                    paste("N-S Yield Curve for ", 
                          subbonds_c[c(1,11,21,31,41,51,61,71,81,91),6], 
                          sep="")),
       col=c(1:11), bty = "n" ,lty=1, cex=.8)

####Table####


rsb_NS<-data.frame(t(y1), t(y2), t(y3), t(y4), t(y5), t(y6), t(y7), t(y8),
                   t(y9), t(y10))
View(rsb_NS)
rsb_NS<-t(rsb_NS)
colnames(rsb_NS)<-c("$r_{1/2}$", "$r_{1}$" , "$r_{3/2}$" , "$r_{2}$" ,
                   "$r_{5/2}$" , "$r_{3}$"  , "$r_{7/2}$" , 
                   "$r_{4}$" , "$r_{9/2}$" , "$r_{5}$")
xtable(rsb_NS, digits = 3)

####Forward Curve ####


testnum<-runif(1000, 1, 3)
exp(-mean(predict(dY1,testnum)))

((as.numeric(y1[,10]))^5/(as.numeric(y1[,2])))^(1/4)-1
((1+as.numeric(.01*y1[,6]))^2/(1+as.numeric(.01*y1[,2])))^(1/2)-1

dY1<--3.75*diff(log(t(y1)))
dY2<--3.75*diff(log(t(y2)))
dY3<--3.75*diff(log(t(y3)))
dY4<--3.75*diff(log(t(y4)))
dY5<--3.75*diff(log(t(y5)))
dY6<--3.75*diff(log(t(y6)))
dY7<--3.75*diff(log(t(y7)))
dY8<--3.75*diff(log(t(y8)))
dY9<--3.75*diff(log(t(y9)))
dY10<--3.75*diff(log(t(y10)))
guccio<-seq(1,5, length.out=length(dY1))
dY1 <- loess(dY1~guccio)
dY2 <- loess(dY2~guccio)
dY3 <- loess(dY3~guccio)
dY4 <- loess(dY4~guccio)
dY5 <- loess(dY5~guccio)
dY6 <- loess(dY6~guccio)
dY7 <- loess(dY7~guccio)
dY8 <- loess(dY8~guccio)
dY9 <- loess(dY9~guccio)
dY10 <- loess(dY10~guccio)
xll <- seq(min(guccio),max(guccio), (max(guccio) - min(guccio))/10000)

plot(xll, predict(dY1,xll), type="l", col=plot_colours[1],lty=1, ann=FALSE)

lines(xll, predict(dY2,xll), type="l",
      lty=1, col=plot_colours[2])
lines(xll, predict(dY3,xll), type="l",
      lty=1, col=plot_colours[3])
lines(xll, predict(dY4,xll), type="l",
      lty=1, col=plot_colours[4])
lines(xll, predict(dY5,xll), type="l",
      lty=1, col=plot_colours[5])
lines(xll, predict(dY6,xll), type="l",
      lty=1, col=plot_colours[6])
lines(xll, predict(dY7,xll), type="l",
      lty=1, col=plot_colours[7])
lines(xll, predict(dY8,xll), type="l",
      lty=1, col=plot_colours[8])
lines(xll, predict(dY9,xll), type="l",
      lty=1, col=plot_colours[9])
lines(xll, predict(dY10,xll), type="l",
      lty=1, col=plot_colours[10])

title(main="N-S Forward Curves", 
      col.main="forestgreen", font.main=4)
title(xlab="Year", col.lab=rgb(0,0.6,.7))
title(ylab="f(t,T)" , col.lab=rgb(0,0.6,.7))
legend(3.4, -.4, paste("N-S Forward Curve for ", subbonds_c[c(1,11,21,31,41,51,61,71,81,91),6], sep=""), lty=c(1,1), 
       lwd=c(2,2),cex=.8, bty = "n", col=c(plot_colours))
