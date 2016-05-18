## start dates:
## Gap 18/11/2011 # 12
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
## W e 26/01/2011 # 5
## W e 20/11/2012 # 10
## W e 28/11/2013 # 2
## W H 28/11/2013 # 2

rm(list=ls())

library(dplR)
library(xts)

# setwd("~/Cambridge/CSU 2014/cave")
#### load data

## try just front loading with the different # of time points
gap1 = read.csv("Gap_2011-2012.csv",na.strings=c("NA"," - "),header=F)
dim(gap1)
gap2 = read.csv("Gap_2012-2013.csv",na.strings=c("NA"," - "),header=F)
dim(gap2)
gap3 = read.csv("Gap_2013-2014.csv",na.strings=c("NA"," - "),header=F)
dim(gap3)
gap4 = read.csv("Gap_2014-2015.csv",na.strings=c("NA"," - "),header=F)
dim(gap4)
w1E = read.csv("WyandotteE_2011-2012.csv",na.strings=c("NA"," - "),header=F)
dim(w1E)
w2E = read.csv("WyandotteE_2012-2013.csv",na.strings=c("NA"," - "),header=F)
dim(w2E)
w3E = read.csv("WyandotteE_2013-2014.csv",na.strings=c("NA"," - "),header=F)
dim(w3E)
w1H = read.csv("WyandotteH_2013-2014.csv",na.strings=c("NA"," - "),header=F)
dim(w1H)

# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(3, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# matplot(t(w1E),type="l",col="grey",ylab="",xlab="")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# plot(rowMeans(t(w1E)),type="l",xlab="",ylab="",ylim=c(0,max(rowMeans(t(w1E)))))
# med<-apply(t(w1E), 1, median)
# lines(med,col="darkgrey")
# legend("top",c("mean","median"),lty=1,col=c("black","grey"),bty="n")
# mtext("# saturated pixels",side=2,outer=T)
# mtext("Frame during day",side=1,line=2,outer=T)
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Wyandotte Cave Entrance 2011-2012",side=3,outer=T)
# ## IF USE MUST CHANGE 0 - 24HR
# 
# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(3, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# matplot(t(w2E),type="l",col="grey",ylab="",xlab="")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# plot(rowMeans(t(w2E)),type="l",xlab="",ylab="",ylim=c(0,max(rowMeans(t(w2E)))))
# med<-apply(t(w2E), 1, median)
# lines(med,col="darkgrey")
# legend("top",c("mean","median"),lty=1,col=c("black","grey"),bty="n")
# mtext("# saturated pixels",side=2,outer=T)
# mtext("Frame during day",side=1,line=2,outer=T)
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Wyandotte Cave Entrance 2012-2013",side=3,outer=T)
# 
# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(3, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# matplot(t(w3E),type="l",col="grey",ylab="",xlab="")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# plot(rowMeans(t(w3E)),type="l",xlab="",ylab="",ylim=c(0,max(rowMeans(t(w3E)))))
# med<-apply(t(w3E), 1, median)
# lines(med,col="darkgrey")
# legend("topright",c("mean","median"),lty=1,col=c("black","grey"),bty="n")
# mtext("# saturated pixels",side=2,outer=T)
# mtext("Frame during day",side=1,line=2,outer=T)
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Wyandotte Cave Entrance 2013-2014",side=3,outer=T)

## remove > 10,000
w3E[w3E>10000] <- NA
# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(3, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# matplot(t(w3E),type="l",col="grey",ylab="",xlab="")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# rr<-range(rowMeans(t(w3E))[!is.na(rowMeans(t(w3E)))])
# plot(rowMeans(t(w3E),na.rm=T),type="l",xlab="",ylab="",ylim=c(0,max(rr)))
# w3E = read.csv("WyandotteE_2013-2014.csv",na.strings=c("NA"," - "),header=F)
# med<-apply(t(w3E), 1, median)
# lines(med,col="darkgrey")
# legend("top",c("mean","median"),lty=1,col=c("black","grey"),bty="n")
# mtext("# saturated pixels",side=2,outer=T)
# mtext("Frame during day",side=1,line=2,outer=T)
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Wyandotte Cave Entrance 2013-2014",side=3,outer=T)
# ####
# w3E[w3E>10000] <- NA
# 
# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(3, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# matplot(t(w1H),type="l",col="grey",ylab="",xlab="")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# plot(rowMeans(t(w1H)),type="l",xlab="",ylab="",ylim=c(0,max(rowMeans(t(w1H)))))
# med<-apply(t(w1H), 1, median)
# lines(med,col="darkgrey")
# legend("top",c("mean","median"),lty=1,col=c("black","grey"),bty="n")
# mtext("# saturated pixels",side=2,outer=T)
# mtext("Frame during day",side=1,line=2,outer=T)
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Wyandotte Cave deeper 2013-2014",side=3,outer=T)
# 
# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(3, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# matplot(t(gap1),type="l",col="grey",ylab="",xlab="")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# plot(rowMeans(t(gap1)),type="l",xlab="",ylab="",ylim=c(0,max(rowMeans(t(gap1)))))
# med<-apply(t(gap1), 1, median)
# lines(med,col="darkgrey")
# legend("top",c("mean","median"),lty=1,col=c("black","grey"),bty="n")
# mtext("# saturated pixels",side=2,outer=T)
# mtext("Frame during day",side=1,line=2,outer=T)
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Gap Cave 2011-2012",side=3,outer=T)
# 
# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(3, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# matplot(t(gap2),type="l",col="grey",ylab="",xlab="")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# plot(rowMeans(t(gap2)),type="l",xlab="",ylab="",ylim=c(0,max(rowMeans(t(gap2)))))
# med<-apply(t(gap2), 1, median)
# lines(med,col="darkgrey")
# legend("top",c("mean","median"),lty=1,col=c("black","grey"),bg='white',box.col="white")
# box()
# mtext("# saturated pixels",side=2,outer=T)
# mtext("Frame during day",side=1,line=2,outer=T)
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Gap Cave 2012-2013",side=3,outer=T)
# 
# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(3, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# matplot(t(gap3),type="l",col="grey",ylab="",xlab="")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# plot(rowMeans(t(gap3)),type="l",xlab="",ylab="",ylim=c(0,max(rowMeans(t(gap3)))))
# med<-apply(t(gap3), 1, median)
# lines(med,col="darkgrey")
# legend("top",c("mean","median"),lty=1,col=c("black","grey"),bty="n")
# mtext("# saturated pixels",side=2,outer=T)
# mtext("Frame during day",side=1,line=2,outer=T)
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Gap Cave 2013-2014",side=3,outer=T)
# ##
# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(3, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# matplot(t(gap4),type="l",col="grey",ylab="",xlab="")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# plot(rowMeans(t(gap4)),type="l",xlab="",ylab="",ylim=c(0,max(rowMeans(t(gap3)))))
# med<-apply(t(gap4), 1, median)
# lines(med,col="darkgrey")
# legend("topright",c("mean","median"),lty=1,col=c("black","grey"),bty="n")
# mtext("# saturated pixels",side=2,outer=T)
# mtext("Frame during day",side=1,line=2,outer=T)
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Gap Cave 2014-2015",side=3,outer=T)
# ##

par(mfrow=c(1,1))
library(matrixStats) ## this works

pdf(file = "Gap Cave 2011-2012 means.pdf")
u95ci<-rowMeans((t(gap1)))+rowSds((t(gap1)))/sqrt(length((t(gap1))[1,]))
l95ci<-rowMeans((t(gap1)))-rowSds((t(gap1)))/sqrt(length((t(gap1))[1,]))
cis<-cbind(u95ci,l95ci)
lims<-range(cis[!is.na(cis)&is.finite(cis)])
plot(rowMeans((t(gap1))),type="l",ylim=c(min(lims),max(lims)),
     main="Gap Cave 2011-2012",xlab="hours",ylab="Mean # saturated pixels",labels=F,axes=F)
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(at=seq(from=0,to=500,by=100),labels=c(0,100,200,300,400,500),side=2)
box()
axis(at=seq(from=0,to=288,by=288/24),labels=seq(from=0,to=24,by=1),side=1)
legend("topright",c("mean","95% confidence intervals"),lty=1,col=c("black","grey"),bty="n")
mtext("hours",side=1,line=3)
dev.off()

pdf(file = "Gap Cave 2012-2013 means.pdf")
u95ci<-rowMeans((t(gap2)))+rowSds((t(gap2)))/sqrt(length((t(gap2))[1,]))
l95ci<-rowMeans((t(gap2)))-rowSds((t(gap2)))/sqrt(length((t(gap2))[1,]))
plot(rowMeans((t(gap2))),type="l",ylim=c(min(l95ci),max(u95ci)),
     main="Gap Cave 2012-2013",xlab="hours",ylab="Mean # saturated pixels",labels=F,axes=F)
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(at=seq(from=5,to=35,by=5),labels=c(5,10,15,20,25,30,35),side=2)
box()
axis(at=seq(from=0,to=288,by=288/24),labels=seq(from=0,to=24,by=1),side=1)
legend("topright",c("mean","95% confidence intervals"),lty=1,col=c("black","grey"),bty="y",bg="white",box.col="white")
box()
dev.off()

pdf(file = "Gap Cave 2013-2014 means.pdf")
u95ci<-rowMeans((t(gap3)))+rowSds((t(gap3)))/sqrt(length((t(gap3))[1,]))
l95ci<-rowMeans((t(gap3)))-rowSds((t(gap3)))/sqrt(length((t(gap3))[1,]))
plot(rowMeans((t(gap3))),type="l",ylim=c(min(l95ci),max(u95ci)),
     main="Gap Cave 2013-2014",xlab="hours",ylab="Mean # saturated pixels",labels=F,axes=F)
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(at=seq(from=5,to=55,by=5),labels=c(5,10,15,20,25,30,35,40,45,50,55),side=2)
box()
axis(at=seq(from=0,to=288,by=288/24),labels=seq(from=0,to=24,by=1),side=1)
legend("topright",c("mean","95% confidence intervals"),lty=1,col=c("black","grey"),bty="y",bg="white",box.col="white")
box()
dev.off()

pdf(file = "Gap Cave 2014-2015 means.pdf")
u95ci<-rowMeans((t(gap4)))+rowSds((t(gap4)))/sqrt(length((t(gap4))[1,]))
l95ci<-rowMeans((t(gap4)))-rowSds((t(gap4)))/sqrt(length((t(gap4))[1,]))
plot(rowMeans((t(gap4))),type="l",ylim=c(min(l95ci),max(u95ci)),
     main="Gap Cave 2014-2015",xlab="hours",ylab="Mean # saturated pixels",labels=F,axes=F)
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(at=seq(from=5,to=95,by=5),labels=c(5,10,15,20,
                                        25,30,35,40,
                                        45,50,55,60,
                                        65,70,75,80,85,90,95),side=2)
box()
axis(at=seq(from=0,to=288,by=288/24),labels=seq(from=0,to=24,by=1),side=1)
legend("topright",c("mean","95% confidence intervals"),lty=1,col=c("black","grey"),bty="n")
dev.off()

pdf(file = "Wyandotte Cave 2011-2012 means.pdf")
u95ci<-rowMeans((t(w1E)))+rowSds((t(w1E)))/sqrt(length((t(w1E))[1,]))
l95ci<-rowMeans((t(w1E)))-rowSds((t(w1E)))/sqrt(length((t(w1E))[1,]))
plot(rowMeans((t(w1E))),type="l",ylim=c(min(l95ci),max(u95ci)),
     main="Wyandotte Cave 2011-2012",xlab="hours",ylab="Mean # saturated pixels",labels=F,axes=F)
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(at=seq(from=0,to=700,by=100),labels=c(0,100,200,300,400,500,600,700),side=2)
box()
axis(at=seq(from=0,to=288,by=288/24),labels=seq(from=0,to=24,by=1),side=1)
legend("top",c("mean","95% confidence intervals"),lty=1,col=c("black","grey"),bty="n")
dev.off()

pdf(file = "Wyandotte Cave 2012-2013 means.pdf")
u95ci<-rowMeans((t(w2E)))+rowSds((t(w2E)))/sqrt(length((t(w2E))[1,]))
l95ci<-rowMeans((t(w2E)))-rowSds((t(w2E)))/sqrt(length((t(w2E))[1,]))
plot(rowMeans((t(w2E))),type="l",ylim=c(min(l95ci),max(u95ci)),
     main="Wyandotte Cave 2012-2013",xlab="hours",ylab="Mean # saturated pixels",labels=F,axes=F)
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(at=seq(from=0,to=500,by=100),labels=c(0,100,200,300,400,500),side=2)
box()
axis(at=seq(from=0,to=288,by=288/24),labels=seq(from=0,to=24,by=1),side=1)
legend("top",c("mean","95% confidence intervals"),lty=1,col=c("black","grey"),bty="n")
dev.off()

pdf(file = "Wyandotte Cave 2013-2014 means.pdf")
u95ci<-rowMeans((t(w3E)),na.rm=T)+rowSds((t(w3E)))/sqrt(length((t(w3E))[1,]))
l95ci<-rowMeans((t(w3E)),na.rm=T)-rowSds((t(w3E)))/sqrt(length((t(w3E))[1,]))
cis<-cbind(u95ci,l95ci)
lims<-range(cis[!is.na(cis)&is.finite(cis)])
plot(rowMeans((t(w3E)),na.rm=T),type="l",ylim=c(min(lims),max(lims)),
     main="Wyandotte Cave 2013-2014",xlab="hours",ylab="Mean # saturated pixels",labels=F,axes=F)
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(at=seq(from=0,to=1500,by=250),labels=c(0,250,500,750,1000,1250,1500),side=2)
box()
axis(at=seq(from=0,to=288,by=288/24),labels=seq(from=0,to=24,by=1),side=1)
legend("top",c("mean","95% confidence intervals"),lty=1,col=c("black","grey"),bty="n")
dev.off()

pdf(file = "Wyandotte Cave deeper 2013-2014 means.pdf")
u95ci<-rowMeans((t(w1H)))+rowSds((t(w1H)))/sqrt(length((t(w1H))[1,]))
l95ci<-rowMeans((t(w1H)))-rowSds((t(w1H)))/sqrt(length((t(w1H))[1,]))
plot(rowMeans((t(w1H))),type="l",ylim=c(min(l95ci),max(u95ci)),
     main="Wyandotte Cave deeper 2013-2014",xlab="hours",ylab="Mean # saturated pixels",labels=F,axes=F)
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(at=seq(from=0,to=50,by=10),labels=c(0,10,20,30,40,50),side=2)
box()
axis(at=seq(from=0,to=288,by=288/24),labels=seq(from=0,to=24,by=1),side=1)
legend("top",c("mean","95% confidence intervals"),lty=1,col=c("black","grey"),bty="n")
dev.off()
################################################
# gap 1

tgap1<-as.matrix(gap1,header=F)
tgap1<-as.vector(t(tgap1))

pdf(file = "Gap Cave 2011-2012.pdf")
#graphics.off()
plot(tgap1,type="l",
     main="Gap Cave 2011-2012",xlab="weeks",ylab="# saturated pixels",labels=F,axes=F)
axis(at=seq(from=0,to=20000,by=10000),labels=c(0,10000,20000),side=2)
box()
axis(at=seq(from=0,to=length(tgap1),by=288*7),labels=seq(from=0,to=14,by=1),side=1)
dev.off()

 wtgap1<-morlet(sqrt(tgap1), x1 = seq_along(tgap1), p2 = NULL, dj = 0.25, siglvl = 0.95)
 pdf(file = "Gap Cave 2011-2012 morlet.pdf") 
 wavelet.plot(wtgap1,crn.lab="Pixel",x.lab="Time")
# graphics.off()
 dev.off()
# 
# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(4, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# plot(tgap1,type="l", ylab="# saturated pixels",xlab="Overwinter (frame #)")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# acf(tgap1, lag.max = length(tgap1),plot=T,main="",xlab="Lag")
# #mtext("# saturated pixels",side=2,outer=T)
# #mtext("Frame during day",side=1,line=2,outer=T)
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Gap Cave 2011-2012",side=3,outer=T)

#gap1.f1 <- filter(tgap1,filter=rep(1/250,250))
pdf(file = "Gap Cave 2011-2012 acf.pdf")
par(mfrow=c(1,1))
#plot(gap1.f1,col="red",lwd=2,lty=1,ylab="# saturated pixels")
# subsample
# i <- 288
# subgap1<-tgap1[1:(i+10)==(i+10)]
# layout(1:2)
# par(mfrow=c(1,2))
# plot(subgap1,type="l", ylab="# saturated pixels",main="Gap Cave 2011-2012 [sub]")
# acf(subgap1, lag.max = length(subgap1),plot=T,main="Gap Cave 2011-2012 [sub]")
# acf(sqrt(tgap1), lag.max = length(tgap1),plot=T,main="Gap Cave 2011-2012",xlab="Lag")
acf(tgap1, lag.max = 288*10,plot=T,main="Gap Cave 2011-2012",xlab="Lag (Days)",labels=F,axes=F,
    ylab="Autocorrelation")
axis(at=c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),side=2)
box()
axis(at=seq(from=0,to=288*10,by=288),labels=seq(from=0,to=10,by=1),side=1)
legend("topright","p-value = 0.05",lty=2,col="blue",bty="n")
# acf(sqrt(tgap1), lag.max = length(tgap1),plot=T,main="Gap Cave 2011-2012",xlab="Lag")
dev.off()

# gap 2
tgap2<-as.matrix(gap2)
tgap2<-as.vector(t(tgap2))
#graphics.off()
pdf(file = "Gap Cave 2012-2013.pdf") 
plot(tgap2,type="l",xlab="weeks",main="Gap Cave 2012-2013",ylab="# saturated pixels",labels=F,axes=F)
axis(at=seq(from=0,to=1000,by=500),labels=c(0,500,1000),side=2)
box()
axis(at=seq(from=0,to=length(tgap2),by=288*7),labels=seq(from=0,to=20,by=1),side=1)
dev.off()

wtgap2<-morlet(sqrt(tgap2), x1 = seq_along(tgap2), p2 = NULL, dj = 0.25, siglvl = 0.95)
pdf(file = "Gap Cave 2012-2013 morlet.pdf") 
wavelet.plot(wtgap2,crn.lab="Pixel",x.lab="Time")
# graphics.off()
dev.off()

# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(4, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# plot(tgap2,type="l", ylab="# saturated pixels",xlab="Overwinter (frame #)")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# acf(tgap2, lag.max = length(tgap2),plot=T,main="",xlab="Lag")
# #mtext("# saturated pixels",side=2,outer=T)
# #mtext("Frame during day",side=1,line=2,outer=T)
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Gap Cave 2012-2013",side=3,outer=T)

#gap2.f1 <- filter(tgap2,filter=rep(1/250,250))
pdf(file = "Gap Cave 2012-2013 acf.pdf")
par(mfrow=c(1,1))
#plot(gap2.f1,col="red",lwd=2,lty=1,ylab="# saturated pixels")
# subsample
#i <- 288
#subgap2<-tgap2[1:(i+10)==(i+10)]
#layout(1:2)
#par(mfrow=c(1,2))
#plot(subgap2,type="l", ylab="# saturated pixels",main="Gap Cave 2012-2013 [sub]")
#acf(subgap2, lag.max = length(subgap2),plot=T,main="Gap Cave 2012-2013 [sub]")
#acf(sqrt(tgap2), lag.max = length(tgap2),plot=T,main="Gap Cave 2012-2013",xlab="Lag")
acf(tgap2, lag.max = 288*10,plot=T,main="Gap Cave 2012-2013",xlab="Lag (Days)",labels=F,axes=F,
    ylab="Autocorrelation")
axis(at=c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),side=2)
box()
axis(at=seq(from=0,to=288*10,by=288),labels=seq(from=0,to=10,by=1),side=1)
legend("topright","p-value = 0.05",lty=2,col="blue",bty="n")
dev.off()

# gap 3
tgap3<-as.matrix(gap3)
tgap3<-as.vector(t(tgap3))
#graphics.off()
pdf(file = "Gap Cave 2013-2014.pdf")
plot(tgap3,type="l",main="Gap Cave 2013-2014",ylab="# saturated pixels",labels=F,axes=F,xlab="weeks")
axis(at=seq(from=0,to=1000,by=500),labels=c(0,500,1000),side=2)
box()
axis(at=seq(from=0,to=length(tgap3),by=288*7),labels=seq(from=0,to=10,by=1),side=1)
dev.off()

wtgap3<-morlet(sqrt(tgap3), x1 = seq_along(tgap3), p2 = NULL, dj = 0.25, siglvl = 0.95)
pdf(file = "Gap Cave 2013-2014 morlet.pdf") 
wavelet.plot(wtgap3,crn.lab="Pixel",x.lab="Time")
dev.off()

# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(4, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# plot(tgap3,type="l", ylab="# saturated pixels",xlab="Overwinter (frame #)")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# acf(tgap3, lag.max = length(tgap3),plot=T,main="",xlab="Lag")
# #mtext("# saturated pixels",side=2,outer=T)
# #mtext("Frame during day",side=1,line=2,outer=T)
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Gap Cave 2013-2014",side=3,outer=T)

#gap3.f1 <- filter(tgap3,filter=rep(1/250,250))
pdf(file = "Gap Cave 2013-2014 acf.pdf")
par(mfrow=c(1,1))
#plot(gap3.f1,col="red",lwd=2,lty=1,ylab="# saturated pixels")
# subsample
#i <- 288
#subgap3<-tgap3[1:(i+10)==(i+10)]
#layout(1:2)
#par(mfrow=c(1,2))
#plot(subgap3,type="l", ylab="# saturated pixels",main="Gap Cave 2013-2014 [sub]")
#acf(subgap3, lag.max = length(subgap3),plot=T,main="Gap Cave 2013-2014 [sub]")
#acf(sqrt(tgap3), lag.max = length(tgap3),plot=T,main="Gap Cave 2013-2014",xlab="Lag")
acf(tgap3, lag.max = 288*10,plot=T,main="Gap Cave 2013-2014",xlab="Lag (Days)",labels=F,axes=F,
    ylab="Autocorrelation")
axis(at=c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),side=2)
box()
axis(at=seq(from=0,to=288*10,by=288),labels=seq(from=0,to=10,by=1),side=1)
legend("topright","p-value = 0.05",lty=2,col="blue",bty="n")
dev.off()

# gap 4
tgap4<-as.matrix(gap4)
tgap4<-as.vector(t(tgap4))
#graphics.off()
pdf(file = "Gap Cave 2014-2015.pdf")
plot(tgap4,type="l",main="Gap Cave 2014-2015",ylab="# saturated pixels",labels=F,axes=F,xlab="weeks")
axis(at=seq(from=0,to=1000,by=500),labels=c(0,500,1000),side=2)
box()
axis(at=seq(from=0,to=length(tgap4),by=288*7),labels=seq(from=0,to=19,by=1),side=1)
dev.off()
# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(4, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# plot(tgap4,type="l", ylab="# saturated pixels",xlab="Overwinter (frame #)")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# acf(tgap4, lag.max = length(tgap4),plot=T,main="",xlab="Lag")
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Gap Cave 2014-2015",side=3,outer=T)
pdf(file = "Gap Cave 2014-2015 acf.pdf")
par(mfrow=c(1,1))
acf(tgap4, lag.max = 288*10,plot=T,main="Gap Cave 2014-2015",xlab="Lag (Days)",labels=F,axes=F,
    ylab="Autocorrelation")
axis(at=c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),side=2)
box()
axis(at=seq(from=0,to=288*10,by=288),labels=seq(from=0,to=10,by=1),side=1)
legend("topright","p-value = 0.05",lty=2,col="blue",bty="n")
dev.off()

wtgap4<-morlet(sqrt(tgap4), x1 = seq_along(tgap4), p2 = NULL, dj = 0.25, siglvl = 0.95)
pdf(file = "Gap Cave 2014-2015 morlet.pdf") 
wavelet.plot(wtgap4,crn.lab="Pixel",x.lab="Time")
dev.off()

# w1E
tw1E<-as.matrix(w1E)
tw1E<-as.vector(t(tw1E))
#graphics.off()
pdf(file = "Wyandotte Cave 2011-2012.pdf")
plot(tw1E,type="l",main="Wyandotte Cave 2011-2012",ylab="# saturated pixels",labels=F,axes=F,xlab="weeks")
axis(at=seq(from=0,to=10000,by=5000),labels=c(0,5000,10000),side=2)
box()
axis(at=seq(from=0,to=length(tw1E),by=288*7),labels=seq(from=0,to=9,by=1),side=1)
dev.off()

wtw1E<-morlet(sqrt(tw1E), x1 = seq_along(tw1E), p2 = NULL, dj = 0.25, siglvl = 0.95)
pdf(file = "Wyandotte Cave 2011-2012 morlet.pdf")
wavelet.plot(wtw1E,crn.lab="Pixel",x.lab="Time")
#graphics.off()
dev.off()
# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(4, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# plot(tw1E,type="l", ylab="# saturated pixels",xlab="Overwinter (frame #)")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# acf(tw1E, lag.max = length(tw1E),plot=T,main="",xlab="Lag")
# #mtext("# saturated pixels",side=2,outer=T)
# #mtext("Frame during day",side=1,line=2,outer=T)
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Wyandotte Cave 2011-2012",side=3,outer=T)

#tw1E.f1 <- filter(tw1E,filter=rep(1/250,250))
pdf(file = "Wyandotte Cave 2011-2012 acf.pdf")
par(mfrow=c(1,1))
#plot(tw1E.f1,col="red",lwd=2,lty=1,ylab="# saturated pixels")
# subsample
#i <- 288
#subw1E<-tw1E[1:(i+10)==(i+10)]
#layout(1:2)
#par(mfrow=c(1,2))
#plot(subw1E,type="l", ylab="# saturated pixels",main="Wyandotte Cave 2011-2012 [sub]")
#acf(subw1E, lag.max = length(subw1E),plot=T,main="Wyandotte Cave 2011-2012 [sub]")
#acf(sqrt(tw1E), lag.max = length(tw1E),plot=T,main="Wyandotte Cave 2011-2012",xlab="Lag")
acf(tw1E, lag.max = 288*10,plot=T,main="Wyandotte Cave 2011-2012",xlab="Lag (Days)",labels=F,axes=F,
    ylab="Autocorrelation")
axis(at=c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),side=2)
box()
axis(at=seq(from=0,to=288*10,by=288),labels=seq(from=0,to=10,by=1),side=1)
legend("topright","p-value = 0.05",lty=2,col="blue",bty="n")
dev.off()

#w2E
tw2E<-as.matrix(w2E)
tw2E<-as.vector(t(tw2E))
#graphics.off()
pdf(file = "Wyandotte Cave 2012-2013.pdf")
plot(tw2E,type="l",main="Wyandotte Cave 2012-2013",ylab="# saturated pixels",labels=F,axes=F,xlab="weeks")
axis(at=seq(from=0,to=5000,by=2500),labels=c(0,2500,5000),side=2)
box()
axis(at=seq(from=0,to=length(tw2E),by=288*7),labels=seq(from=0,to=18,by=1),side=1)
dev.off()

wtw2E<-morlet(sqrt(tw2E), x1 = seq_along(tw2E), p2 = NULL, dj = 0.25, siglvl = 0.95)
pdf(file = "Wyandotte Cave 2012-2013 morlet.pdf")
wavelet.plot(wtw2E,crn.lab="Pixel",x.lab="Time")
dev.off()

# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(4, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# plot(tw2E,type="l", ylab="# saturated pixels",xlab="Overwinter (frame #)")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# acf(tw2E, lag.max = length(tw2E),plot=T,main="",xlab="Lag")
# #mtext("# saturated pixels",side=2,outer=T)
# #mtext("Frame during day",side=1,line=2,outer=T)
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Wyandotte Cave 2012-2013",side=3,outer=T)

#w2E.f1 <- filter(tw2E,filter=rep(1/250,250))
pdf(file = "Wyandotte Cave 2012-2013 acf.pdf")
par(mfrow=c(1,1))
#plot(w2E.f1,col="red",lwd=2,lty=1,ylab="# saturated pixels")
# subsample
#i <- 288
#subw2E<-tw2E[1:(i+10)==(i+10)]
#layout(1:2)
#par(mfrow=c(1,2))
#plot(subw2E,type="l", ylab="# saturated pixels",main="Wyandotte Cave 2012-2013 [sub]")
#acf(subw2E, lag.max = length(subw2E),plot=T,main="Wyandotte Cave 2012-2013 [sub]")
#acf(sqrt(tw2E), lag.max = length(tw2E),plot=T,main="Wyandotte Cave 2012-2013",xlab="Lag")
acf(tw2E, lag.max = 288*10,plot=T,main="Wyandotte Cave 2012-2013",xlab="Lag (Days)",labels=F,axes=F,
    ylab="Autocorrelation")
axis(at=c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),side=2)
box()
axis(at=seq(from=0,to=288*10,by=288),labels=seq(from=0,to=10,by=1),side=1)
legend("topright","p-value = 0.05",lty=2,col="blue",bty="n")
dev.off()


# wtw2H<-morlet(log(tw2E+1)-mean(log(tw2E+1)), x1 = seq_along(tw2E), p2 = NULL, dj = 0.25, siglvl = 0.95)
# windows() # if plot too large
# wavelet.plot(wtw2H,crn.lab="Pixel",x.lab="Time")
#graphics.off()
#axis(at=0:10,labels=0:10,side=3)
#w3E
tw3E<-as.matrix(w3E)
tw3E<-as.vector(t(tw3E))
#graphics.off()
pdf(file = "Wyandotte Cave 2013-2014.pdf")
plot(tw3E,type="l",main="Wyandotte Cave 2013-2014",ylab="# saturated pixels",labels=F,axes=F,xlab="weeks")
axis(at=seq(from=0,to=10000,by=5000),labels=c(0,5000,10000),side=2)
box()
axis(at=seq(from=0,to=length(tw3E),by=288*7),labels=seq(from=0,to=16,by=1),side=1)
dev.off()

wtw3E<-morlet(sqrt(tw3E), x1 = seq_along(tw3E), p2 = NULL, dj = 0.25, siglvl = 0.95)
pdf(file = "Wyandotte Cave 2013-2014 morlet.pdf")
wavelet.plot(wtw3E,crn.lab="Pixel",x.lab="Time")
dev.off()

# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(4, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# plot(tw3E,type="l", ylab="# saturated pixels",xlab="Overwinter (frame #)")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# acf(tw3E,na.action=na.pass, lag.max = length(tw3E),plot=T,main="",xlab="Lag")
# #mtext("# saturated pixels",side=2,outer=T)
# #mtext("Frame during day",side=1,line=2,outer=T)
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Wyandotte Cave 2013-2014",side=3,outer=T)

#w3E.f1 <- filter(tw3E,filter=rep(1/250,250))
pdf(file = "Wyandotte Cave 2013-2014 acf.pdf")
par(mfrow=c(1,1))
#plot(w3E.f1,col="red",lwd=2,lty=1,ylab="# saturated pixels")
# subsample
#i <- 288
#subw3E<-tw3E[1:(i+10)==(i+10)]
#layout(1:2)
#par(mfrow=c(1,2))
#plot(subw3E,type="l", ylab="# saturated pixels",main="Wyandotte Cave 2013-2014 [sub]")
#acf(subw3E, lag.max = length(subw3E),plot=T,main="Wyandotte Cave 2013-2014 [sub]")
#acf(sqrt(tw3E), lag.max = length(tw3E),plot=T,main="Wyandotte Cave 2013-2014",xlab="Lag")
acf(tw3E,na.action=na.pass, lag.max = 288*10,plot=T,main="Wyandotte Cave 2013-2014",xlab="Lag (Days)",labels=F,axes=F,
    ylab="Autocorrelation")
axis(at=c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),side=2)
box()
axis(at=seq(from=0,to=288*10,by=288),labels=seq(from=0,to=10,by=1),side=1)
legend("topright","p-value = 0.05",lty=2,col="blue",bty="n")
dev.off()

#w1H
tw1H<-as.matrix(w1H)
tw1H<-as.vector(t(tw1H))
#graphics.off()

pdf(file = "Wyandotte Cave deeper 2013-2014.pdf")
plot(tw1H,type="l",main="Wyandotte Cave deeper 2013-2014",ylab="# saturated pixels",labels=F,axes=F,xlab="weeks")
axis(at=seq(from=0,to=1000,by=500),labels=c(0,500,1000),side=2)
box()
axis(at=seq(from=0,to=length(tw1H),by=288*7),labels=seq(from=0,to=16,by=1),side=1)
dev.off()

wtw1H<-morlet(sqrt(tw1H), x1 = seq_along(tw1H), p2 = NULL, dj = 0.25, siglvl = 0.95)
pdf(file = "Wyandotte Cave deeper 2013-2014 morlet.pdf")
wavelet.plot(wtw1H,crn.lab="Pixel",x.lab="Time")
dev.off()

# par(omi=c(1,0.5,0.5,0.5))
# par(mai=c(0.8,0.8,0.8,0.8))
# par(mar=c(4, 4, 3, 2) + 0.1)
# par(mfrow=c(1,2))
# plot(tw1H,type="l", ylab="# saturated pixels",xlab="Overwinter (frame #)")
# mtext('a',font=2,side=3,line=1,at=-2,cex=1)
# acf(tw1H, lag.max = length(tw1H),plot=T,main="",xlab="Lag")
# #mtext("# saturated pixels",side=2,outer=T)
# #mtext("Frame during day",side=1,line=2,outer=T)
# mtext('b',font=2,side=3,line=1,at=-2,cex=1)
# mtext("Wyandotte Cave deeper 2013-2014",side=3,outer=T)

#w1H.f1 <- filter(tw1H,filter=rep(1/250,250))
pdf(file = "Wyandotte Cave deeper 2013-2014 acf.pdf")
par(mfrow=c(1,1))
#plot(w1H.f1,col="red",lwd=2,lty=1,ylab="# saturated pixels")
# subsample
#i <- 288
#subw1H<-tw1H[1:(i+10)==(i+10)]
#layout(1:2)
#par(mfrow=c(1,2))
#plot(subw1H,type="l", ylab="# saturated pixels",main="Wyandotte Cave deeper 2013-2014 [sub]")
#acf(subw1H, lag.max = length(subw1H),plot=T,main="Wyandotte Cave deeper 2013-2014 [sub]")
#acf(sqrt(tw1H), lag.max = length(tw1H),plot=T,main="Wyandotte Cave deeper 2013-2014",xlab="Lag")
acf(tw1H, lag.max = 288*10,plot=T,main="Wyandotte Cave deeper 2013-2014",xlab="Lag (Days)",labels=F,axes=F,
    ylab="Autocorrelation")
axis(at=c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),side=2)
box()
axis(at=seq(from=0,to=288*10,by=288),labels=seq(from=0,to=10,by=1),side=1)
legend("topright","p-value = 0.05",lty=2,col="blue",bty="n")
dev.off()

# ## spectral analysis
# require(multitaper);
# resSpec <-  spec.mtm(as.ts(rowMeans(t(w2E))), #k=10, nw=5.0,
#                     nFFT = "default",
#                     #centreWithSlepians = TRUE,
#                     Ftest = TRUE,
#                     jackknife = FALSE, maxAdaptiveIterations = 100,
#                     plot = TRUE, na.action = na.fail)
# spectrum(as.ts(rowMeans(t(w2E))),span=100,ci=0.95)
# 
## plot data
# plot(x=0:152352,type="n", axes=F, xlab="", ylab="",ylim=c(0,max(c(tgap1,tgap2,tw1E,tw2E))*2),main="")
# points(x=((70*288)+1):((70*288)+length(tw1E)),y=tw1E,type="l")
# points(x=((369*288)+1):((369*288)+length(tw2E)),y=tw2E,type="l")
#mtext('b',font=2,side=3,line=-10,at=-2,cex=1)
#
#points(x=1:length(tgap1),y=tgap1+max(c(tgap1,tgap2,tw1E,tw2E)),type="l")
#points(x=((383*288)+1):((383*288)+length(tgap2)),y=tgap2+max(c(tgap1,tgap2,tw1E,tw2E)),type="l")
#mtext(2,text="No. of saturated pixels",line=2,cex=0.8)
#mtext('a',font=2,side=3,line=1,at=-2,cex=1)

#mtext(2,text="2012-2013         2011-2012",line=1,cex=0.8)
#axis(side=1, at = seq(from =0, to= 152352, by = (288*30)),
#     labels = c("Nov","Dec","Jan","Feb","Mar","Apr","May","Jun",
#                    "Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr"), tick = TRUE,
#    lty=2)
#
## plot data # 2 - each cave with own x-axis 
# par(omi=c(0.2,0.5,0.2,0.2))
# par(mai=c(0.8,0.2,0.2,0.2))
# par(mar=c(4, 4, 3, 2) + 0.1)
# par(mfrow=c(2,3))
# plot(x=0:(6*30*288),type="n", axes=F, xlab="", ylab="",
#      ylim=c(0,max(c(tgap1,tgap2,tw1E,tw2E))),
#      main="")
# points(x=((68*288)+1):((68*288)+length(tw1E)),y=tw1E,type="l")
# axis(side=1, at = seq(from =0, to= (6*30*288), by = (288*30)),
#      labels = c("Nov","Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
#      lty=2)
# mtext('a',font=1,side=3,line=1,at=-2,cex=1)
# plot(x=0:(6*30*288),type="n", axes=F, xlab="", ylab="",
#      ylim=c(0,max(c(tgap1,tgap2,tw1E,tw2E))),
#      main="")
# points(x=1:length(tw2E),y=tw2E,type="l")
# mtext('b',font=1,side=3,line=1,at=-2,cex=1)
# mtext(2,text="No. of saturated pixels",cex=1,outer=T)
# axis(side=1, at = seq(from =0, to= (6*30*288), by = (288*30)),
#      labels = c("Nov","Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
#      lty=2)
# 
# plot(x=0:(6*30*288),type="n", axes=F, xlab="", ylab="",
#      ylim=c(0,max(c(tgap1,tgap2,tw1E,tw2E))),
#      main="")
# points(x=((18*288)+1):((18*288)+length(tgap2)),y=tgap2,type="l")
# axis(side=1, at = seq(from =0, to= (6*30*288), by = (288*30)),
#      labels = c("Nov","Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
#      lty=2)
# mtext('c',font=1,side=3,line=1,at=-2,cex=1)
# 
# plot(x=0:(6*30*288),type="n", axes=F, xlab="", ylab="",
#      ylim=c(0,max(c(tgap1,tgap2,tw1E,tw2E))),
#      main="")
# points(x=1:length(tgap1),y=tgap1,type="l")
# axis(side=1, at = seq(from =0, to= (6*30*288), by = (288*30)),
#      labels = c("Nov","Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
#      lty=2)
# mtext('d',font=1,side=3,line=1,at=-2,cex=1)
# 

##################################################
## plot means over time
id_wy12 <- c(31:43, 59:64)
id_wy13 <-c(69:76)
id_wyH13 <-c(69, 74, 83, 85)
##
## W e 26/01/2011 # 5
## W e 20/11/2012 # 10
## W e 28/11/2013 # 2
## W H 28/11/2013 # 2
##

par(mfrow=c(1,1))

##
dim(t(w1E))
pdf(file = "Wyandotte 2011-2012 month.pdf")
par(mfrow=c(2,2))
par(oma=c(1,2,1,2))
par(mar=c(2,2,1,1))
# set upper limit for scaling axes

u95ci<-rowMeans(t(w1E)[,1:5])+rowSds(t(w1E)[,1:5])/sqrt(length((t(w1E)[1,])))
l95ci<-rowMeans(t(w1E)[,1:5])-rowSds(t(w1E)[,1:5])/sqrt(length((t(w1E))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(w1E)[,1:5]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",
     col="black",ylab="# saturated pixels",main="Wyandotte January (2011-2012)",bty="n")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
## plot means over time
##
## W e 26/01/2011 # 5
## W e 20/11/2012 # 10
## W e 28/11/2013 # 2
## W H 28/11/2013 # 2
##

u95ci<-rowMeans(t(w1E)[,6:34])+rowSds(t(w1E)[,6:34])/sqrt(length((t(w1E)[1,])))
l95ci<-rowMeans(t(w1E)[,6:34])-rowSds(t(w1E)[,6:34])/sqrt(length((t(w1E))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(w1E)[,6:34]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Wyandotte February (2011-2012)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)

u95ci<-rowMeans(t(w1E)[,35:65])+rowSds(t(w1E)[,35:65])/sqrt(length((t(w1E)[1,])))
l95ci<-rowMeans(t(w1E)[,35:65])-rowSds(t(w1E)[,35:65])/sqrt(length((t(w1E))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(w1E)[,35:65]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Wyandotte March (2011-2012)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)

dev.off()
## plot means over time
##
## W e 26/01/2011 # 5
## W e 20/11/2012 # 10
## W e 28/11/2013 # 2
## W H 28/11/2013 # 2
##

##
dim(t(w2E))
pdf(file = "Wyandotte 2012-2013 month.pdf")
par(mfrow=c(3,2))
par(oma=c(1,4,1,2))
par(mar=c(4,4,1,1))
u95ci<-rowMeans(t(w2E)[,1:10])+rowSds(t(w2E)[,1:10])/sqrt(length((t(w2E)[1,])))
l95ci<-rowMeans(t(w2E)[,1:10])-rowSds(t(w2E)[,1:10])/sqrt(length((t(w2E))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(w2E)[,1:10]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",
     col="black",ylab="# saturated pixels",main="Wyandotte November (2012-2013)",bty="n")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
lines(rowMeans(t(w2E)[,1:10]))
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
## plot means over time
##
## W e 26/01/2011 # 5
## W e 20/11/2012 # 10
## W e 28/11/2013 # 2
## W H 28/11/2013 # 2
##

u95ci<-rowMeans(t(w2E)[,11:42])+rowSds(t(w2E)[,11:42])/sqrt(length((t(w2E)[1,])))
l95ci<-rowMeans(t(w2E)[,11:42])-rowSds(t(w2E)[,11:42])/sqrt(length((t(w2E))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(w2E)[,11:42]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Wyandotte December (2012-2013)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
## plot means over time
##
## W e 26/01/2011 # 5
## W e 20/11/2012 # 10
## W e 28/11/2013 # 2
## W H 28/11/2013 # 2
##

u95ci<-rowMeans(t(w2E)[,43:72])+rowSds(t(w2E)[,43:72])/sqrt(length((t(w2E)[1,])))
l95ci<-rowMeans(t(w2E)[,43:72])-rowSds(t(w2E)[,43:72])/sqrt(length((t(w2E))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(w2E)[,43:72]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Wyandotte January (2012-2013)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
## plot means over time
##
## W e 26/01/2011 # 5
## W e 20/11/2012 # 10
## W e 28/11/2013 # 2
## W H 28/11/2013 # 2
##

u95ci<-rowMeans(t(w2E)[,73:100])+rowSds(t(w2E)[,73:100])/sqrt(length((t(w2E)[1,])))
l95ci<-rowMeans(t(w2E)[,73:100])-rowSds(t(w2E)[,73:100])/sqrt(length((t(w2E))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(w2E)[,73:100]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Wyandotte February (2012-2013)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)

u95ci<-rowMeans(t(w2E)[,101:130])+rowSds(t(w2E)[,101:130])/sqrt(length((t(w2E)[1,])))
l95ci<-rowMeans(t(w2E)[,101:130])-rowSds(t(w2E)[,101:130])/sqrt(length((t(w2E))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(w2E)[,101:130]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Wyandotte March (2012-2013)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)

dev.off()

##
## plot means over time
##
## W e 26/01/2011 # 5
## W e 20/11/2012 # 10
## W e 28/11/2013 # 2
## W H 28/11/2013 # 2
##
dim(t(w3E))
pdf(file = "Wyandotte 2013-2014 month.pdf")
par(mfrow=c(3,2))
par(oma=c(1,4,1,2))
par(mar=c(4,4,1,1))
u95ci<-rowMeans(t(w3E)[,1:2])+rowSds(t(w3E)[,1:2])/sqrt(length((t(w3E)[1,])))
l95ci<-rowMeans(t(w3E)[,1:2])-rowSds(t(w3E)[,1:2])/sqrt(length((t(w3E))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(w3E)[,1:2]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",
     col="black",ylab="# saturated pixels",main="Wyandotte November (2013-2014)",bty="n")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
## plot means over time
##
## W e 26/01/2011 # 5
## W e 20/11/2012 # 10
## W e 28/11/2013 # 2
## W H 28/11/2013 # 2
##

u95ci<-rowMeans(t(w3E)[,3:34])+rowSds(t(w3E)[,3:34])/sqrt(length((t(w3E)[1,])))
l95ci<-rowMeans(t(w3E)[,3:34])-rowSds(t(w3E)[,3:34])/sqrt(length((t(w3E))[1,]))
lims<-na.omit(cbind(u95ci,l95ci))
plot(rowMeans(t(w3E)[,3:34],na.rm=T),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Wyandotte December (2013-2014)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
## plot means over time
##
## W e 26/01/2011 # 5
## W e 20/11/2012 # 10
## W e 28/11/2013 # 2
## W H 28/11/2013 # 2
##

u95ci<-rowMeans(t(w3E)[,35:62])+rowSds(t(w3E)[,35:62])/sqrt(length((t(w3E)[1,])))
l95ci<-rowMeans(t(w3E)[,35:62])-rowSds(t(w3E)[,35:62])/sqrt(length((t(w3E))[1,]))
lims<-na.omit(cbind(u95ci,l95ci))
plot(rowMeans(t(w3E)[,35:62]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Wyandotte January (2013-2014)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
## plot means over time
##
## W e 26/01/2011 # 5
## W e 20/11/2012 # 10
## W e 28/11/2013 # 2
## W H 28/11/2013 # 2
##

u95ci<-rowMeans(t(w3E)[,63:91])+rowSds(t(w3E)[,63:91])/sqrt(length((t(w3E)[1,])))
l95ci<-rowMeans(t(w3E)[,63:91])-rowSds(t(w3E)[,63:91])/sqrt(length((t(w3E))[1,]))
lims<-na.omit(cbind(u95ci,l95ci))
plot(rowMeans(t(w3E)[,63:91],na.rm=T),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Wyandotte February (2013-2014)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)

u95ci<-rowMeans(t(w3E)[,92:113])+rowSds(t(w3E)[,92:113])/sqrt(length((t(w3E)[1,])))
l95ci<-rowMeans(t(w3E)[,92:113])-rowSds(t(w3E)[,92:113])/sqrt(length((t(w3E))[1,]))
lims<-na.omit(cbind(u95ci,l95ci))
plot(rowMeans(t(w3E)[,92:113],na.rm=T),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Wyandotte March (2013-2014)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)

dev.off()
## plot means over time
##
## W e 26/01/2011 # 5
## W e 20/11/2012 # 10
## W e 28/11/2013 # 2
## W H 28/11/2013 # 2
##

##
dim(t(w1H))
pdf(file = "Wyandotte Cave deeper 2013-2014 month.pdf")
par(mfrow=c(3,2))
par(oma=c(1,4,1,2))
par(mar=c(4,4,1,1))
u95ci<-rowMeans(t(w1H)[,1:2])+rowSds(t(w1H)[,1:2])/sqrt(length((t(w1H)[1,])))
l95ci<-rowMeans(t(w1H)[,1:2])-rowSds(t(w1H)[,1:2])/sqrt(length((t(w1H))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(w1H)[,1:2]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",
     col="black",ylab="# saturated pixels",main="Wyandotte deeper November (2013-2014)",bty="n")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
## plot means over time
##
## W e 26/01/2011 # 5
## W e 20/11/2012 # 10
## W e 28/11/2013 # 2
## W H 28/11/2013 # 2
##
u95ci<-rowMeans(t(w1H)[,3:34])+rowSds(t(w1H)[,3:34])/sqrt(length((t(w1H)[1,])))
l95ci<-rowMeans(t(w1H)[,3:34])-rowSds(t(w1H)[,3:34])/sqrt(length((t(w1H))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(w1H)[,3:34]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",
     col="black",ylab="# saturated pixels",main="Wyandotte deeper December (2013-2014)",bty="n")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)

u95ci<-rowMeans(t(w1H)[,35:66])+rowSds(t(w1H)[,35:66])/sqrt(length((t(w1H)[1,])))
l95ci<-rowMeans(t(w1H)[,35:66])-rowSds(t(w1H)[,35:66])/sqrt(length((t(w1H))[1,]))
lims<-na.omit(cbind(u95ci,l95ci))
plot(rowMeans(t(w1H)[,35:66],na.rm=T),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Wyandotte deeper January (2013-2014)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
## plot means over time
##
## W e 26/01/2011 # 5
## W e 20/11/2012 # 10
## W e 28/11/2013 # 2
## W H 28/11/2013 # 2
##

u95ci<-rowMeans(t(w1H)[,67:95])+rowSds(t(w1H)[,67:95])/sqrt(length((t(w1H)[1,])))
l95ci<-rowMeans(t(w1H)[,67:95])-rowSds(t(w1H)[,67:95])/sqrt(length((t(w1H))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(w1H)[,67:95]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Wyandotte deeper February (2013-2014)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
## plot means over time
##
## W e 26/01/2011 # 5
## W e 20/11/2012 # 10
## W e 28/11/2013 # 2
## W H 28/11/2013 # 2
##

u95ci<-rowMeans(t(w1H)[,96:114])+rowSds(t(w1H)[,96:114])/sqrt(length((t(w1H)[1,])))
l95ci<-rowMeans(t(w1H)[,96:114])-rowSds(t(w1H)[,96:114])/sqrt(length((t(w1H))[1,]))
lims<-na.omit(cbind(u95ci,l95ci))
plot(rowMeans(t(w1H)[,96:114],na.rm=T),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Wyandotte deeper March (2013-2014)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
dev.off()

##################################################
## plot means over time
##
## Gap 18/11/2011 # 12
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
##
id_gap1 <- c(12,22,47,66)
id_gap2 <-c(35,36)
id_gap3 <-c(36,39:57)

##

dim(t(gap1))
pdf(file = "Gap Cave 2011-2012 month.pdf")
par(mfrow=c(2,2))
par(oma=c(1,4,1,2))
par(mar=c(4,4,1,1))
u95ci<-rowMeans(t(gap1)[,1:12])+rowSds(t(gap1)[,1:12])/sqrt(length((t(gap1)[1,])))
l95ci<-rowMeans(t(gap1)[,1:12])-rowSds(t(gap1)[,1:12])/sqrt(length((t(gap1))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(gap1)[,1:12]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",
     col="black",ylab="# saturated pixels",main="Gap Cave November (2011-2012)",bty="n")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)
##
## Gap 18/11/2011 # 12
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
##

u95ci<-rowMeans(t(gap1)[,13:44])+rowSds(t(gap1)[,13:44])/sqrt(length((t(gap1)[1,])))
l95ci<-rowMeans(t(gap1)[,13:44])-rowSds(t(gap1)[,13:44])/sqrt(length((t(gap1))[1,]))
lims<-na.omit(cbind(u95ci,l95ci))
plot(rowMeans(t(gap1)[,13:44],na.rm=T),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Gap Cave December (2011-2012)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
##
## Gap 18/11/2011 # 12
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
##

u95ci<-rowMeans(t(gap1)[,45:76])+rowSds(t(gap1)[,45:76])/sqrt(length((t(gap1)[1,])))
l95ci<-rowMeans(t(gap1)[,45:76])-rowSds(t(gap1)[,45:76])/sqrt(length((t(gap1))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(gap1)[,45:76]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Gap Cave January (2011-2012)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)

u95ci<-rowMeans(t(gap1)[,77:101])+rowSds(t(gap1)[,77:101])/sqrt(length((t(gap1)[1,])))
l95ci<-rowMeans(t(gap1)[,77:101])-rowSds(t(gap1)[,77:101])/sqrt(length((t(gap1))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(gap1)[,77:101]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Gap Cave February (2011-2012)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)

dev.off()
##
## Gap 18/11/2011 # 12
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
##

##
dim(t(gap2))
pdf(file = "Gap Cave 2012-2013 month.pdf")
par(mfrow=c(3,2))
par(oma=c(1,4,1,2))
par(mar=c(4,4,1,1))
u95ci<-rowMeans(t(gap2)[,1:27])+rowSds(t(gap2)[,1:27])/sqrt(length((t(gap2)[1,])))
l95ci<-rowMeans(t(gap2)[,1:27])-rowSds(t(gap2)[,1:27])/sqrt(length((t(gap2))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(gap2)[,1:27]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",
     col="black",ylab="# saturated pixels",main="Gap Cave December (2012-2013)",bty="n")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
##
## Gap 18/11/2011 # 12
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
##

u95ci<-rowMeans(t(gap2)[,27:58])+rowSds(t(gap2)[,27:58])/sqrt(length((t(gap2)[1,])))
l95ci<-rowMeans(t(gap2)[,27:58])-rowSds(t(gap2)[,27:58])/sqrt(length((t(gap2))[1,]))
lims<-na.omit(cbind(u95ci,l95ci))
plot(rowMeans(t(gap2)[,27:58],na.rm=T),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Gap Cave January (2012-2013)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
##
## Gap 18/11/2011 # 12
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
##

u95ci<-rowMeans(t(gap2)[,59:87])+rowSds(t(gap2)[,59:87])/sqrt(length((t(gap2)[1,])))
l95ci<-rowMeans(t(gap2)[,59:87])-rowSds(t(gap2)[,59:87])/sqrt(length((t(gap2))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(gap2)[,59:87]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Gap Cave February (2012-2013)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
##
## Gap 18/11/2011 # 12
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
##

u95ci<-rowMeans(t(gap2)[,87:118])+rowSds(t(gap2)[,87:118])/sqrt(length((t(gap2)[1,])))
l95ci<-rowMeans(t(gap2)[,87:118])-rowSds(t(gap2)[,87:118])/sqrt(length((t(gap2))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(gap2)[,87:118]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Gap Cave March (2012-2013)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
##
## Gap 18/11/2011 # 12
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
##

u95ci<-rowMeans(t(gap2)[,118:145])+rowSds(t(gap2)[,118:145])/sqrt(length((t(gap2)[1,])))
l95ci<-rowMeans(t(gap2)[,118:145])-rowSds(t(gap2)[,118:145])/sqrt(length((t(gap2))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(gap2)[,118:145]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Gap Cave April (2012-2013)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
dev.off()
##
## Gap 18/11/2011 # 12
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
##

##
dim(t(gap3))
pdf(file = "Gap Cave 2012-2013 month.pdf")
par(mfrow=c(2,2))
par(oma=c(1,4,1,2))
par(mar=c(4,4,1,1))
u95ci<-rowMeans(t(gap3)[,1:27])+rowSds(t(gap3)[,1:27])/sqrt(length((t(gap3)[1,])))
l95ci<-rowMeans(t(gap3)[,1:27])-rowSds(t(gap3)[,1:27])/sqrt(length((t(gap3))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(gap3)[,1:27]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",
     col="black",ylab="# saturated pixels",main="Gap Cave December (2013-2014)",bty="n")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
##
## Gap 18/11/2011 # 12
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
##

u95ci<-rowMeans(t(gap3)[,28:59])+rowSds(t(gap3)[,28:59])/sqrt(length((t(gap3)[1,])))
l95ci<-rowMeans(t(gap3)[,28:59])-rowSds(t(gap3)[,28:59])/sqrt(length((t(gap3))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(gap3)[,28:59]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Gap Cave January (2013-2014)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)

u95ci<-rowMeans(t(gap3)[,60:73])+rowSds(t(gap3)[,60:73])/sqrt(length((t(gap3)[1,])))
l95ci<-rowMeans(t(gap3)[,60:73])-rowSds(t(gap3)[,60:73])/sqrt(length((t(gap3))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(gap3)[,60:73]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Gap Cave February (2013-2014)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)

dev.off()
##
## Gap 18/11/2011 # 12
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
##

##
dim(t(gap4))
pdf(file = "Gap Cave 2014-2015 month.pdf")
par(mfrow=c(3,2))
par(oma=c(1,4,1,2))
par(mar=c(4,4,1,1))
u95ci<-rowMeans(t(gap4)[,1:8])+rowSds(t(gap4)[,1:8])/sqrt(length((t(gap4)[1,])))
l95ci<-rowMeans(t(gap4)[,1:8])-rowSds(t(gap4)[,1:8])/sqrt(length((t(gap4))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(gap4)[,1:8]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",
     col="black",ylab="# saturated pixels",main="Gap Cave November (2014-2015)",bty="n")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
##
## Gap 18/11/2011 # 12
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
##

u95ci<-rowMeans(t(gap4)[,9:40])+rowSds(t(gap4)[,9:40])/sqrt(length((t(gap4)[1,])))
l95ci<-rowMeans(t(gap4)[,9:40])-rowSds(t(gap4)[,9:40])/sqrt(length((t(gap4))[1,]))
lims<-na.omit(cbind(u95ci,l95ci))
plot(rowMeans(t(gap4)[,9:40],na.rm=T),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Gap Cave December (2014-2015)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
##
## Gap 18/11/2011 # 12
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
##

u95ci<-rowMeans(t(gap4)[,41:72])+rowSds(t(gap4)[,41:72])/sqrt(length((t(gap4)[1,])))
l95ci<-rowMeans(t(gap4)[,41:72])-rowSds(t(gap4)[,41:72])/sqrt(length((t(gap4))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(gap4)[,41:72]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,100),xlab="hours",bty="n", ###### NOTE NOT MAX #####
     col="black",ylab="# saturated pixels",main="Gap Cave January (2014-2015)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
##
## Gap 18/11/2011 # 12
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
##

u95ci<-rowMeans(t(gap4)[,73:101])+rowSds(t(gap4)[,73:101])/sqrt(length((t(gap4)[1,])))
l95ci<-rowMeans(t(gap4)[,73:101])-rowSds(t(gap4)[,73:101])/sqrt(length((t(gap4))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(gap4)[,73:101]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Gap Cave February (2014-2015)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)
##
## Gap 18/11/2011 # 12
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
##

u95ci<-rowMeans(t(gap4)[,102:132])+rowSds(t(gap4)[,102:132])/sqrt(length((t(gap4)[1,])))
l95ci<-rowMeans(t(gap4)[,102:132])-rowSds(t(gap4)[,102:132])/sqrt(length((t(gap4))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(gap4)[,102:132]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Gap Cave March (2014-2015)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)

u95ci<-rowMeans(t(gap4)[,133:139])+rowSds(t(gap4)[,133:139])/sqrt(length((t(gap4)[1,])))
l95ci<-rowMeans(t(gap4)[,133:139])-rowSds(t(gap4)[,133:139])/sqrt(length((t(gap4))[1,]))
lims<-cbind(u95ci,l95ci)
plot(rowMeans(t(gap4)[,133:139]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
     ylim=c(0,max(lims)),xlab="hours",bty="n",
     col="black",ylab="# saturated pixels",main="Gap Cave April (2014-2015)")
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(side=1, at = seq(from =0, to= 288, by = 288/24),
     labels = c(0:24), tick = TRUE,
     lty=1)#,outer=T)


dev.off()

##########################################
##
## TO HERE ###############################
##
#########################################


# 
# library("CircStats")
# # Compare the edf's of two simulated sets of data.
# data1 <- rvm(10, 0, 3)
# data2 <- rvm(10, 0, 1)
# plot.edf(data1, xlab="Data", ylab="EDF", main="Plots of Two EDF's")
# par(new=TRUE)
# plot.edf(data2, axes=FALSE, xlab="", ylab="", lty=2)
# # Generate 100 observations from a von Mises distribution.
# # with mean direction 0 and concentration 3.
# data.vm <- rvm(100, 0, 3)
# # Plot data set. All points do not fit on plot.
# circ.plot(data.vm, stack=TRUE, bins=150)
# # Shrink the plot so that all points fit.
# circ.plot(data.vm, stack=TRUE, bins=150, shrink=1.5)
# 
# #plot(rowMeans(t(w1E)[,1:10]),type="l",ylim=c(0,1200),col="lightblue",ylab="# saturated pixels")
# #lines(rowMeans(t(w1E)[,11:20]),col="blue")
# #lines(rowMeans(t(w1E)[,21:30]),col="darkblue")
# #lines(rowMeans(t(w1E)[,31:40]),col="lightgreen")
# #lines(rowMeans(t(w1E)[,41:50]),col="green")
# #lines(rowMeans(t(w1E)[,51:60]),col="darkgreen")
# #lines(rowMeans(t(w1E)[,61:70]),col="yellow")
# #lines(rowMeans(t(w1E)[,71:80]),col="orange")
# #lines(rowMeans(t(w1E)[,1:80]),col="red")
# 
# #plot(rowMeans(t(w1E)[,1:30]),type="l",ylim=c(0,1200),col="lightblue",ylab="# saturated pixels")
# #lines(rowMeans(t(w1E)[,31:60]),col="blue")
# #lines(rowMeans(t(w1E)[,61:80]),col="darkblue")
# 
# par(mfrow=c(1,1))
# #windows()
# plot(rowMeans(t(gap1)[,1:45]),type="l",bty="n",
#      ylim=c(0,1200*2.8),xlim=c(0,1200),
#      xlab="",ylab="",xaxt="n",yaxt="n")
# lines(rowMeans(t(gap1)[,46:101])+1200*1)
# ## gap2
# lines(x=1:288+300,rowMeans(t(gap2)[,1:29]))#+1200*1)
# lines(x=1:288+300,rowMeans(t(gap2)[,30:89]+1200*1))#+1200*1)
# lines(x=1:288+300,rowMeans(t(gap2)[,90:145]+1200*2))#+1200*1)
# ## w1E
# lines(x=1:320+600,rowMeans(t(w1E)[,1:36]+1200*1))#+1200*1)
# lines(x=1:320+600,rowMeans(t(w1E)[,37:80]+1200*2))#+1200*1)
# ## w2E
# lines(x=1:288+920,rowMeans(t(w2E)[,1:41]))#+1200*1)
# lines(x=1:288+920,rowMeans(t(w2E)[,42:101]+1200*1))#+1200*1)
# lines(x=1:288+920,rowMeans(t(w2E)[,102:130]+1200*2))#+1200*1)
# mtext(2,text="Mean no. of saturated pixels",line=2.5,cex=0.8)
# mtext("(a)",side=3,line=-1.5, 
#       at=par("usr")[1]+0.05*diff(par("usr")[1:2]),
#       cex=1.2)
# mtext("(b)",side=3,line=-1.5, 
#       at=par("usr")[1]+0.3*diff(par("usr")[1:2]),
#       cex=1.2)
# mtext("(c)",side=3,line=-1.5, 
#       at=par("usr")[1]+0.52*diff(par("usr")[1:2]),
#       cex=1.2)
# mtext("(d)",side=3,line=-1.5, 
#       at=par("usr")[1]+0.75*diff(par("usr")[1:2]),
#       cex=1.2)
# mtext("Nov-Dec",side=2,line=1, 
#       at=par("usr")[1]+0.15*diff(par("usr")[1:2]),
#       cex=0.8)
# mtext("Jan-Feb",side=2,line=1, 
#       at=par("usr")[1]+1.15*diff(par("usr")[1:2]),
#       cex=0.8)
# mtext("Mar-Apr",side=2,line=1, 
#       at=par("usr")[1]+2.15*diff(par("usr")[1:2]),
#       cex=0.8)
# # note 164 days of data
# #plot(x=0:2171,type="n", axes=F, xlab="", ylab="",ylim=c(0,25000),main="Wyandotte Cave")
# #points(x=545:1202,y=w1E[1:658,]+17500,type="l",col="blue")
# #points(x=43:2171,y=w2E[1:2129,]+10000,type="l",col="purple")
# #points(x=631:1976,y=w3[1:1346,],type="l",col="red")
# #mtext(2,text="No. of saturated pixels",line=2,cex=0.8)
# #mtext(2,text="2012-2013       2012-2013     2011-2012",line=1,cex=0.8)
# #mtext(2,text="Camera 1       Camera 2",at=c(8000),cex=0.8)
# #axis(side=1, at = seq(from =172, to= 2171, by = 399.8),
# #     labels = c("Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
# #     lty=2)
# #
# ##################
# library(reshape)
# 
# #head(w1E)
# #dim(w1E)
# #time<-rep(1:14,(658/14))
# #w1Ecirc<-cbind(w1E,time)
# #tday<-factor(time)
# #res.w1E<-matrix(NA,nrow=14,ncol=(length(w1Ecirc[,1])/14))
# 
# #for (i in 1:length(w1Ecirc[,1])){
#  # res.w1E[i]<-w1Ecirc[i,1]
# #}
# #plot(x=w1Ecirc[,2],y=log(w1Ecirc[,1]),pch=".")
# #points(log(rowMeans(res.w1E)),col="red",pch=16)
# #
# #plot(rowMeans(res.w1E),type="l")
# #
# #plot(x=w1Ecirc[,2],y=(w1Ecirc[,1]),pch=20,cex=0.5)
# #points((rowMeans(res.w1E)),col="red",pch=16)
# 
# #plot(x=w1Ecirc[,2],y=(w1Ecirc[,1]),pch=16,cex=0.5,col="grey45")
# #lines((rowMeans(res.w1E)),col="red")
# 
# #res.w1E.sd<-apply(res.w1E[,1:14],1,sd)
# #res.w1E.mean<-rowMeans(res.w1E)
# #res.w1E.all<-cbind((res.w1E.mean-res.w1E.sd),res.w1E.mean,(res.w1E.mean+res.w1E.sd))
# #plot(res.w1E.all[,2],ylim=c(min(res.w1E.all[,1]),max(res.w1E.all[,3])),type="l")
# #lines(res.w1E.all[,1])
# #lines(res.w1E.all[,3])
# 
# ## Gap year 1
# 
# head(gap1)
# dim(gap1)
# time<-rep(1:14,(2114/14)) ## NB could not use last data points... see length difference
# gap1circ<-cbind(gap1[1:2114,],time)
# tday<-factor(time)
# res.gap1<-matrix(NA,nrow=14,ncol=(length(gap1circ[,1])/14))
# 
# for (i in 1:length(gap1circ[,1])){
#   res.gap1[i]<-gap1circ[i,1]
# }
# 
# plot(x=gap1circ[,2],y=log(gap1circ[,1]),pch=".")
# points(log(rowMeans(res.gap1)),col="red",pch=16)
# 
# plot(rowMeans(res.gap1),type="l")
# 
# plot(x=gap1circ[,2],y=(gap1circ[,1]),pch=20,cex=0.5)
# points((rowMeans(res.gap1)),col="red",pch=16)
# 
# ############
# 
# ## Gap year 2
# 
# head(gap2)
# dim(gap2)
# time<-rep(1:14,(1456/14))
# gap2circ<-cbind(gap2[1:1456,],time)
# tday<-factor(time)
# res.gap2<-matrix(NA,nrow=14,ncol=(length(gap2circ[,1])/14))
# 
# for (i in 1:length(gap2circ[,1])){
#   res.gap2[i]<-gap2circ[i,1]
# }
# plot(x=gap2circ[,2],y=log(gap2circ[,1]),pch=".")
# points(log(rowMeans(res.gap2)),col="red",pch=16)
# 
# plot(rowMeans(res.gap2),type="l")
# 
# plot(x=gap2circ[,2],y=(gap2circ[,1]),pch=20,cex=0.5)
# points((rowMeans(res.gap2)),col="red",pch=16)
# 
# plot(x=gap2circ[,2],y=(gap2circ[,1]),pch=16,cex=0.5,col="grey45",ylim=c(0,5000))
# lines((rowMeans(res.gap2)),col="red")
# 
# ############
# # w2E
# 
# head(w2E)
# dim(w2E)
# time<-rep(1:14,(2128/14))
# w2Ecirc<-cbind(w2E[1:2128,],time)
# tday<-factor(time)
# res.w2E<-matrix(NA,nrow=14,ncol=(length(w2Ecirc[,1])/14))
# 
# for (i in 1:length(w2Ecirc[,1])){
#   res.w2E[i]<-w2Ecirc[i,1]
# }
# plot(x=w2Ecirc[,2],y=log(w2Ecirc[,1]),pch=".")
# points(log(rowMeans(res.w2E)),col="red",pch=16)
# 
# plot(rowMeans(res.w2E),type="l")
# 
# plot(x=w2Ecirc[,2],y=(w2Ecirc[,1]),pch=20,cex=0.5)
# points((rowMeans(res.w2E)),col="red",pch=16)
# 
# ##########################
# 
# head(w3)
# dim(w3)
# time<-rep(1:14,(1344/14))
# w3circ<-cbind(w3[1:1344,],time)
# tday<-factor(time)
# res.w3<-matrix(NA,nrow=14,ncol=(length(w3circ[,1])/14))
# 
# for (i in 1:length(w3circ[,1])){
#   res.w3[i]<-w3circ[i,1]
# }
# 
# plot(x=w3circ[,2],y=log(w3circ[,1]),pch=".")
# points(log(rowMeans(res.w3)),col="red",pch=16)
# 
# plot(rowMeans(res.w3),type="l")
# 
# plot(x=w3circ[,2],y=(w3circ[,1]),pch=20,cex=0.5)
# points((rowMeans(res.w3)),col="red",pch=16)
# 
# ############3
# 
# plot((rowMeans(res.w1E)-mean(rowMeans(res.w1E))),
#      type="l",lty=1,ylab="",col="blue",axes=F,xlab="time")
# abline(h=0,lty=3,col="grey75")
# lines((rowMeans(res.w2E)-mean(rowMeans(res.w2E))),lty=1,col="purple")
# lines((rowMeans(res.w3)-mean(rowMeans(res.w3))),lty=1,col="red")
# lines((rowMeans(res.gap1)-mean(rowMeans(res.gap1))),lty=2,col="orange")
# lines((rowMeans(res.gap2)-mean(rowMeans(res.gap2))),lty=2,col="green")
# mtext(2,text="Deviation of mean no. of saturated pixels",line=3,cex=0.8)
# lab<-as.character(1:23)
# axis(side=1, at = seq(from =1, to=14,by=14/24),
#      labels = lab, tick = TRUE,
#      lty=2)
# axis(side=2, tick = TRUE,
#      lty=2)
# legend("topright",c("Wyandotte Yr1","Wyandotte Yr2 #1","Wyandotte Yr2 #2",
#                     "Gap Yr1", "Gap Yr2"),
#        col=c("blue","purple","red","orange","green"), lty=c(1,1,1,2,2),bty="n",
#        cex=0.8)
# 
# ############
# seq(1,152,21)
# ## dates 11.20.12-5.5.13
# Novw2E<-cbind(res.w2E[,1:10])
# Decw2E<-cbind(res.w2E[,11:42])
# Janw2E<-cbind(res.w2E[,43:74])
# Febw2E<-cbind(res.w2E[,75:106])
# Marw2E<-cbind(res.w2E[,107:138])
# Aprw2E<-cbind(res.w2E[,139:152])
# #Mayw2E<-cbind(res.w2E[,:])
# 
# plot(rowMeans(Novw2E)-mean(Novw2E)+1800,type="l",ylim=c(-1800,2000),col="grey1",
#      axes=F,xlab="time",ylab="")
# abline(h=1800,col="red",lty=2)
# lines(rowMeans(Decw2E)-mean(Decw2E)+1200,type="l",col="grey20")
# abline(h=1200,col="red",lty=2)
# lines(rowMeans(Janw2E)-mean(Janw2E)+600,type="l",col="grey30")
# abline(h=600,col="red",lty=2)
# lines(rowMeans(Febw2E)-mean(Febw2E),type="l",col="grey40")
# abline(h=0,col="red",lty=2)
# lines(rowMeans(Marw2E)-mean(Marw2E)-600,type="l",col="grey30")
# abline(h=-600,col="red",lty=2)
# lines(rowMeans(Aprw2E)-mean(Aprw2E)-1200,type="l",col="grey20")
# abline(h=-1200,col="red",lty=2)
# lab<-as.character(1:23)
# axis(side=1, at = seq(from =1, to=14,by=14/24),
#      labels = lab, tick = TRUE,
#      lty=2)
# ##########################333
# plot(x=w1Ecirc[,2],y=(log(w1Ecirc[,1])-log(rowMeans(res.w1E))),
#      pch=".",ylab="",col="blue",axes=F,xlab="time")
# points((log(rowMeans(res.w1E))-mean(log(rowMeans(res.w1E)))),
#        type="l",lty=1,ylab="",col="blue")
# abline(h=0,lty=3,col="grey10")
# points(x=w2Ecirc[,2]+0.15,y=(log(w2Ecirc[,1])-log(rowMeans(res.w2E))),pch=".",col="purple")
# points((log(rowMeans(res.w2E))-mean(log(rowMeans(res.w2E)))),
#        type="l",lty=1,ylab="",col="purple")
# points(x=w3circ[,2]+0.3,y=(log(w3circ[,1])-log(rowMeans(res.w3))),pch=".",col="red")
# points((log(rowMeans(res.w3))-mean(log(rowMeans(res.w3)))),
#        type="l",lty=1,ylab="",col="red")
# points(x=gap1circ[,2]+0.45,y=(log(gap1circ[,1])-log(rowMeans(res.gap1))),pch=".",col="orange")
# points((log(rowMeans(res.gap1))-mean(log(rowMeans(res.gap1)))),
#        type="l",lty=1,ylab="",col="orange")
# points(x=w3circ[,2]+0.3,y=(log(w3circ[,1])-log(rowMeans(res.w3))),pch=".",col="green")
# points((log(rowMeans(res.gap2))-mean(log(rowMeans(res.gap2)))),
#        type="l",lty=1,ylab="",col="green")
# axis(side=1, at = seq(from =1, to=14,by=14/24),
#      labels = lab, tick = TRUE,
#      lty=2)
# axis(side=2, tick = TRUE,
#      lty=2)
# legend("bottomright",c("Wyandotte Yr1","Wyandotte Yr2 #1","Wyandotte Yr2 #2",
#                     "Gap Yr1", "Gap Yr2"),
#        col=c("blue","purple","red","orange","green"),pch=16,bty="n",
#        cex=0.8)
# mtext(2,text="Deviation from mean no. of saturated pixels (Log scale)",line=3,cex=0.8)
# 
# 
# ############3
# 
# ## TO HERE
# 
# ##########################################################
# rm(list=ls())
# 
# library(dplR)
# library(xts)
# 
# setwd("~/Cambridge/CSU 2013/Video/paper_methods/Combined CSVs")
# #### load data
# 
# ## try just front loading with the different # of time points
# gap1 = read.csv("Gap_11.29.12-4.29.13.csv",na.strings=c("NA"," - "),header=F)
# dim(gap1)
# 
# gap2 = read.csv("Gap_11.17.11-3.2.12.csv",na.strings=c("NA"," - "),header=F)
# dim(gap2)
# 
# w1 = read.csv("Wyandotte_1.25.12-4.17.12.csv",na.strings=c("NA"," - "),header=F)
# dim(w1)
# 
# w2 = read.csv("Wyandotte_11.20.12-5.5.13.csv",na.strings=c("NA"," - "),header=F)
# dim(w2)
# 
# w3 = read.csv("Wyandotte_1.2.13_1-95.csv",na.strings=c("NA"," - "),header=F)
# dim(w3)
# 
# ## note 166 days of data
# layout(1:1)
# plot(x=0:2294,type="n", axes=F, xlab="", ylab="",ylim=c(0,6000),main="Gap Cave")
# points(x=169:2294,y=gap1[1:2126,]+4500,type="l",col="orange")
# points(x=1:1464,y=gap2[1:1464,],type="l", col="green")
# mtext(2,text="No. of saturated pixels",line=2,cex=0.8)
# mtext(2,text="2012-2013         2011-2012",line=1,cex=0.8)
# axis(side=1, at = seq(from =179, to= 2280, by = 420.2),
#      labels = c("Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
#      lty=2)
# 
# # note 164 days of data
# plot(x=0:2171,type="n", axes=F, xlab="", ylab="",ylim=c(0,25000),main="Wyandotte Cave")
# points(x=545:1202,y=w1[1:658,]+17500,type="l",col="blue")
# points(x=43:2171,y=w2[1:2129,]+10000,type="l",col="purple")
# points(x=631:1976,y=w3[1:1346,],type="l",col="red")
# mtext(2,text="No. of saturated pixels",line=2,cex=0.8)
# mtext(2,text="2012-2013       2012-2013     2011-2012",line=1,cex=0.8)
# mtext(2,text="Camera 1       Camera 2",at=c(8000),cex=0.8)
# axis(side=1, at = seq(from =172, to= 2171, by = 399.8),
#      labels = c("Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
#      lty=2)
# 
# ##################
# ## Gap year 1
# 
# layout(1:2)
# par(mfrow=c(2,1))
# gap1<-ts(gap1)
# plot(gap1,type="l", ylab="# saturated pixels",main="Gap Cave 2011-2012")
# gap1.f1 <- filter(gap1,filter=rep(1/5,5))
# plot(gap1.f1,col="red",lwd=2,lty=1,ylab="# saturated pixels")
# 
# layout(1:2)
# plot(gap1, ylab="# saturated pixels",type="l",main="Gap Cave 2011-2012")
# acf(gap1, lag.max = length(gap1),plot=T,main="")
# 
# layout(1:1)
# acf(gap1, lag.max = length(gap1),plot=T,main="Gap Cave 2011-2012")
# # samples/day
# # days 151 
# sg1<-dim(gap1)/151
# abline(v=sg1[1]*7,col="red",lty=2)
# 
# wtgap1<-morlet(gap1, x1 = seq_along(gap1), p2 = NULL, dj = 0.25, siglvl = 0.95)
# # windows() # if plot too large
# wavelet.plot(wtgap1,crn.lab="Pixel",x.lab="Time")
# 
# ############
# ## truncate data
# 
# gap1s<-gap1[900:2126]
# 
# layout(1:2)
# par(mfrow=c(2,1))
# plot(gap1s,type="l", ylab="# saturated pixels",main="Gap Cave 2011-2012")
# gap1s.f1 <- filter(gap1s,filter=rep(1/5,5))
# plot(gap1s.f1,col="red",lwd=2,lty=1,ylab="# saturated pixels")
# 
# layout(1:2)
# plot(gap1s, ylab="# saturated pixels",type="l",main="Gap Cave 2011-2012")
# acf(gap1s, lag.max = length(gap1s),plot=T,main="")
# 
# layout(1:1)
# acf(gap1s, lag.max = length(gap1s),plot=T,main="Gap Cave 2011-2012")
# abline(v=sg1[1]*7,col="red",lty=2)
# 
# wtgap1s<-morlet(gap1s, x1 = seq_along(gap1s), p2 = NULL, dj = 0.25, siglvl = 0.95)
# # windows() # if plot too large
# wavelet.plot(wtgap1s,crn.lab="Pixel",x.lab="Time")
# 
# gap1s.f1<-na.omit(gap1s.f1)
# wtgap1s.f1<-morlet(gap1s.f1, x1 = seq_along(gap1s.f1), p2 = NULL, dj = 0.25, siglvl = 0.95)
# # windows() # if plot too large
# wavelet.plot(wtgap1s.f1,crn.lab="Pixel",x.lab="Time")
# 
# ##################
# ## Gap year 2
# 
# layout(1:2)
# par(mfrow=c(2,1))
# gap2<-ts(gap2)
# plot(gap2,type="l", ylab="# saturated pixels",main="Gap Cave 2012-2013")
# gap2.f1 <- filter(gap2,filter=rep(1/5,5))
# plot(gap2.f1,col="red",lwd=2,lty=1,ylab="# saturated pixels")
# 
# layout(1:2)
# plot(gap2, ylab="# saturated pixels",type="l",main="Gap Cave 2012-2013")
# acf(gap2, lag.max = length(gap2),plot=T,main="")
# 
# layout(1:1)
# acf(gap2, lag.max = length(gap2),plot=T,main="Gap Cave 2012-2013")
# # samples/day
# # days 106 
# sg2<-dim(gap2)/106
# abline(v=sg2[1]*7,col="red",lty=2)
# 
# wtgap2<-morlet(gap2, x1 = seq_along(gap2), p2 = NULL, dj = 0.25, siglvl = 0.95)
# # windows() # if plot too large
# wavelet.plot(wtgap2,crn.lab="Pixel",x.lab="Time")
# 
# ############
# ##
# ## Wyandotte
# ## year 1
# ##################
# 
# layout(1:2)
# par(mfrow=c(2,1))
# w1<-ts(w1)
# plot(w1,type="l", ylab="# saturated pixels",main="Wyandotte Cave 2011-2012")
# w1.f1 <- filter(w1,filter=rep(1/5,5))
# plot(w1.f1,col="red",lwd=2,lty=1,ylab="# saturated pixels")
# 
# layout(1:2)
# plot(w1, ylab="# saturated pixels",type="l",main="Wyandotte Cave 2011-2012")
# acf(w1, lag.max = length(w1),plot=T,main="")
# 
# layout(1:1)
# acf(w1, lag.max = length(w1),plot=T,main="Wyandotte Cave 2011-2012")
# # samples/day 
# # days 83
# sw1<-dim(w1)/83
# abline(v=sw1[1]*7,col="red",lty=2)
# 
# wtw1<-morlet(w1, x1 = seq_along(w1), p2 = NULL, dj = 0.25, siglvl = 0.95)
# # windows() # if plot too large
# wavelet.plot(wtw1,crn.lab="Pixel",x.lab="Time")
# 
# ############
# ## Wyandotte year 2 #1
# 
# layout(1:2)
# par(mfrow=c(2,1))
# w2<-ts(w2)
# plot(w2,type="l", ylab="# saturated pixels",main="Wyandotte Cave 2012-2013 Camera 1")
# w2.f1 <- filter(w2,filter=rep(1/5,5))
# plot(w2.f1,col="red",lwd=2,lty=1,ylab="# saturated pixels")
# 
# layout(1:2)
# plot(w2, ylab="# saturated pixels",type="l",main="Wyandotte Cave 2012-2013 Camera 1")
# acf(w2, lag.max = length(w2),plot=T,main="")
# 
# layout(1:1)
# acf(w2, lag.max = length(w2),plot=T,main="Wyandotte Cave 2012-2013 Camera 1")
# # samples/day 
# # days 166
# sw2<-dim(w2)/166
# abline(v=sw2[1]*7,col="red",lty=2)
# 
# wtw2<-morlet(w2, x1 = seq_along(w2), p2 = NULL, dj = 0.25, siglvl = 0.95)
# # windows() # if plot too large
# wavelet.plot(wtw2,crn.lab="Pixel",x.lab="Time")
# 
# ############
# ## truncate data
# 
# w2s<-w2[750:2129]
# 
# layout(1:2)
# par(mfrow=c(2,1))
# plot(w2s,type="l", ylab="# saturated pixels",main="Wyandotte Cave 2012-2013 Camera 1")
# w2s.f1 <- filter(w2s,filter=rep(1/5,5))
# plot(w2s.f1,col="red",lwd=2,lty=1,ylab="# saturated pixels")
# 
# layout(1:2)
# plot(w2s, ylab="# saturated pixels",type="l",main="Wyandotte Cave 2012-2013 Camera 1")
# acf(w2s, lag.max = length(w2s),plot=T,main="")
# 
# layout(1:1)
# acf(w2s, lag.max = length(w2s),plot=T,main="Wyandotte Cave 2012-2013 Camera 1")
# abline(v=sw2[1]*7,col="red",lty=2)
# 
# wtw2s<-morlet(w2s, x1 = seq_along(w2s), p2 = NULL, dj = 0.25, siglvl = 0.95)
# # windows() # if plot too large
# wavelet.plot(wtw2s,crn.lab="Pixel",x.lab="Time")
# 
# w2s.f1<-na.omit(w2s.f1)
# wtw2s.f1<-morlet(w2s.f1, x1 = seq_along(w2s.f1), p2 = NULL, dj = 0.25, siglvl = 0.95)
# # windows() # if plot too large
# wavelet.plot(wtw2s.f1,crn.lab="Pixel",x.lab="Time")
# 
# ############
# ## Wyandotte year 2 #2
# 
# layout(1:2)
# par(mfrow=c(2,1))
# w3<-ts(w3)
# plot(w3,type="l", ylab="# saturated pixels",main="Wyandotte Cave 2012-2013 Camera 2")
# w3.f1 <- filter(w3,filter=rep(1/5,5))
# plot(w3.f1,col="red",lwd=2,lty=1,ylab="# saturated pixels")
# 
# layout(1:2)
# plot(w3, ylab="# saturated pixels",type="l",main="Wyandotte Cave 2012-2013 Camera 2")
# acf(w3, lag.max = length(w3),plot=T,main="")
# 
# layout(1:1)
# acf(w3, lag.max = length(w3),plot=T,main="Wyandotte Cave 2012-2013 Camera 2")
# # samples/day
# # days 100
# sw3<-dim(w3)/100
# abline(v=sw3[1]*7,col="red",lty=2)
# 
# wtw3<-morlet(w3, x1 = seq_along(w3), p2 = NULL, dj = 0.25, siglvl = 0.95)
# # windows() # if plot too large
# wavelet.plot(wtw3,crn.lab="Pixel",x.lab="Time")
# 
# ############
# ## truncate data
# 
# w3s<-w3[300:1346]
# 
# layout(1:2)
# par(mfrow=c(2,1))
# plot(w3s,type="l", ylab="# saturated pixels",main="Wyandotte Cave 2012-2013 Camera 2")
# w3s.f1 <- filter(w3s,filter=rep(1/5,5))
# plot(w3s.f1,col="red",lwd=2,lty=1,ylab="# saturated pixels")
# 
# layout(1:2)
# plot(w3s, ylab="# saturated pixels",type="l",main="Wyandotte Cave 2012-2013 Camera 2")
# acf(w3s, lag.max = length(w3s),plot=T,main="")
# 
# layout(1:1)
# acf(w3s, lag.max = length(w3s),plot=T,main="Wyandotte Cave 2012-2013 Camera 2")
# abline(v=sw3[1]*7,col="red",lty=2)
# 
# wtw3s<-morlet(w3s, x1 = seq_along(w3s), p2 = NULL, dj = 0.25, siglvl = 0.95)
# # windows() # if plot too large
# wavelet.plot(wtw3s,crn.lab="Pixel",x.lab="Time")
# 
# w3s.f1<-na.omit(w3s.f1)
# wtw3s.f1<-morlet(w3s.f1, x1 = seq_along(w3s.f1), p2 = NULL, dj = 0.25, siglvl = 0.95)
# # windows() # if plot too large
# wavelet.plot(wtw3s.f1,crn.lab="Pixel",x.lab="Time")
# 
# ##################
# wWin = read.csv("allW1window124th.csv",na.strings=c("NA"," - "),header=F)
# dim(wWin)
# wWin<-t(wWin)
# dim(wWin)
# 
# layout(1:2)
# par(mfrow=c(2,1))
# wWin<-ts(wWin)
# plot(wWin[1:1345,],type="l", ylab="# saturated pixels",main="Wyandotte Cave 2012-2013")
# wWin.f1 <- filter(wWin,filter=rep(1/5,5))
# plot(wWin.f1,col="red",lwd=2,lty=1,ylab="# saturated pixels")
# 
# layout(1:2)
# plot(wWin, ylab="# saturated pixels",type="l",main="Wyandotte Cave 2012-2013")
# acf(wWin, lag.max = length(wWin),plot=T,main="")
# 
# layout(1:1)
# acf(wWin, lag.max = length(wWin),plot=T,main="Wyandotte Cave 2012-2013")
# abline(v=sw2[1]*7,col="red",lty=2)
# 
# wtwWin<-morlet(wWin, x1 = seq_along(wWin), p2 = NULL, dj = 0.25, siglvl = 0.95)
# # windows() # if plot too large
# wavelet.plot(wtwWin,crn.lab="Pixel",x.lab="Time")

g1<-as.vector(t(gap1))
g2<-as.vector(t(gap2))
g3<-as.vector(t(gap3))
g4<-as.vector(t(gap4))
w1<-as.vector(t(w1E))
w2<-as.vector(t(w2E))
w3<-as.vector(t(w3E))
w1hole<-as.vector(t(w1H))
##
## Gap 04/12/2012 # 27
## Gap 04/02/2013 # 24
## Gap 22/11/2014 # 8
## W e 26/01/2011 # 5
## W e 20/11/2012 # 10
## W e 28/11/2013 # 2
## W H 28/11/2013 # 2
##
lims<-max(length(g1),length(g2),length(g3),length(g4),length(w1),length(w2),length(w3),length(w1hole))

pdf(file = "Gap Cave 2011-2012_data.pdf")
plot(g1,xlim=c(-18*288,(6*30*288)-18*288),type='l',ylab="No. saturated pixels",xlab='time', xaxt='n',main="Gap cave 2011-2012")
## Gap 18/11/2011 # 12
axis(side=1, at = seq(from =-18*288, to= 8640*6, by = 8640),
     labels = c("Nov","Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
     lty=2)
dev.off()

pdf(file = "Gap Cave 2012-2013_data.pdf")
plot(g2,xlim=c(-((4+31)*288),(6*30*288)-((4+31)*288)),type='l',ylab="No. saturated pixels",xlab='time', xaxt='n',main="Gap cave 2012-2013")
## Gap 04/12/2012 # 27
axis(side=1, at = seq(from =-((4+31)*288), to= 8640*5, by = 8640),
     labels = c("Nov","Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
     lty=2)
dev.off()

pdf(file = "Gap Cave 2012-2013_data_pub.pdf")
plot(g2,xlim=c(-((4+31)*288),(6*30*288)-((4+31)*288)),ylim=c(0,5000),type='l',ylab="No. saturated pixels",xlab='time', xaxt='n',main="Gap cave 2012-2013")
## Gap 04/12/2012 # 27
axis(side=1, at = seq(from =-((4+31)*288), to= 8640*5, by = 8640),
     labels = c("Nov","Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
     lty=2)
dev.off()


pdf(file = "Gap Cave 2013-2014_data.pdf")
plot(g3,xlim=c(-((4+31+31+31)*288),(6*30*288)-(4+31+31+31)*288),type='l',ylab="No. saturated pixels",xlab='time', xaxt='n',main="Gap cave 2013-2014")
## Gap 04/02/2013 # 24
axis(side=1, at = seq(from =-(4+31+31+31)*288, to= 8640*3, by = 8640),
     labels = c("Nov","Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
     lty=2)
dev.off()

pdf(file = "Gap Cave 2014-2015_data.pdf")
plot(g4,xlim=c(-((22)*288),(6*30*288)-(22)*288),type='l',ylab="No. saturated pixels",xlab='time', xaxt='n',main="Gap cave 2014-2015")
## Gap 22/11/2014 # 8
axis(side=1, at = seq(from =-22*288, to= 8640*6, by = 8640),
     labels = c("Nov","Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
     lty=2)
dev.off()

pdf(file = "Wyandotte cave entrance 2011-2012_data.pdf")
plot(w1,xlim=c(-((31+31+26)*288),(6*30*288)-(31+31+26)*288),type='l',ylab="No. saturated pixels",xlab='time', xaxt='n',main="Wyandotte cave entrance 2011-2012")
## W e 26/01/2011 # 5
axis(side=1, at = seq(from =-(31+31+26)*288, to= 8640*4, by = 8640),
     labels = c("Nov","Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
     lty=2)
dev.off()

pdf(file = "Wyandotte cave entrance 2012-2013_data.pdf")
plot(w2,xlim=c(-((20)*288),(6*30*288)-(20)*288),type='l',ylab="No. saturated pixels",xlab='time', xaxt='n',main="Wyandotte cave entrance 2012-2013")
## W e 20/11/2012 # 10
axis(side=1, at = seq(from =-20*288, to= 8640*6, by = 8640),
     labels = c("Nov","Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
     lty=2)
dev.off()

pdf(file = "Wyandotte cave entrance 2013-2014_data.pdf")
plot(w3,xlim=c(-((28)*288),(6*30*288)-(28)*288),type='l',ylab="No. saturated pixels",xlab='time', xaxt='n',main="Wyandotte cave entrance 2013-2014")
## W e 28/11/2013 # 2
axis(side=1, at = seq(from =-28*288, to= 8640*6, by = 8640),
     labels = c("Nov","Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
     lty=2)
dev.off()

pdf(file = "Wyandotte cave deeper 2013-2014_data.pdf")
plot(w1hole,xlim=c(-((28)*288),(6*30*288)-(28)*288),type='l',ylab="No. saturated pixels",xlab='time', xaxt='n',main="Wyandotte cave deeper 2013-2014")
## W H 28/11/2013 # 2
axis(side=1, at = seq(from =-28*288, to= 8640*6, by = 8640),
     labels = c("Nov","Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
     lty=2)
dev.off()

library(lubridate)
library(ggplot2)   # use at least 0.9.3 for theme_minimal()

# ## generate random data in POSIX date-time format
# set.seed(44)
# N=500
# events <- as.POSIXct("2011-01-01", tz="GMT") +
#   days(floor(365*runif(N))) +
#   hours(floor(24*rnorm(N))) +  # using rnorm here
#   minutes(floor(60*runif(N))) +
#   seconds(floor(60*runif(N)))
# 
# 
# # extract hour with lubridate function
# hour_of_event <- hour(events)
# # make a dataframe
# eventdata <- data.frame(datetime = events, eventhour = hour_of_event)
# # determine if event is in business hours
# eventdata$Workday <- eventdata$eventhour %in% seq(9, 17)
# 
# ggplot(eventdata, aes(x = eventhour, fill = Workday)) + geom_histogram(breaks = seq(0,
#                                                                                     24), width = 2, colour = "grey") + coord_polar(start = 0) + theme_minimal() +
#   scale_fill_brewer() + ylab("Count") + ggtitle("Events by Time of day") +
#   scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,
# 24))
# ##

hrs11<-rep(seq(from=1,to=24,by=2),12)
hrs12<-rep(seq(from=0,to=23,by=2),12)
hrs<-c(hrs11,hrs12)

tgp1<-as.data.frame(t(gap1))
tgp1$rs<-rowSums(tgp1)
tgp1$hrs<-sort(hrs)
res.dat.gp1<-aggregate(tgp1$rs, by=list(tgp1$hrs), FUN=sum)[2]
res.dat.gp1$hr<-0:23
dat.p.gp1<-as.data.frame(rep(0:23,times=c(res.dat.gp1$x)))
colnames(dat.p.gp1)<-'x'

pdf(file = "Gap Cave 2011-2012 rose.pdf")
ggplot(dat.p.gp1, aes(x = x)) + geom_histogram(breaks = seq(0,24), width = 2, colour = "grey") + coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Pixel count") + ggtitle("Gap Cave 2011-2012 \n Activity by hour") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))
dev.off()

##

tgp2<-as.data.frame(t(gap2))
tgp2$rs<-rowSums(tgp2)
tgp2$hrs<-sort(hrs)
res.dat.gp2<-aggregate(tgp2$rs, by=list(tgp2$hrs), FUN=sum)[2]
res.dat.gp2$hr<-0:23
dat.p.gp2<-as.data.frame(rep(0:23,times=c(res.dat.gp2$x)))
colnames(dat.p.gp2)<-'x'


pdf(file = "Gap Cave 2012-2013 rose.pdf")
ggplot(dat.p.gp2, aes(x = x)) + geom_histogram(breaks = seq(0,24), width = 2, colour = "grey") + coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Pixel count") + ggtitle("Gap Cave 2012-2013 \n Activity by hour") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))
dev.off()
##

tgp3<-as.data.frame(t(gap3))
tgp3$rs<-rowSums(tgp3)
tgp3$hrs<-sort(hrs)
res.dat.gp3<-aggregate(tgp3$rs, by=list(tgp3$hrs), FUN=sum)[2]
res.dat.gp3$hr<-0:23
dat.p.gp3<-as.data.frame(rep(0:23,times=c(res.dat.gp3$x)))
colnames(dat.p.gp3)<-'x'

pdf(file = "Gap Cave 2013-2014 rose.pdf")
ggplot(dat.p.gp3, aes(x = x)) + geom_histogram(breaks = seq(0,24), width = 2, colour = "grey") + coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Pixel count") + ggtitle("Gap Cave 2013-2014 \n Activity by hour") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))
dev.off()

##

tgp4<-as.data.frame(t(gap4))
tgp4$rs<-rowSums(tgp4)
tgp4$hrs<-sort(hrs)
res.dat.gp4<-aggregate(tgp4$rs, by=list(tgp4$hrs), FUN=sum)[2]
res.dat.gp4$hr<-0:23
dat.p.gp4<-as.data.frame(rep(0:23,times=c(res.dat.gp4$x)))
colnames(dat.p.gp4)<-'x'

pdf(file = "Gap Cave 2014-2015 rose.pdf")
ggplot(dat.p.gp4, aes(x = x)) + geom_histogram(breaks = seq(0,24), width = 2, colour = "grey") + coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Pixel count") + ggtitle("Gap Cave 2014-2015 \n Activity by hour") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))
dev.off()

##

tw1E<-as.data.frame(t(w1E))
tw1E$rs<-rowSums(tw1E)
tw1E$hrs<-sort(hrs)
res.dat.w1E<-aggregate(tw1E$rs, by=list(tw1E$hrs), FUN=sum)[2]
res.dat.w1E$hr<-0:23
dat.p.w1E<-as.data.frame(rep(0:23,times=c(res.dat.w1E$x)))
colnames(dat.p.w1E)<-'x'

pdf(file = "Wyandotte Cave 2011-2012 rose.pdf")
ggplot(dat.p.w1E, aes(x = x)) + geom_histogram(breaks = seq(0,24), width = 2, colour = "grey") + coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Pixel count") + ggtitle("Wyandotte Cave 2011-2012 \n Activity by hour") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))
dev.off()

##
tw2E<-as.data.frame(t(w2E))
tw2E$rs<-rowSums(tw2E)
tw2E$hrs<-sort(hrs)
res.dat.w2E<-aggregate(tw2E$rs, by=list(tw2E$hrs), FUN=sum)[2]
res.dat.w2E$hr<-0:23
dat.p.w2E<-as.data.frame(rep(0:23,times=c(res.dat.w2E$x)))
colnames(dat.p.w2E)<-'x'

pdf(file = "Wyandotte Cave 2012-2013 rose.pdf")
ggplot(dat.p.w2E, aes(x = x)) + geom_histogram(breaks = seq(0,24), width = 2, colour = "grey") + coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Pixel count") + ggtitle("Wyandotte Cave 2012-2013 \n Activity by hour") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))
dev.off()

##

tw3E<-as.data.frame(t(w3E))
tw3E$rs<-rowSums(tw3E)
tw3E$hrs<-sort(hrs)
res.dat.w3E<-aggregate(tw3E$rs, by=list(tw3E$hrs), FUN=sum)[2]
res.dat.w3E$hr<-0:23
dat.p.w3E<-as.data.frame(rep(0:23,times=c(res.dat.w3E$x)))
colnames(dat.p.w3E)<-'x'

pdf(file = "Wyandotte Cave 2013-2014 rose.pdf")
ggplot(dat.p.w3E, aes(x = x)) + geom_histogram(breaks = seq(0,24), width = 2, colour = "grey") + coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Pixel count") + ggtitle("Wyandotte Cave 2013-2014 \n Activity by hour") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))
dev.off()

##

tw1H<-as.data.frame(t(w1H))
tw1H$rs<-rowSums(tw1H)
tw1H$hrs<-sort(hrs)
res.dat.w1H<-aggregate(tw1H$rs, by=list(tw1H$hrs), FUN=sum)[2]
res.dat.w1H$hr<-0:23
dat.p.w1H<-as.data.frame(rep(0:23,times=c(res.dat.w1H$x)))
colnames(dat.p.w1H)<-'x'

pdf(file = "Wyandotte Cave deeper 2013-2014 rose.pdf")
ggplot(dat.p.w1H, aes(x = x)) + geom_histogram(breaks = seq(0,24), width = 2, colour = "grey") + coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Pixel count") + ggtitle("Wyandotte Cave deeper 2013-2014 \n Activity by hour") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))
dev.off()
##