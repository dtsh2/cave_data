
rm(list=ls())

library(matrixStats) ## 
library(dplR)
library(xts)

#### load data

gap1 = read.csv("Gap_2011-2012.csv",na.strings=c("NA"," - "),header=F)
# gap2 = read.csv("Gap_2012-2013.csv",na.strings=c("NA"," - "),header=F)
# gap3 = read.csv("Gap_2013-2014.csv",na.strings=c("NA"," - "),header=F)
# gap4 = read.csv("Gap_2014-2015.csv",na.strings=c("NA"," - "),header=F)
# w1E = read.csv("WyandotteE_2011-2012.csv",na.strings=c("NA"," - "),header=F)
# w2E = read.csv("WyandotteE_2012-2013.csv",na.strings=c("NA"," - "),header=F)
# w3E = read.csv("WyandotteE_2013-2014.csv",na.strings=c("NA"," - "),header=F)
# w3E[w3E>10000] <- NA # remove single point
# w1H = read.csv("WyandotteH_2013-2014.csv",na.strings=c("NA"," - "),header=F)

## example of plotting means with 95% CI

pdf(file = "Gap Cave 2011-2012 means.pdf")
par(mfrow=c(1,1))
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

# example of plotting time series

tgap1<-as.matrix(gap1,header=F)
tgap1<-as.vector(t(tgap1))
pdf(file = "Gap Cave 2011-2012.pdf")
plot(tgap1,type="l",
     main="Gap Cave 2011-2012",xlab="weeks",ylab="# saturated pixels",labels=F,axes=F)
axis(at=seq(from=0,to=20000,by=10000),labels=c(0,10000,20000),side=2)
box()
axis(at=seq(from=0,to=length(tgap1),by=288*7),labels=seq(from=0,to=14,by=1),side=1)
dev.off()

# example of plotting time series morlet analysis

wtgap1<-morlet(sqrt(tgap1), x1 = seq_along(tgap1), p2 = NULL, dj = 0.25, siglvl = 0.95)
pdf(file = "Gap Cave 2011-2012 morlet.pdf") 
wavelet.plot(wtgap1,crn.lab="Pixel",x.lab="Time")
dev.off()

# example of ACF

pdf(file = "Gap Cave 2011-2012 acf.pdf")
par(mfrow=c(1,1))
acf(tgap1, lag.max = 288*10,plot=T,main="Gap Cave 2011-2012",xlab="Lag (Days)",labels=F,axes=F,
    ylab="Autocorrelation")
axis(at=c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),side=2)
box()
axis(at=seq(from=0,to=288*10,by=288),labels=seq(from=0,to=10,by=1),side=1)
legend("topright","p-value = 0.05",lty=2,col="blue",bty="n")
dev.off()

## rose plots

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