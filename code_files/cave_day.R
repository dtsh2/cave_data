
rm(list=ls())

library(dplR)
library(xts)
library(matrixStats) ## this works

#### load data

gap1 = read.csv("VA_2011-2012.csv",na.strings=c("NA"," - "),header=F)
gap2 = read.csv("VA_2012-2013.csv",na.strings=c("NA"," - "),header=F)
w1E = read.csv("IN_2011-2012 (Entrance).csv",na.strings=c("NA"," - "),header=F)
w2E = read.csv("IN_2012-2013 (Entrance).csv",na.strings=c("NA"," - "),header=F)
w3E = read.csv("IN_2013-2014 (Entrance).csv",na.strings=c("NA"," - "),header=F)
w1H = read.csv("IN_2013-2014 (Deeper).csv",na.strings=c("NA"," - "),header=F)

###### test

data<-gap1
data_name <-'gap1'

par(mfrow=c(1,1))

main_fun<- ifelse(data_name == 'gap1','M. lu',
                 ifelse(data_name == 'gap2','Test_Year_2'))
axis_fun_1<- ifelse(data_name == 'gap1',ax_1<-seq(from=0,to=max(gap1),by=10),
                  ifelse(data_name == 'gap2',ax_1<-seq(from=0,to=max(gap2),by=10)))

### test end
pdf(file = paste(data_name,"_means.pdf",sep=''))
u95ci<-rowMeans((t(data)))+rowSds((t(data)))/sqrt(length((t(data))[1,]))
l95ci<-rowMeans((t(data)))-rowSds((t(data)))/sqrt(length((t(data))[1,]))
cis<-cbind(u95ci,l95ci)
lims<-range(cis[!is.na(cis)&is.finite(cis)])
plot(rowMeans((t(data))),type="l",ylim=c(min(lims),max(lims)),
     main=main_fun,xlab="hours",ylab="Mean # saturated pixels",axes=F)
lines(u95ci,col="grey")
lines(l95ci,col="grey")
axis(at=ax_1,labels=ax_1,side=2)
box()
axis(at=seq(from=0,to=288,by=288/24),labels=seq(from=0,to=24,by=1),side=1)
legend("topright",c("mean","95% confidence intervals"),lty=1,col=c("black","grey"),bty="n")
mtext("hours",side=1,line=3)
dev.off()

#########################
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

#gap1.f1 <- filter(tgap1,filter=rep(1/250,250))
pdf(file = "Gap Cave 2011-2012 acf.pdf")
par(mfrow=c(1,1))
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

#gap2.f1 <- filter(tgap2,filter=rep(1/250,250))
pdf(file = "Gap Cave 2012-2013 acf.pdf")
par(mfrow=c(1,1))
acf(tgap2, lag.max = 288*10,plot=T,main="Gap Cave 2012-2013",xlab="Lag (Days)",labels=F,axes=F,
    ylab="Autocorrelation")
axis(at=c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),side=2)
box()
axis(at=seq(from=0,to=288*10,by=288),labels=seq(from=0,to=10,by=1),side=1)
legend("topright","p-value = 0.05",lty=2,col="blue",bty="n")
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


#tw1E.f1 <- filter(tw1E,filter=rep(1/250,250))
pdf(file = "Wyandotte Cave 2011-2012 acf.pdf")
par(mfrow=c(1,1))

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


#w2E.f1 <- filter(tw2E,filter=rep(1/250,250))
pdf(file = "Wyandotte Cave 2012-2013 acf.pdf")
par(mfrow=c(1,1))

acf(tw2E, lag.max = 288*10,plot=T,main="Wyandotte Cave 2012-2013",xlab="Lag (Days)",labels=F,axes=F,
    ylab="Autocorrelation")
axis(at=c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),side=2)
box()
axis(at=seq(from=0,to=288*10,by=288),labels=seq(from=0,to=10,by=1),side=1)
legend("topright","p-value = 0.05",lty=2,col="blue",bty="n")
dev.off()


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


pdf(file = "Wyandotte Cave 2013-2014 acf.pdf")
par(mfrow=c(1,1))

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

#w1H.f1 <- filter(tw1H,filter=rep(1/250,250))
pdf(file = "Wyandotte Cave deeper 2013-2014 acf.pdf")
par(mfrow=c(1,1))

acf(tw1H, lag.max = 288*10,plot=T,main="Wyandotte Cave deeper 2013-2014",xlab="Lag (Days)",labels=F,axes=F,
    ylab="Autocorrelation")
axis(at=c(0,0.2,0.4,0.6,0.8,1),labels=c(0,0.2,0.4,0.6,0.8,1),side=2)
box()
axis(at=seq(from=0,to=288*10,by=288),labels=seq(from=0,to=10,by=1),side=1)
legend("topright","p-value = 0.05",lty=2,col="blue",bty="n")
dev.off()

##################################################
## plot means over time
id_wy12 <- c(31:43, 59:64)
id_wy13 <-c(69:76)
id_wyH13 <-c(69, 74, 83, 85)
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

g1<-as.vector(t(gap1))
g2<-as.vector(t(gap2))
g3<-as.vector(t(gap3))
g4<-as.vector(t(gap4))
w1<-as.vector(t(w1E))
w2<-as.vector(t(w2E))
w3<-as.vector(t(w3E))
w1hole<-as.vector(t(w1H))
##
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

hrs11<-rep(seq(from=1,to=24,by=2),12)
hrs12<-rep(seq(from=0,to=23,by=2),12)
hrs<-c(hrs11,hrs12)

tw3E<-as.data.frame(t(w3E))
tw3E$rs<-rowSums(tw3E)
tw3E$hrs<-sort(hrs)
res.dat.w3E<-aggregate(tw3E$rs, by=list(tw3E$hrs), FUN=sum)[2]
res.dat.w3E$hr<-1:24
dat.p.w3E<-as.data.frame(rep(1:24,times=c(res.dat.w3E$x)))
colnames(dat.p.w3E)<-'x'

pdf(file = "Wyandotte Cave 2013-2014 rose.pdf")
ggplot(dat.p.w3E, aes(x = x)) + geom_histogram(breaks = seq(0,24), colour = "grey") + coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Pixel count") + ggtitle("Wyandotte Cave 2013-2014 \n Activity by hour") + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))
dev.off()
