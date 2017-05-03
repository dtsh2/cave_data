
rm(list=ls())

library(dplR)
library(xts)
library(matrixStats)
library(lubridate)
library(ggplot2)   # use at least 0.9.3 for theme_minimal()

### load data

gap1 = read.csv("VA_2011-2012.csv",na.strings=c("NA"," - "),header=F)
gap2 = read.csv("VA_2012-2013.csv",na.strings=c("NA"," - "),header=F)
w1E = read.csv("IN_2011-2012 (Entrance).csv",na.strings=c("NA"," - "),header=F)
w2E = read.csv("IN_2012-2013 (Entrance).csv",na.strings=c("NA"," - "),header=F)
w3E = read.csv("IN_2013-2014 (Entrance).csv",na.strings=c("NA"," - "),header=F)
w1H = read.csv("IN_2013-2014 (Deeper).csv",na.strings=c("NA"," - "),header=F)

### choose data

data<-gap1
data_name <-'gap1'
data<-gap2
data_name <-'gap2'
# data<-w1E
# data_name <-'w1E'
# data<-w2E
# data_name <-'w2E'
# data<-w3E
# data_name <-'w3E'
# data<-w1H
# data_name <-'w1H'

### set up

par(mfrow=c(1,1))

main_fun<- ifelse(data_name == 'gap1','M. lu',
                 ifelse(data_name == 'gap2','Test_Year_2'))
axis_fun_1<- ifelse(data_name == 'gap1',ax_1<-seq(from=0,to=max(gap1),by=10),
                  ifelse(data_name == 'gap2',ax_1<-seq(from=0,to=max(gap2),by=10)))
axis_fun_2<- ifelse(data_name == 'gap1',ax_2<-seq(from=0,to=max(gap1),by=100),
                    ifelse(data_name == 'gap2',ax_2<-seq(from=0,to=max(gap2),by=100)))

### plot means

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

# ### plot means by month
# 
# ### plot means over time
# 
# id_wy12 <- c(31:43, 59:64)
# id_wy13 <-c(69:76)
# id_wyH13 <-c(69, 74, 83, 85)
# 
# par(mfrow=c(1,1))
# 
# ##
# dim(t(w1E))
# pdf(file = "Wyandotte 2011-2012 month.pdf")
# par(mfrow=c(2,2))
# par(oma=c(1,2,1,2))
# par(mar=c(2,2,1,1))
# # set upper limit for scaling axes
# 
# u95ci<-rowMeans(t(w1E)[,1:5])+rowSds(t(w1E)[,1:5])/sqrt(length((t(w1E)[1,])))
# l95ci<-rowMeans(t(w1E)[,1:5])-rowSds(t(w1E)[,1:5])/sqrt(length((t(w1E))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(w1E)[,1:5]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",
#      col="black",ylab="# saturated pixels",main="Wyandotte January (2011-2012)",bty="n")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# u95ci<-rowMeans(t(w1E)[,6:34])+rowSds(t(w1E)[,6:34])/sqrt(length((t(w1E)[1,])))
# l95ci<-rowMeans(t(w1E)[,6:34])-rowSds(t(w1E)[,6:34])/sqrt(length((t(w1E))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(w1E)[,6:34]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Wyandotte February (2011-2012)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# u95ci<-rowMeans(t(w1E)[,35:65])+rowSds(t(w1E)[,35:65])/sqrt(length((t(w1E)[1,])))
# l95ci<-rowMeans(t(w1E)[,35:65])-rowSds(t(w1E)[,35:65])/sqrt(length((t(w1E))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(w1E)[,35:65]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Wyandotte March (2011-2012)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# dev.off()
# 
# ##
# dim(t(w2E))
# pdf(file = "Wyandotte 2012-2013 month.pdf")
# par(mfrow=c(3,2))
# par(oma=c(1,4,1,2))
# par(mar=c(4,4,1,1))
# u95ci<-rowMeans(t(w2E)[,1:10])+rowSds(t(w2E)[,1:10])/sqrt(length((t(w2E)[1,])))
# l95ci<-rowMeans(t(w2E)[,1:10])-rowSds(t(w2E)[,1:10])/sqrt(length((t(w2E))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(w2E)[,1:10]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",
#      col="black",ylab="# saturated pixels",main="Wyandotte November (2012-2013)",bty="n")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# lines(rowMeans(t(w2E)[,1:10]))
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# u95ci<-rowMeans(t(w2E)[,11:42])+rowSds(t(w2E)[,11:42])/sqrt(length((t(w2E)[1,])))
# l95ci<-rowMeans(t(w2E)[,11:42])-rowSds(t(w2E)[,11:42])/sqrt(length((t(w2E))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(w2E)[,11:42]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Wyandotte December (2012-2013)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# u95ci<-rowMeans(t(w2E)[,43:72])+rowSds(t(w2E)[,43:72])/sqrt(length((t(w2E)[1,])))
# l95ci<-rowMeans(t(w2E)[,43:72])-rowSds(t(w2E)[,43:72])/sqrt(length((t(w2E))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(w2E)[,43:72]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Wyandotte January (2012-2013)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# u95ci<-rowMeans(t(w2E)[,73:100])+rowSds(t(w2E)[,73:100])/sqrt(length((t(w2E)[1,])))
# l95ci<-rowMeans(t(w2E)[,73:100])-rowSds(t(w2E)[,73:100])/sqrt(length((t(w2E))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(w2E)[,73:100]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Wyandotte February (2012-2013)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# u95ci<-rowMeans(t(w2E)[,101:130])+rowSds(t(w2E)[,101:130])/sqrt(length((t(w2E)[1,])))
# l95ci<-rowMeans(t(w2E)[,101:130])-rowSds(t(w2E)[,101:130])/sqrt(length((t(w2E))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(w2E)[,101:130]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Wyandotte March (2012-2013)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# dev.off()
# 
# ##
# dim(t(w3E))
# pdf(file = "Wyandotte 2013-2014 month.pdf")
# par(mfrow=c(3,2))
# par(oma=c(1,4,1,2))
# par(mar=c(4,4,1,1))
# u95ci<-rowMeans(t(w3E)[,1:2])+rowSds(t(w3E)[,1:2])/sqrt(length((t(w3E)[1,])))
# l95ci<-rowMeans(t(w3E)[,1:2])-rowSds(t(w3E)[,1:2])/sqrt(length((t(w3E))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(w3E)[,1:2]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",
#      col="black",ylab="# saturated pixels",main="Wyandotte November (2013-2014)",bty="n")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# u95ci<-rowMeans(t(w3E)[,3:34])+rowSds(t(w3E)[,3:34])/sqrt(length((t(w3E)[1,])))
# l95ci<-rowMeans(t(w3E)[,3:34])-rowSds(t(w3E)[,3:34])/sqrt(length((t(w3E))[1,]))
# lims<-na.omit(cbind(u95ci,l95ci))
# plot(rowMeans(t(w3E)[,3:34],na.rm=T),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Wyandotte December (2013-2014)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# ## plot means over time
# 
# u95ci<-rowMeans(t(w3E)[,35:62])+rowSds(t(w3E)[,35:62])/sqrt(length((t(w3E)[1,])))
# l95ci<-rowMeans(t(w3E)[,35:62])-rowSds(t(w3E)[,35:62])/sqrt(length((t(w3E))[1,]))
# lims<-na.omit(cbind(u95ci,l95ci))
# plot(rowMeans(t(w3E)[,35:62]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Wyandotte January (2013-2014)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# ## plot means over time
# 
# u95ci<-rowMeans(t(w3E)[,63:91])+rowSds(t(w3E)[,63:91])/sqrt(length((t(w3E)[1,])))
# l95ci<-rowMeans(t(w3E)[,63:91])-rowSds(t(w3E)[,63:91])/sqrt(length((t(w3E))[1,]))
# lims<-na.omit(cbind(u95ci,l95ci))
# plot(rowMeans(t(w3E)[,63:91],na.rm=T),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Wyandotte February (2013-2014)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# u95ci<-rowMeans(t(w3E)[,92:113])+rowSds(t(w3E)[,92:113])/sqrt(length((t(w3E)[1,])))
# l95ci<-rowMeans(t(w3E)[,92:113])-rowSds(t(w3E)[,92:113])/sqrt(length((t(w3E))[1,]))
# lims<-na.omit(cbind(u95ci,l95ci))
# plot(rowMeans(t(w3E)[,92:113],na.rm=T),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Wyandotte March (2013-2014)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# dev.off()
# ## plot means over time
# 
# 
# ##
# dim(t(w1H))
# pdf(file = "Wyandotte Cave deeper 2013-2014 month.pdf")
# par(mfrow=c(3,2))
# par(oma=c(1,4,1,2))
# par(mar=c(4,4,1,1))
# u95ci<-rowMeans(t(w1H)[,1:2])+rowSds(t(w1H)[,1:2])/sqrt(length((t(w1H)[1,])))
# l95ci<-rowMeans(t(w1H)[,1:2])-rowSds(t(w1H)[,1:2])/sqrt(length((t(w1H))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(w1H)[,1:2]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",
#      col="black",ylab="# saturated pixels",main="Wyandotte deeper November (2013-2014)",bty="n")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# ## plot means over time
# 
# u95ci<-rowMeans(t(w1H)[,3:34])+rowSds(t(w1H)[,3:34])/sqrt(length((t(w1H)[1,])))
# l95ci<-rowMeans(t(w1H)[,3:34])-rowSds(t(w1H)[,3:34])/sqrt(length((t(w1H))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(w1H)[,3:34]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",
#      col="black",ylab="# saturated pixels",main="Wyandotte deeper December (2013-2014)",bty="n")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# u95ci<-rowMeans(t(w1H)[,35:66])+rowSds(t(w1H)[,35:66])/sqrt(length((t(w1H)[1,])))
# l95ci<-rowMeans(t(w1H)[,35:66])-rowSds(t(w1H)[,35:66])/sqrt(length((t(w1H))[1,]))
# lims<-na.omit(cbind(u95ci,l95ci))
# plot(rowMeans(t(w1H)[,35:66],na.rm=T),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Wyandotte deeper January (2013-2014)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# ## plot means over time
# 
# 
# u95ci<-rowMeans(t(w1H)[,67:95])+rowSds(t(w1H)[,67:95])/sqrt(length((t(w1H)[1,])))
# l95ci<-rowMeans(t(w1H)[,67:95])-rowSds(t(w1H)[,67:95])/sqrt(length((t(w1H))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(w1H)[,67:95]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Wyandotte deeper February (2013-2014)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# ## plot means over time
# ##
# u95ci<-rowMeans(t(w1H)[,96:114])+rowSds(t(w1H)[,96:114])/sqrt(length((t(w1H)[1,])))
# l95ci<-rowMeans(t(w1H)[,96:114])-rowSds(t(w1H)[,96:114])/sqrt(length((t(w1H))[1,]))
# lims<-na.omit(cbind(u95ci,l95ci))
# plot(rowMeans(t(w1H)[,96:114],na.rm=T),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Wyandotte deeper March (2013-2014)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# dev.off()
# 
# ##################################################
# ## plot means over time
# 
# id_gap1 <- c(12,22,47,66)
# id_gap2 <-c(35,36)
# id_gap3 <-c(36,39:57)
# 
# ##
# 
# dim(t(gap1))
# pdf(file = "Gap Cave 2011-2012 month.pdf")
# par(mfrow=c(2,2))
# par(oma=c(1,4,1,2))
# par(mar=c(4,4,1,1))
# u95ci<-rowMeans(t(gap1)[,1:12])+rowSds(t(gap1)[,1:12])/sqrt(length((t(gap1)[1,])))
# l95ci<-rowMeans(t(gap1)[,1:12])-rowSds(t(gap1)[,1:12])/sqrt(length((t(gap1))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(gap1)[,1:12]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",
#      col="black",ylab="# saturated pixels",main="Gap Cave November (2011-2012)",bty="n")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)
# ##
# 
# 
# u95ci<-rowMeans(t(gap1)[,13:44])+rowSds(t(gap1)[,13:44])/sqrt(length((t(gap1)[1,])))
# l95ci<-rowMeans(t(gap1)[,13:44])-rowSds(t(gap1)[,13:44])/sqrt(length((t(gap1))[1,]))
# lims<-na.omit(cbind(u95ci,l95ci))
# plot(rowMeans(t(gap1)[,13:44],na.rm=T),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Gap Cave December (2011-2012)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# 
# u95ci<-rowMeans(t(gap1)[,45:76])+rowSds(t(gap1)[,45:76])/sqrt(length((t(gap1)[1,])))
# l95ci<-rowMeans(t(gap1)[,45:76])-rowSds(t(gap1)[,45:76])/sqrt(length((t(gap1))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(gap1)[,45:76]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Gap Cave January (2011-2012)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# u95ci<-rowMeans(t(gap1)[,77:101])+rowSds(t(gap1)[,77:101])/sqrt(length((t(gap1)[1,])))
# l95ci<-rowMeans(t(gap1)[,77:101])-rowSds(t(gap1)[,77:101])/sqrt(length((t(gap1))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(gap1)[,77:101]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Gap Cave February (2011-2012)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# dev.off()
# 
# ##
# dim(t(gap2))
# pdf(file = "Gap Cave 2012-2013 month.pdf")
# par(mfrow=c(3,2))
# par(oma=c(1,4,1,2))
# par(mar=c(4,4,1,1))
# u95ci<-rowMeans(t(gap2)[,1:27])+rowSds(t(gap2)[,1:27])/sqrt(length((t(gap2)[1,])))
# l95ci<-rowMeans(t(gap2)[,1:27])-rowSds(t(gap2)[,1:27])/sqrt(length((t(gap2))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(gap2)[,1:27]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",
#      col="black",ylab="# saturated pixels",main="Gap Cave December (2012-2013)",bty="n")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# ##
# 
# 
# u95ci<-rowMeans(t(gap2)[,27:58])+rowSds(t(gap2)[,27:58])/sqrt(length((t(gap2)[1,])))
# l95ci<-rowMeans(t(gap2)[,27:58])-rowSds(t(gap2)[,27:58])/sqrt(length((t(gap2))[1,]))
# lims<-na.omit(cbind(u95ci,l95ci))
# plot(rowMeans(t(gap2)[,27:58],na.rm=T),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Gap Cave January (2012-2013)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# ##
# 
# 
# u95ci<-rowMeans(t(gap2)[,59:87])+rowSds(t(gap2)[,59:87])/sqrt(length((t(gap2)[1,])))
# l95ci<-rowMeans(t(gap2)[,59:87])-rowSds(t(gap2)[,59:87])/sqrt(length((t(gap2))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(gap2)[,59:87]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Gap Cave February (2012-2013)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# 
# u95ci<-rowMeans(t(gap2)[,87:118])+rowSds(t(gap2)[,87:118])/sqrt(length((t(gap2)[1,])))
# l95ci<-rowMeans(t(gap2)[,87:118])-rowSds(t(gap2)[,87:118])/sqrt(length((t(gap2))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(gap2)[,87:118]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Gap Cave March (2012-2013)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# 
# u95ci<-rowMeans(t(gap2)[,118:145])+rowSds(t(gap2)[,118:145])/sqrt(length((t(gap2)[1,])))
# l95ci<-rowMeans(t(gap2)[,118:145])-rowSds(t(gap2)[,118:145])/sqrt(length((t(gap2))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(gap2)[,118:145]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Gap Cave April (2012-2013)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# dev.off()
# ##
# 
# ##
# dim(t(gap3))
# pdf(file = "Gap Cave 2012-2013 month.pdf")
# par(mfrow=c(2,2))
# par(oma=c(1,4,1,2))
# par(mar=c(4,4,1,1))
# u95ci<-rowMeans(t(gap3)[,1:27])+rowSds(t(gap3)[,1:27])/sqrt(length((t(gap3)[1,])))
# l95ci<-rowMeans(t(gap3)[,1:27])-rowSds(t(gap3)[,1:27])/sqrt(length((t(gap3))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(gap3)[,1:27]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",
#      col="black",ylab="# saturated pixels",main="Gap Cave December (2013-2014)",bty="n")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# ##
# 
# u95ci<-rowMeans(t(gap3)[,28:59])+rowSds(t(gap3)[,28:59])/sqrt(length((t(gap3)[1,])))
# l95ci<-rowMeans(t(gap3)[,28:59])-rowSds(t(gap3)[,28:59])/sqrt(length((t(gap3))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(gap3)[,28:59]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Gap Cave January (2013-2014)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# u95ci<-rowMeans(t(gap3)[,60:73])+rowSds(t(gap3)[,60:73])/sqrt(length((t(gap3)[1,])))
# l95ci<-rowMeans(t(gap3)[,60:73])-rowSds(t(gap3)[,60:73])/sqrt(length((t(gap3))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(gap3)[,60:73]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Gap Cave February (2013-2014)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# dev.off()
# ##
# 
# ##
# dim(t(gap4))
# pdf(file = "Gap Cave 2014-2015 month.pdf")
# par(mfrow=c(3,2))
# par(oma=c(1,4,1,2))
# par(mar=c(4,4,1,1))
# u95ci<-rowMeans(t(gap4)[,1:8])+rowSds(t(gap4)[,1:8])/sqrt(length((t(gap4)[1,])))
# l95ci<-rowMeans(t(gap4)[,1:8])-rowSds(t(gap4)[,1:8])/sqrt(length((t(gap4))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(gap4)[,1:8]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",
#      col="black",ylab="# saturated pixels",main="Gap Cave November (2014-2015)",bty="n")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# ##
# 
# u95ci<-rowMeans(t(gap4)[,9:40])+rowSds(t(gap4)[,9:40])/sqrt(length((t(gap4)[1,])))
# l95ci<-rowMeans(t(gap4)[,9:40])-rowSds(t(gap4)[,9:40])/sqrt(length((t(gap4))[1,]))
# lims<-na.omit(cbind(u95ci,l95ci))
# plot(rowMeans(t(gap4)[,9:40],na.rm=T),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Gap Cave December (2014-2015)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# 
# u95ci<-rowMeans(t(gap4)[,41:72])+rowSds(t(gap4)[,41:72])/sqrt(length((t(gap4)[1,])))
# l95ci<-rowMeans(t(gap4)[,41:72])-rowSds(t(gap4)[,41:72])/sqrt(length((t(gap4))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(gap4)[,41:72]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,100),xlab="hours",bty="n", ###### NOTE NOT MAX #####
#      col="black",ylab="# saturated pixels",main="Gap Cave January (2014-2015)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# ##
# 
# u95ci<-rowMeans(t(gap4)[,73:101])+rowSds(t(gap4)[,73:101])/sqrt(length((t(gap4)[1,])))
# l95ci<-rowMeans(t(gap4)[,73:101])-rowSds(t(gap4)[,73:101])/sqrt(length((t(gap4))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(gap4)[,73:101]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Gap Cave February (2014-2015)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# ##
# 
# u95ci<-rowMeans(t(gap4)[,102:132])+rowSds(t(gap4)[,102:132])/sqrt(length((t(gap4)[1,])))
# l95ci<-rowMeans(t(gap4)[,102:132])-rowSds(t(gap4)[,102:132])/sqrt(length((t(gap4))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(gap4)[,102:132]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Gap Cave March (2014-2015)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# u95ci<-rowMeans(t(gap4)[,133:139])+rowSds(t(gap4)[,133:139])/sqrt(length((t(gap4)[1,])))
# l95ci<-rowMeans(t(gap4)[,133:139])-rowSds(t(gap4)[,133:139])/sqrt(length((t(gap4))[1,]))
# lims<-cbind(u95ci,l95ci)
# plot(rowMeans(t(gap4)[,133:139]),type="l",xaxt="n",#xlim=c(0,1200),xaxt="n",
#      ylim=c(0,max(lims)),xlab="hours",bty="n",
#      col="black",ylab="# saturated pixels",main="Gap Cave April (2014-2015)")
# lines(u95ci,col="grey")
# lines(l95ci,col="grey")
# axis(side=1, at = seq(from =0, to= 288, by = 288/24),
#      labels = c(0:24), tick = TRUE,
#      lty=1)#,outer=T)
# 
# 
# dev.off()
# 
# ##########################################

### rose plot

hrs11<-rep(seq(from=1,to=24,by=2),12)
hrs12<-rep(seq(from=0,to=23,by=2),12)
hrs<-c(hrs11,hrs12)

data_rose<-as.data.frame(t(data))
data_rose$rs<-rowSums(data_rose)
data_rose$hrs<-sort(hrs)
data_rose_r<-aggregate(data_rose$rs, by=list(data_rose$hrs), FUN=sum)[2]
data_rose_r$hr<-1:24
data_rose_r_plot<-as.data.frame(rep(1:24,times=c(data_rose_r$x)))
colnames(data_rose_r_plot)<-'x'

pdf(file = paste(data_name,"_rose.pdf",sep=''))
ggplot(data_rose_r_plot, aes(x = x)) + geom_histogram(breaks = seq(0,24), colour = "grey") + coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Pixel count") + ggtitle(paste(data_name,"\n Activity by hour")) + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))
dev.off()

### plot the data

g1<-as.vector(t(gap1))
g2<-as.vector(t(gap2))
w1<-as.vector(t(w1E))
w2<-as.vector(t(w2E))
w3<-as.vector(t(w3E))
w1h<-as.vector(t(w1H))

x_lim_fun<- ifelse(data_name == 'gap1',x_lim<-(c(-18*288,(6*30*288)-18*288)),
                    ifelse(data_name == 'gap2',x_lim<-(c(-((4+31)*288),(6*30*288)-((4+31)*288)))))
at_fun <- ifelse(data_name == 'gap1',at_1<-seq(from = -18*288, to= 8640*6, by = 8640),
                   ifelse(data_name == 'gap2',at_1<-seq(from = -(4+31)*288, to= 8640*5, by = 8640)))
###
# w1
# xlim=c(-((31+31+26)*288),(6*30*288)-(31+31+26)*288)
# at = seq(from =-(31+31+26)*288, to= 8640*4, by = 8640)
# 
# w2
# xlim=c(-((20)*288),(6*30*288)-(20)*288)
# at = seq(from =-20*288, to= 8640*6, by = 8640)
# 
# w3
# xlim=c(-((28)*288),(6*30*288)-(28)*288)
# at = seq(from =-28*288, to= 8640*6, by = 8640)
# 
# w1h
# xlim=c(-((28)*288),(6*30*288)-(28)*288)
# at = seq(from =-28*288, to= 8640*6, by = 8640)

###

data<-as.vector(t(as.matrix(data,header=F)))

pdf(file = paste(data_name,"_data.pdf",sep=''))
plot(data,xlim=x_lim,type='l',ylab="No. saturated pixels",xlab='time', xaxt='n',main=data_name)
axis(side=1, at = at_1,
     labels = c("Nov","Dec","Jan","Feb","Mar","Apr","May"), tick = TRUE,
     lty=2)
dev.off()

### wavelet

mor_data<-morlet(sqrt(data), x1 = seq_along(data), p2 = NULL, dj = 0.25, siglvl = 0.95)

pdf(file = paste(data_name,"_morlet.pdf",sep=''))
  wavelet.plot(mor_data,crn.lab="Pixel",x.lab="Time")
dev.off()

### acf

pdf(file = paste(data_name,"_acf.pdf",sep=''))
  par(mfrow=c(1,1))
  acf(data, lag.max = 288*10,plot=T,main=main_fun,xlab="Lag (Days)",axes=F,
      ylab="Autocorrelation")
  axis(at=seq(from=0,to=1,by=.2),labels=seq(from=0,to=1,by=.2),side=2)
  box()
  axis(at=seq(from=0,to=288*10,by=288),labels=seq(from=0,to=10,by=1),side=1)
  legend("topright","p-value = 0.05",lty=2,col="blue",bty="n")
dev.off()

###