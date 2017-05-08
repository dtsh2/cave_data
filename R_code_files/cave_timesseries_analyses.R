
rm(list=ls())

library(dplR)
library(xts)
library(matrixStats)
library(lubridate)
library(ggplot2)   # use at least 0.9.3 for theme_minimal()

### load data

gap1 = read.csv("VA_2011-2012.csv",na.strings=c("NA"," - "),header=F)
gap2 = read.csv("VA_2012-2013.csv",na.strings=c("NA"," - "),header=F)
gap3 = read.csv("VA_2013-2014.csv",na.strings=c("NA"," - "),header=F)
w1E = read.csv("IN_2011-2012 (Entrance).csv",na.strings=c("NA"," - "),header=F)
w2E = read.csv("IN_2012-2013 (Entrance).csv",na.strings=c("NA"," - "),header=F)
w3E = read.csv("IN_2013-2014 (Entrance).csv",na.strings=c("NA"," - "),header=F)
w3H = read.csv("IN_2013-2014 (Deeper).csv",na.strings=c("NA"," - "),header=F)

### choose data

# data<-gap1
# data_name <-'gap1'
# data<-gap2
# data_name <-'gap2'
# data<-gap3
# data_name <-'gap3'
# data<-w1E
# data_name <-'w1E'
# data<-w2E
# data_name <-'w2E'
# data<-w3E
# data_name <-'w3E'
# data<-w3H
# data_name <-'w3H'

### set up

par(mfrow=c(1,1))

main_fun<- ifelse(data_name == 'gap1',expression(paste('Gap Cave, VA, 2011-2012 ',italic('(M. lucifugus)'))),
              ifelse(data_name == 'gap2',expression(paste('Gap Cave, VA, 2012-2013 ',italic('(M. lucifugus)'))),
              ifelse(data_name == 'gap3',expression(paste('Gap Cave, VA, 2013-2014 ',italic('(M. lucifugus)'))),
              ifelse(data_name == 'w1E',expression(paste('Wyandotte Cave, IN, 2011-2012 ',italic('(M. sodalis)'))),
              ifelse(data_name == 'w2E',expression(paste('Wyandotte Cave, IN, 2012-2013 ',italic('(M. sodalis)'))),
              ifelse(data_name == 'w3E',expression(paste('Wyandotte Cave, IN, 2013-2014 ',italic('(M. sodalis)'))),
              ifelse(data_name == 'w3H',expression(paste('Wyandotte Cave, IN, 2013-2014 (Deeper) ',italic('(M. sodalis)'))),
              "NA")))))))
axis_fun_1<- ifelse(data_name == 'gap1',ax_1<-seq(from=0,to=max(gap1),by=10),
              ifelse(data_name == 'gap2',ax_1<-seq(from=0,to=max(gap2),by=10),
              ifelse(data_name == 'gap3',ax_1<-seq(from=0,to=max(gap3),by=10),
              ifelse(data_name == 'w1E',ax_1<-seq(from=0,to=max(w1E),by=10),
              ifelse(data_name == 'w2E',ax_1<-seq(from=0,to=max(w2E),by=10),
              ifelse(data_name == 'w3E',ax_1<-seq(from=0,to=max(w3E),by=10),
              ifelse(data_name == 'w3H',ax_1<-seq(from=0,to=max(w3H),by=10),
              "NA")))))))
axis_fun_2<- ifelse(data_name == 'gap1',ax_2<-seq(from=0,to=max(gap1),by=10),
              ifelse(data_name == 'gap2',ax_2<-seq(from=0,to=max(gap2),by=10),
              ifelse(data_name == 'gap3',ax_2<-seq(from=0,to=max(gap3),by=10),
              ifelse(data_name == 'w1E',ax_2<-seq(from=0,to=max(w1E),by=10),
              ifelse(data_name == 'w2E',ax_2<-seq(from=0,to=max(w2E),by=10),
              ifelse(data_name == 'w3E',ax_2<-seq(from=0,to=max(w3E),by=10),
              ifelse(data_name == 'w3H',ax_2<-seq(from=0,to=max(w3H),by=10),
              "NA")))))))

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
  scale_fill_brewer() + ylab("Pixel count") + ggtitle(c(main_fun)) + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))
dev.off()

### plot the data

st_fun <- ifelse(data_name == 'gap1',start<-c('2011-11-18 00:00:00'),
                ifelse(data_name == 'gap2',start<-c('2012-12-04 00:00:00'),
                ifelse(data_name == 'gap3',start<-c('2014-02-04 00:00:00'),
                ifelse(data_name == 'w1E',start<-c('2012-01-26 00:00:00'),
                ifelse(data_name == 'w2E',start<-c('2012-11-20 00:00:00'),
                ifelse(data_name == 'w3E',start<-c('2013-11-28 00:00:00'),
                ifelse(data_name == 'w3H',start<-c('2013-11-28 00:00:00'))))))))

data<-as.vector(as.matrix(t(data)))

df <- data.frame(interval = seq(ymd_hms(start), 
                                by = '5 min',length.out=length(data)), 
                 data = data)
ts <- xts(df$data, order.by=df$interval)

pdf(file = paste(data_name,"_data.pdf",sep=''))
plot(ts,
     #xlim=as.POSIXct(c('2012-01-26 00:00:00','2012-04-26 00:00:00')), # to set axes limits
     main=main_fun,major.ticks="weeks", major.format="%b %d")
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