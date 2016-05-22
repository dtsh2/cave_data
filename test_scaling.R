dim(gap1)
dim(gap2)
dim(gap3)
dim(gap4)
dim(w1E)
dim(w2E)
dim(w3E)
dim(w1H)

tgap1<-as.matrix(gap1,header=F)
tgap1<-as.vector(t(tgap1))

tw1E<-as.matrix(w1E,header=F)
tw1E<-as.vector(t(tw1E))

head(tw1E)
min(tw1E)

tw2E<-as.matrix(w2E,header=F)
tw2E<-as.vector(t(tw2E))

head(tw2E)
min(tw2E)


test<-tw1E+1
# test<-tw2E+1
# test<-tw3E+1
# test<-tw1H+1

plot(sqrt(test),type='l')
plot(log(test),type='l')
test2<-log(test)
# test2<-sqrt(test)

z = (test2-mean(test2))/sd(test2)
plot(z,type='l')
# mean(z)
# var(z)
# std(z)

wttest2<-morlet(z, x1 = seq_along(z), p2 = NULL, dj = 0.95, siglvl = 0.99)
# wavelet.plot(wttest2,crn.lab="Pixel",x.lab="Time")
wavelet.plot(wttest2,crn.lab="Pixel",x.lab="Time",add.spline = T)

# x3R <- smooth(z, "3R") # 2 iterations of "3"
# smooth(x3R, kind = "S")
# 
# wttest3<-morlet(x3R, x1 = seq_along(x3R), p2 = NULL, dj = 0.5, siglvl = 0.99)
# wavelet.plot(wttest3,crn.lab="Pixel",x.lab="Time")

library(WaveletComp)
