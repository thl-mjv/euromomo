holiday.file<-holiday()
summary(holiday.file)
summary(rTDF)
tail(rTDF)
opts<-options()$euromomo
opts$StartDelayEst<-"2008-W01"
options(euromomo=opts)
summary(crTDF1<-delay(rTDF,opentype="n",holiday=holiday.file))
summary(crTDF2<-delay(rTDF,opentype="o",holiday=holiday.file))
summary(crTDF3<-delay(rTDF,opentype="e",holiday=holiday.file))
summary(crTDF4<-delay.nb(rTDF,holiday=holiday.file))
opts$StartDelayEst<-"2011-W01"
options(euromomo=opts)
summary(crTDF5<-delay.nb(rTDF,holiday=holiday.file))
summary(crTDF6<-delay(rTDF,opentype="e",holiday=holiday.file))

tail(crTDF4)
tail(crTDF5)
tail(crTDF3)
tail(crTDF6)



with(rTDF,matplot(cbind(w00,w01,w02,w03,w04,w05,w06),type="l",lty=1))
with(rTDF,matplot(cbind(w00,w01,w02,w03,w04,w05,w06)/w06,type="l",lty=1))

crTDFall<-rbind(cbind(crTDF1,type="n"),
                cbind(crTDF2,type="o"),
                cbind(crTDF3,type="e"),
                cbind(crTDF4,type="nb"))
crTDFall$u.cnb<-with(crTDFall,cnb+2*sqrt(v.cnb))
crTDFall$l.cnb<-with(crTDFall,cnb-2*sqrt(v.cnb))
crTDFall
with(subset(crTDFall,as.character(ISOweek)>"2012-W45"),
     xyplot(onb+nb+cnb+u.cnb+l.cnb~(ISOweek)|type,
                      type="l",lwd=c(1,3,1,1,1),lty=c(1,1,1,2,2),col=c(1,1,2,3,3)))


with(crTDF,tail(sqrt(v.cnb)))

with(subset(rTDF.long,as.character(ISOweek)>"2012-W40"),
     xyplot(open/maxopen~as.numeric(ISOweek)|delay,
            type="l",lwd=c(3,1,1,1),lty=c(1,1,2,2),col=c(1,2,3,3)))

