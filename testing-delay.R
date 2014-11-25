library(ISOweek)
library(foreign)

#doIt()

holiday.file<-holiday()
summary(holiday.file)
summary(rTDF)
summary(crTDF1<-delay(rTDF,opentype="n",holiday=holiday.file))
crTDF2<-delay(rTDF,opentype="o",holiday=holiday.file)
crTDF3<-delay(rTDF,opentype="e",holiday=holiday.file)
crTDF4<-delay.nb(rTDF,holiday=holiday.file)
names(crTDF3)

with(rTDF,matplot(cbind(w00,w01,w02,w03,w04,w05,w06),type="l",lty=1))
with(rTDF,matplot(cbind(w00,w01,w02,w03,w04,w05,w06)/w06,type="l",lty=1))

crTDFall<-rbind(cbind(crTDF1,type="n")[,c("ISOweek","cnb","v.cnb","type","onb")],
                cbind(crTDF2,type="o")[,c("ISOweek","cnb","v.cnb","type","onb")],
                cbind(crTDF3,type="e")[,c("ISOweek","cnb","v.cnb","type","onb")],
                cbind(crTDF4,type="nb",onb=NA)[,c("ISOweek","cnb","v.cnb","type","onb")])
crTDFall$u.cnb<-with(crTDFall,cnb+2*sqrt(v.cnb))
crTDFall$l.cnb<-with(crTDFall,cnb-2*sqrt(v.cnb))

with(subset(crTDFall,as.character(ISOweek)>"2012-W40"),
     xyplot(onb+cnb+u.cnb+l.cnb~as.numeric(ISOweek)|type,
                      type="l",lwd=c(3,1,1,1),lty=c(1,1,2,2),col=c(1,2,3,3)))


with(crTDF,tail(sqrt(v.cnb)))

with(subset(rTDF.long,as.character(ISOweek)>"2012-W40"),
     xyplot(open/maxopen~as.numeric(ISOweek)|delay,
            type="l",lwd=c(3,1,1,1),lty=c(1,1,2,2),col=c(1,2,3,3)))

