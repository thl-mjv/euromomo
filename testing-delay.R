doIt()

crTDF1<-delay(rTDF,opentype="n")
crTDF2<-delay(rTDF,opentype="o")
crTDF3<-delay(rTDF,opentype="e")


with(rTDF,matplot(cbind(w00,w01,w02,w03,w04,w05,w06),type="l",lty=1))
with(rTDF,matplot(cbind(w00,w01,w02,w03,w04,w05,w06)/w06,type="l",lty=1))

crTDFall<-rbind(cbind(crTDF1,type="n"),cbind(crTDF2,type="o"),cbind(crTDF3,type="e"))
with(subset(crTDFall,as.character(ISOweek)>"2012-W40"),
     xyplot(wr+cnb+u.cnb+l.cnb~as.numeric(ISOweek)|type,
                      type="l",lwd=c(3,1,1,1),lty=c(1,1,2,2),col=c(1,2,3,3)))


with(crTDF,tail(sqrt(v.cnb)))

with(subset(rTDF.long,as.character(ISOweek)>"2012-W40"),
     xyplot(open/maxopen~as.numeric(ISOweek)|delay,
            type="l",lwd=c(3,1,1,1),lty=c(1,1,2,2),col=c(1,2,3,3)))

