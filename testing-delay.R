doIt()

crTDF<-delay(rTDF)

with(rTDF,matplot(cbind(w00,w01,w02,w03,w04,w05,w06),type="l",lty=1))
with(rTDF,matplot(cbind(w00,w01,w02,w03,w04,w05,w06)/w06,type="l",lty=1))



with(subset(crTDF,as.character(ISOweek)>"2012-W40"),
     xyplot(wr+cnb+u.cnb+l.cnb~as.numeric(ISOweek)|delay,
                      type="l",lwd=c(3,1,1,1),lty=c(1,1,2,2),col=c(1,2,3,3)))




with(subset(rTDF.long,as.character(ISOweek)>"2012-W40"),
     xyplot(open/maxopen~as.numeric(ISOweek)|delay,
            type="l",lwd=c(3,1,1,1),lty=c(1,1,2,2),col=c(1,2,3,3)))

