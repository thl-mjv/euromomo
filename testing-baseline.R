loadDefaults(TRUE)

options()$euromomo

data <- read.csv2("delay.txt")


data2<-addconditions(crTDF4,spring=15:26,autumn=30:48,delay=1:2)
summary(data2)

data3 <- baseline(data2,spline="n",splinedf=2)

head(data3)
with(data3,as.data.frame(table(CondSeason,CondSomething,CondDelays,CondLength,cond)))

with(data3,matplot(cbind(sin1,sin2),type="l"))

data4 <- zscore(data3)

data5 <- excess(data4,type="bo")

tail(data5)

library("lattice")

xyplot(nb+cnb+pnb+u.pnb+l.pnb+u.cnb+l.cnb~wk,data=data5,subset=wk>300,
       type="l",ylim=c(0,2000),lty=c(1,1,1,2,2,2,2),lwd=c(2,2,1,1,1,1,1),
       col=c(1,2,3,3,3,2,2))
xyplot(Zscore~wk,data=data5,type="l")
xyplot(excess+u.excess+l.excess~wk,data=data5,type="l")

#### the problem with spines
x1<-1:200
x2<-1:250
library(splines)
matplot(x1,bs(x1),type="l",xlim=c(1,250))
matlines(x2,bs(x2),type="l")
