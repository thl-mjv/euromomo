
data <- read.csv2("delay.txt")

data2<-addconditions(data,spring=5:26,autumn=30:48)


data3 <- baseline(data2,seasonality=2,spline="l",splinedf=2)

with(data3,matplot(cbind(sin1,sin2),type="l"))

data4 <- zscore(data3)

data5 <- excess(data4)

head(data5)

library("lattice")

xyplot(nb+cnb+pnb+u.pnb+l.pnb~wk,data=data5,type="l")
xyplot(Zscore~wk,data=data5,type="l")
xyplot(excess+u.excess+l.excess~wk,data=data5,type="l")

#### the problem with spines
x1<-1:200
x2<-1:250
library(splines)
matplot(x1,bs(x1),type="l",xlim=c(1,250))
matlines(x2,bs(x2),type="l")
