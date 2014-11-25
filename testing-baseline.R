loadDefaults(TRUE)

options()$euromomo

data <- read.csv2("delay.txt")

data2<-addconditions(data,spring="warm and pretty",autumn=30:48,delay=1:2)


data3 <- baseline(data2,seasonality=2,spline="n",splinedf=2)

with(data3,as.data.frame(table(Cond3,Cond4,Cond5,Cond6,cond)))

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
