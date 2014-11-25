
data <- read.csv2("delay.txt")

data2<-addconditions(data)


data3 <- baseline(data2)
traceback()

library("lattice")

xyplot(nb+cnb+pnb~wk,data=data3,type="l")
