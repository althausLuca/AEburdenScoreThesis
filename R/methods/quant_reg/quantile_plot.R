rm(list = ls())


#https://cran.r-project.org/web/packages/quantreg/vignettes/rq.pdf

library(quantreg)
data(engel)
attach(engel)
plot(income,foodexp,cex=.25,type="n",xlab="Income", ylab="Food Expenditure" , ylim=c(0,2500))
points(income,foodexp,cex=.5,col="blue")
abline(rq(foodexp~income,tau=.5),col="blue")
abline(lm(foodexp~income),lty=2,col="red") #the dreaded ols line
taus <- c(.05,.1,.25,.75,.90,.95)
for( i in 1:length(taus)){
  abline(rq(foodexp~income,tau=taus[i]),col="gray")
}


foodexp[100] <- 2000
foodexp[101] <- 2200
foodexp[102] <- 2400

plot(income,foodexp,cex=.25,type="n",xlab="Income", ylab="Food Expenditure" , ylim=c(0,2500))
points(income,foodexp,cex=.5,col="blue")
abline(rq(foodexp~income,tau=.5),col="blue")
abline(lm(foodexp~income),lty=2,col="red") #the dreaded ols line
taus <- c(.05,.1,.25,.75,.90,.95)
for( i in 1:length(taus)){
  abline(rq(foodexp~income,tau=taus[i]),col="gray")
}
