#Indsæt værdier i datax og datay
rm(list=ls())
datax = c(0.0,0.1)
datay = c(0.0,1.0)
data = cbind(datax,datay)
n= length(datax)

xbar = mean(datax)
ybar = mean(datay)
sxx = sum ( (datax-xbar)^2)
sxy = sum ( (datax-xbar)*(datay-ybar))
syy = sum ( (datay-ybar)^2)

beta1 = sxy/sxx

beta0 = ybar - beta1*xbar


fit = function(x){
  y = beta0+beta1*x
  return(y)
}

fittedy = fit(datax)

fittedy

e = datay-fittedy


MSE= 1

R2 = sxy^2/(sxx*syy)


#### Confidensintervaller for fixed ny obs ####
s02 = 1/(n-2)*sum( (datay-fittedy)^2)

confNyiobs= function(x,alpha = 0.05){
  s02 = 1/(n-2)*sum( (datay-fittedy)^2)
  dyt = 1/n+((x-xbar)^2)/sxx
  sd = qt(1-alpha/2,n-2)*sqrt(s02*dyt)
  yhat = fit(x)
  Clow= yhat-sd
  Chigh = yhat +sd
  return(c(Clow,Chigh))
}

xnew=0.4
Cnew=confNyiobs(xnew)
fit(xnew)



#### Plots ####
plot(data)
abline(beta0,beta1,col="red")
points(xnew,fit(xnew),col="blue")
lines(c(xnew,xnew),Cnew )


0.25*0.8+0.25*0.6+0.5*0.25
