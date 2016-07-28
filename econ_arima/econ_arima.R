rm(list=ls(all=TRUE))
graphics.off()

# please change your working directory
setwd("~/EconCrix/econ_arima")

# install and load packages
libraries = c("zoo", "tseries")
  lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

load(file="crix.RData")
Pr = as.numeric(crix)
Da= factor(date1)
crx = data.frame(Da, Pr)

ret = diff(log(crx$Pr))
Dare = factor(date1[-1])
retts = data.frame(Dare, ret)

#ret = diff(log(crix))

#arima model
par(mfrow=c(1,1))
#auto.arima(ret)
fit1 = arima(ret, order=c(1,0,1))
tsdiag(fit1)
Box.test(fit1$residuals,lag=1)

#aic
aic=matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    a.p.q=arima(ret,order=c(p,0,q))
    aic.p.q=a.p.q$aic
    aic[p+1,q+1]=aic.p.q
  }
}
aic

#bic
bic=matrix(NA,6,6)
for(p in 0:5)
{
  for(q in 0:5)
  {
    b.p.q=arima(ret,order=c(p,0,q))
    bic.p.q=AIC(b.p.q, k=log(length(ret)))
    bic[p+1,q+1]=bic.p.q
  }
}
bic

# select p and q order of ARIMA model 
fit4 = arima(ret, order=c(2,0,3))
tsdiag(fit4)
Box.test(fit4$residuals,lag=1)

fitr4 = arima(ret, order=c(2,1,3))
tsdiag(fitr4)
Box.test(fitr4$residuals,lag=1)

#to conclude, 202 is better than 213
fit202=arima(ret, order=c(2,0,2))
tsdiag(fit202)
tsdiag(fit4)
tsdiag(fitr4)

AIC(fit202, k=log(length(ret)))
AIC(fit4, k=log(length(ret)))
AIC(fitr4, k=log(length(ret)))
fit202$aic
fit4$aic
fitr4$aic

#arima202 predict
fit202 = arima(ret,order=c(2,0,2))
crpre = predict(fit202, n.ahead = 30)
tsret = ts(ret)

plot(retts$Dare, retts$ret, type="o", xlim=c(0,644))
lines(retts$ret)
lines(crpre$pred,col="red",lwd=3)
lines(crpre$pred+2*crpre$se,col="red",lty=3, lwd=3)
lines(crpre$pred-2*crpre$se,col="red",lty=3,lwd=3)

