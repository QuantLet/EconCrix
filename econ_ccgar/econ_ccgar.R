rm(list=ls(all=TRUE))
graphics.off()

#require(forecast)
#require(FinTS)
# please change your working directory
setwd("~/Dropbox/OP_CRIX/codes")

load(file="crix.RData")
#Pr = as.numeric(crix)
#Da= factor(date1)
#crx = data.frame(Da, Pr)

#plot of crix return
#ret = diff(log(crx$Pr))
#Dare = factor(date1[-1])
#retts = data.frame(Dare, ret)
#plot(retts$Dare, retts$ret, type="o")
#lines(retts$ret)

#fit arima model
#fit202 = arima(ret, order=c(2,0,2))
#res = fit202$residuals

# fits an EGARCH model with t distributed errors
#load(file = "btc_index.csv")
load(file = "ecrix.RData")
load(file = "efcrix.RData")
cret = diff(log(crix))
ecret = diff(log(ecrix))
efcret = diff(log(efcrix))
require(ccgarch)
require(rmgarch)
require(xts)
require(zoo)
require(tseries)
require(fGarch)

#arima fitting for each index
cr.arfit = arima(cret, order=c(2,0,2))
tsdiag(cr.arfit)
cres = cr.arfit$residuals

ecr.arfit = arima(ecret, order=c(2,0,2))
tsdiag(ecr.arfit)
ecres = ecr.arfit$residuals

efcr.arfit = arima(efcret, order=c(2,0,2))
tsdiag(cr.arfit)
efcres = efcr.arfit$residuals


garch11.spec = ugarchspec(mean.model = list(armaOrder = c(2,2)),
                          variance.model = list(garchOrder = c(1,1),model = "sGARCH"),
                          distribution.model = "std")
dcc.garch11.spec = dccspec(uspec = multispec( replicate(2, garch11.spec) ), 
                           dccOrder = c(1,1), distribution = "mvnorm")
dcc.garch11.spec

######crix and ecrix
#zcret = as.xts(cret)
sample1 = cbind(as.xts(cres),as.xts(ecres))
sample11 = cbind(as.xts(cret),as.xts(ecret))
dcc.fit1 = dccfit(dcc.garch11.spec, data = sample11)
class(dcc.fit1)
names(dcc.fit@mfit1)
dcc.fit1
plot(dcc.fit1)
# 100-step ahead forecasts of conditional covariances and conditional correlations
dcc.fcst1 = dccforecast(dcc.fit1, n.ahead=100)
class(dcc.fcst1)
dcc.fcst1
plot(dcc.fcst1)


######crix and efcrix
sample2 = cbind(as.xts(cret),as.xts(efcret))
dcc.fit2 = dccfit(dcc.garch11.spec, data = sample2)
dcc.fit2
plot(dcc.fit2)
# 100-step ahead forecasts of conditional covariances and conditional correlations
dcc.fcst2 = dccforecast(dcc.fit2, n.ahead=100)
plot(dcc.fcst2)

######ecrix and efcrix
sample3 = cbind(as.xts(ecret),as.xts(efcret))
dcc.fit3 = dccfit(dcc.garch11.spec, data = sample3)
dcc.fit3
plot(dcc.fit3)
# 100-step ahead forecasts of conditional covariances and conditional correlations
dcc.fcst3 = dccforecast(dcc.fit3, n.ahead=100)
plot(dcc.fcst3)

par(mfrow=c(3,1))
plot(dcc.fit1)
plot(dcc.fit2)
plot(dcc.fit3)



par(mfrow=c(1,1))

######crix, ecrix and efcrix
sampleall = cbind(as.xts(cret), as.xts(ecret),as.xts(efcret))
garch11.spec = ugarchspec(mean.model = list(armaOrder = c(2,2)),
                          variance.model = list(garchOrder = c(2,2),model = "sGARCH"),
                          distribution.model = "std")

dcc.garch11.spec = dccspec(uspec = multispec( replicate(3, garch11.spec) ), 
                           #                          dccOrder = c(1,1), distribution = "mvnorm")
                           dccOrder = c(1,1), distribution = "mvt")
#                           dccOrder = c(1,1), distribution = "mvlaplace")
dcc.fitall = dccfit(dcc.garch11.spec, data = sampleall)
dcc.fitall
plot(dcc.fitall, auto.grid = FALSE)

plot(dcc.fitall, pair = c(1,2), which =4)
plot(dcc.fitall, pair = c(1,3), which =4)

nisurface(dcc.fitall, pair=c(1,2), plot=T)
nisurface(dcc.fitall, pair=c(1,3))
nisurface(dcc.fitall, pair=c(2,3))

#DCC conditional covariance
par(mfrow=c(3,1))
rcovr = rcov(dcc.fitall)
plot(rcovr[1,2,], type ="l", lwd=2, ylab=NA, main="crix v.s. ecrix")
plot(rcovr[1,3,], type ="l", lwd=2, ylab=NA, main="ecrix v.s. ecrix")
plot(rcovr[2,3,], type ="l", lwd=2, ylab=NA, main="crix v.s. ecrix")

rcovr1=data.frame(rcovr[1,2,], names(rcovr[1,2,]))
plot(rcovr1$names.rcovr.1..2...., rcovr1$rcovr.1..2..., type="o")
lines(rcovr1$rcovr.1..2...)
rcovr2=data.frame(rcovr[1,3,], names(rcovr[1,3,]))
plot(rcovr2$names.rcovr.1..3...., rcovr2$rcovr.1..3..., type="o")
lines(rcovr2$rcovr.1..3...)
rcovr3=data.frame(rcovr[2,3,], names(rcovr[2,3,]))
plot(rcovr3$names.rcovr.2..3...., rcovr3$rcovr.2..3..., type="o")
lines(rcovr3$rcovr.2..3...)

#DCC conditional correlation plot
rcorr = rcor(dcc.fitall)
#rcorr1 = zoo(rcorr[1,2, ], order.by = names(rcorr[1,2,]))
plot(rcorr[1,2,],type ="l", lwd=2, ylab=NA)
plot(rcorr[1,3,], type ="l", lwd=2, ylab=NA)
plot(rcorr[2,3,], type ="l", lwd=2, ylab=NA)

rcorr1=data.frame(rcorr[1,2,], names(rcorr[1,2,]))
plot(rcorr1$names.rcorr.1..2...., rcorr1$rcorr.1..2..., type="o")
lines(rcorr1$rcorr.1..2...)
rcorr2=data.frame(rcorr[1,3,], names(rcorr[1,3,]))
plot(rcorr2$names.rcorr.1..3...., rcorr2$rcorr.1..3..., type="o")
lines(rcorr2$rcorr.1..3...)
rcorr3=data.frame(rcorr[2,3,], names(rcorr[2,3,]))
plot(rcorr3$names.rcorr.2..3...., rcorr3$rcorr.2..3..., type="o")
lines(rcorr3$rcorr.2..3...)

# 100-step ahead forecasts of conditional covariances and conditional correlations
dcc.fcstall = dccforecast(dcc.fitall, n.ahead=100)
plot(dcc.fcstall, pairs(1,3))



#three indices return 
plot(as.xts(cret), type="l")
lines(as.xts(ecret), col="blue")
lines(as.xts(efcret), col="red")

plot(as.xts(crix), type="l", main="Indices in CRIX family", auto.grid = FALSE)
lines(as.xts(ecrix), col="blue")
lines(as.xts(efcrix), col="red")

##standard residual plot
par(mfrow=c(1,1))
res2 = dcc.fitall@mfit$stdresid

res2[which.min(res2[,1]),1]=0
plot(res2[,1], ylim=c(-7,7))





plot(res2[,1], ylim=c(-7,7), type="l", lwd=2)
lines(res2[,2], col="red")
lines(res2[,3], col="blue")
acfdccall = acf(res2, ylab=NA, lag.max = 20, main="ACF of Residuals", lwd = 2)
pacfdccall = pacf(res2, lag.max = 20, main="PACF of Residuals", lwd = 2, ylab=NA)

##qq
set.seed(100)
x = rnorm(100)
xx = rt(100)
qqplot(c(res2[,1]), xx, col="blue", lwd=2, ylim=c(-4,4), xlim=c(-4,4))
qqline(c(res2[,1]))

res22 = (dcc.fitall@mfit$stdresid)^2
acfdccall = acf(res22, ylab=NA, lag.max = 20, main="ACF of Squared Residuals", lwd = 2)
pacfdccall = pacf(res22, lag.max = 20, main="PACF of Squared Residuals", lwd = 2, ylab=NA)
