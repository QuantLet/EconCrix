rm(list=ls(all=TRUE))
graphics.off()

# install and load packages
libraries = c("FinTS", "tseries", "forecast")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# please change your working directory
setwd()

load(file="crix.RData")
Pr = as.numeric(crix)
Da= factor(date1)
crx = data.frame(Da, Pr)

#plot of crix return
ret = diff(log(crx$Pr))
Dare = factor(date1[-1])
retts = data.frame(Dare, ret)

######arima202 predict
fit202 = arima(ret, order=c(2,0,2))
tsdiag(fit202)

#vol cluster
par(mfrow=c(1,1))
res = fit202$residuals
res2 = fit202$residuals^2

#different package yields different result.
require(fGarch)
fg11 = garchFit(data=res, data ~ garch(1, 1))
summary(fg11)
fg12 = garchFit(data=res, data ~ garch(1, 2))
summary(fg12)
fg21 = garchFit(data=res, data ~ garch(2, 1))
summary(fg21)
fg22 = garchFit(data=res, data ~ garch(2, 2))
summary(fg22)
#res
timefg11 = data.frame(fg11@residuals, Dare)
par(mfrow=c(1,1))
plot(timefg11$Dare, timefg11$fg11.residuals, type="o")
lines(timefg11$fg11.residuals)

#fg11 = garchFit(res ~ garch(1,1))
par(mfrow=c(1,2))
#plot(fg11)#1,2,9,10,11,13#
#plot(fg11, which = 11)
#plot(fg12, which = 11)

fg11res2 = fg11@residuals
acfres2 = acf(fg11res2, lag.max = 20, ylab = "Sample Autocorrelation", main=NA, lwd = 2)
pacfres2 = pacf(fg11res2, lag.max = 20, ylab = "Sample Partial Autocorrelation", main=NA, lwd = 2, ylim=c(-0.5,0.5))

fg12res2 = fg12@residuals
acfres2 = acf(fg12res2, lag.max = 20, ylab = "Sample Autocorrelation", main=NA, lwd = 2)
pacfres2 = pacf(fg12res2, lag.max = 20, ylab = "Sample Partial Autocorrelation", main=NA, lwd = 2,ylim=c(-0.5,0.5))
#qq plot
par(mfrow=c(1,1))
plot(fg11, which=13)#9,10,11,13

#kp test
set.seed(100)
x <- rnorm(200)

# Do x and y come from the same distribution?
ks.test(x, fg11@residuals)

fg11stu = garchFit(data=res, data ~ garch(1, 1), cond.dist="std")
fg11sstu = garchFit(data=res, data ~ garch(1, 1), cond.dist="sstd")

##different forecast with t-garch 
par(mfrow=c(1,1))
fg11stufore = predict(fg11stu, n.ahead = 30, plot=TRUE, mse="uncond", auto.grid=FALSE)
fg11stufore = predict(fg11stu, n.ahead = 30, plot=TRUE, crit_val=2)
fg11stufore = predict(fg11stu, n.ahead = 30, plot=TRUE, cond.dist="QMLE")
fg11stufore = predict(fg11stu, n.ahead = 30, plot=TRUE)

fg11stupred = predict(fg11stu)
plot(res, type="l", xlab="Time", lwd=2, ylab=NA)
lines(fg11stupred[,1], col="blue", lty="dashed", lwd=2)
lines(fg11stupred[,2], col="blue", lty="dashed", lwd=2)

# forecast with skewed-t-garch
fg11sstufore = predict(fg11sstu, n.ahead = 100, plot=TRUE)

par(mfrow=c(1,2))
#plot(fg11)#1,2,9,10,11,13
stu.fg11res2 = fg11stu@residuals
sstu.fg11res2 = fg11sstu@residuals
#acf and pacf for t-garch
stu.acfres2 = acf(stu.fg11res2, ylab=NA, lag.max = 20, main="ACF of Squared Residuals", lwd = 2)
stu.pacfres2 = pacf(stu.fg11res2, lag.max = 20, main="PACF of Squared Residuals", lwd = 2, ylab=NA, ylim=c(-0.5,0.5))
#acf and pacf for skewed-t-garch
sstu.acfres2 = acf(sstu.fg11res2, ylab=NA, lag.max = 20, main="ACF of Squared Residuals", lwd = 2)
sstu.pacfres2 = pacf(sstu.fg11res2, lag.max = 20, main="PACF of Squared Residuals", lwd = 2, ylab=NA, ylim=c(-0.5,0.5))

# ARIMA-t-GARCH qq plot
par(mfrow=c(1,1))
plot(fg11stu, which=13)
