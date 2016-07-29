rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("FinTS", "tseries", "forecast")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load dataset
load(file = "crix.RData")
Pr = as.numeric(crix)
Da = factor(date1)
crx = data.frame(Da, Pr)

# plot of crix return
ret = diff(log(crx$Pr))
Dare = factor(date1[-1])
retts = data.frame(Dare, ret)

# arima202 predict
fit202 = arima(ret, order = c(2, 0, 2))
tsdiag(fit202)

# vol cluster
par(mfrow = c(1, 1))

# arch effect
res = fit202$residuals
ArchTest(res)
Box.test(res2, type = "Ljung-Box")

# ArchTest(ret) arch1 model arch models
crar1 = garch(res, order = c(0, 1))
summary(crar1)
logLik(crar1)
AIC(crar1)
AIC(crar1, k = log(length(res)))

crar2 = garch(res, order = c(0, 2))
summary(crar2)
logLik(crar2)
AIC(crar2)
AIC(crar2, k = log(length(res)))


crar3 = garch(res, order = c(0, 3))
summary(crar3)
logLik(crar3)
AIC(crar3)
AIC(crar3, k = log(length(res)))

crar4 = garch(res, order = c(0, 4))
summary(crar4)
logLik(crar4)
AIC(crar4)
AIC(crar4, k = log(length(res)))