rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c( "tseries", "forecast", "fGarch", "bsts")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# please change your working directory
setwd()

# load dataset
load(file = "crix.RData")
ret = diff(log(crix))
fit202 = arima(ret, order = c(2, 0, 2))
par(mfrow = c(1, 1))
res = fit202$residuals

fg11stu = garchFit(data = res, data ~ garch(1, 1), cond.dist = "std")

# different forecast with t-garch 
# fg11stufore = predict(fg11stu, n.ahead = 30, plot=TRUE, mse='uncond', auto.grid=FALSE)
fg11stufore = predict(fg11stu, n.ahead = 30, plot = TRUE, cond.dist = "QMLE", 
                      auto.grid = FALSE)

par(mfrow = c(1, 2))
stu.fg11res2 = fg11stu@residuals

# acf and pacf for t-garch
stu.acfres2 = acf(stu.fg11res2, ylab = NA, lag.max = 20, main = "ACF of Squared Residuals", 
                  lwd = 2)
stu.pacfres2 = pacf(stu.fg11res2, lag.max = 20, main = "PACF of Squared Residuals", 
                    lwd = 2, ylab = NA, ylim = c(-0.5, 0.5))

# ARIMA-t-GARCH qq plot
par(mfrow = c(1, 1))
plot(fg11stu, which = 13)

### fGarch:::.plot.garch.13
library(bsts)
aa=function (x, ...) 
{
  sres = residuals(x, standardize = TRUE)
  cond.dist = x@fit$params$cond.dist
  cond.dist = paste("q", cond.dist, sep = "")
  nc = nchar(x@fit$params$cond.dist)
  parNames <- names(x@fit$par)
  skew <- if ("skew" %in% parNames) 
    x@fit$par["skew"]
  else x@fit$params$skew
  shape <- if ("shape" %in% parNames) 
    x@fit$par["shape"]
  else x@fit$params$shape
  if (cond.dist == "qnorm" || cond.dist == "qQMLE") 
    .qqDist(sres, dist = "qnorm")
  if (cond.dist == "qstd" | cond.dist == "qged") 
    .qqDist(sres, dist = cond.dist, nu = shape,ylim=c(-6.7,6.7))
  if (cond.dist == "qsnorm") 
    .qqDist(sres, dist = cond.dist, xi = skew)
  if (cond.dist == "qsstd" | cond.dist == "qsged") 
    .qqDist(sres, dist = cond.dist, xi = skew, nu = shape)
  if (cond.dist == "qsnig") 
    .qqDist(sres, dist = ".qsnigC", rho = skew, zeta = shape)
}

aa(x = fg11stu)

  
