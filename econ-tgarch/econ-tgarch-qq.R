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

#https://rdrr.io/cran/fGarch/src/R/methods-plot.R
.qqDist <-
  function (y, dist = "qnorm", ylim = NULL, main = paste(dist, "- QQ Plot"),
            xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", doplot = TRUE,
            datax = FALSE, ...)
  {
    # A function implemented by Diethelm Wuertz
    
    # Description
    #   QQ Plot for arbitray distribution
    
    # FUNCTION:
    # print(dist)
    
    # Match Function :
    qDist = match.fun(dist)
    
    # Check Arguments:
    # if (substr(dist, 1, 1) != "q") stop("dist is misspecified")
    # test = class(test = try(qDist(0.5, ...), silent = TRUE))
    # if (test == "try-error") stop("dist does not exist")
    
    # Transform to Vector Mode:
    y = as.vector(y)
    
    # Compute Data:
    if (has.na <- any(ina <- is.na(y))) {
      yN = y
      y = y[!ina]
    }
    if (0 == (n <- length(y))) stop("y is empty or has only NAs")
    x <- qDist(ppoints(n,), ...)[order(order(y))]
    if (has.na) {
      y = x
      x = yN
      x[!ina] = y
      y = yN
    }
    
    # Create QQ Plot:
    if (doplot) {
      if (is.null(ylim)) ylim = range(y)
      if (datax) {
        plot(y, x, main = main, xlab = ylab, ylab = xlab, xlim = ylim,
             col = "steelblue", cex = 0.7)
      } else {
        plot(x, y, main = main, xlab = xlab, ylab = ylab, ylim = ylim,
             col = "steelblue", cex = 0.7)
      }
      .qqLine(y = y, dist = dist, datax = datax, ...)
      #grid()
    }
    
    # Return Value:
    invisible(if (datax) list(x = y, y = x) else list(x = x, y = y))
  }





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

  
