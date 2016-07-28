rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("zoo", "tseries")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

load(file = "crix.RData")
Pr  = as.numeric(crix)
Da  = factor(date1)
crx = data.frame(Da, Pr)

# plot of crix
plot(crx$Da, crx$Pr, type = "o", main = "CRIX Daily Price Series from Feb 1st, 2014 to April 12nd, 2015")
plot(crx$Da, crx$Pr, type = "o")
lines(crx$Pr)

# plot of crix return
ret   = diff(log(crx$Pr))
Dare  = factor(date1[-1])
retts = data.frame(Dare, ret)
plot(retts$Dare, retts$ret, type = "o")
lines(retts$ret)

mean(ret)
var(ret)
sd(ret)

# histogram of price
hist(Pr, col = "grey", breaks = 40, freq = FALSE)
lines(density(Pr), lwd = 2)

par(mfrow = c(1, 2))
# histogram of returns
hist(ret, col = "grey", breaks = 20, freq = FALSE, ylim = c(0, 25))
lines(density(ret), lwd = 2)
mu = mean(ret)
sigma = sd(ret)
x = seq(-4, 4, length = 100)
curve(dnorm(x, mean = mean(ret), sd = sd(ret)), add = TRUE, col = "darkblue", 
      lwd = 2)
# qq-plot
qqnorm(ret)
qqline(ret, col = "blue", lwd = 3)


# normality test
ks.test(ret, "pnorm", mean(ret), sd(ret))
shapiro.test(ret)

# d order
Box.test(ret, type = "Ljung-Box", lag = 20)

# stationary test
adf.test(ret, alternative = "stationary")
kpss.test(ret, null = "Trend")

# acf plot
autocorr = acf(ret, lag.max = 20, ylab = "Sample Autocorrelation", main = NA, 
               lwd = 2, ylim = c(-0.3, 1))

# LB test of linear dependence
print(cbind(autocorr$lag, autocorr$acf))
Box.test(ret, type = "Ljung-Box", lag = 1, fitdf = 0)
Box.test(autocorr$acf, type = "Ljung-Box")

# plot of pacf
autopcorr = pacf(ret, lag.max = 20, ylab = "Sample Partial Autocorrelation", 
                 main = NA, ylim = c(-0.3, 0.3), lwd = 2)
print(cbind(autopcorr$lag, autopcorr$acf))
