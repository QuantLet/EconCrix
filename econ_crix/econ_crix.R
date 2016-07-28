rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("zoo", "tseries", "xts")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load dataset
load(file = "crix.RData")

# plot of crix
plot(as.xts(crix), type="l", auto.grid=FALSE, main = NA)

# plot of crix return
ret   = diff(log(crix))
plot(as.xts(ret), type="l", auto.grid=FALSE, main = NA)

mean(ret)
var(ret)
sd(ret)

# histogram of price
hist(crix, col = "grey", breaks = 40, freq = FALSE)
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
