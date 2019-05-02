### BDA Project ###

### load the data ###
### movie death data ###
#### input data ####
# install.packages("RCurl")


library(RCurl)

datafile <- getURL("https://raw.githubusercontent.com/TerrySitu/Bayesian-Data-Analysis-Project-Body-Count-Analysis-of-Quentin-Tarantino-Movies/master/filmdeathcounts.csv")


filmdeathcounts <- read.csv(text = datafile, header=T)

deaths <- filmdeathcounts$Body_Count
hist(deaths, breaks=50)

boxplot(deaths)
summary(deaths)
quantile(deaths, c(0.005, 0.995))

### kill rate in hours in other similar movies ###
# kill.hour <- deaths/(filmdeathcounts$Length_Minutes/60)
# summary(kill.hour)
# table(kill.hour)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


mode.deaths <- Mode(deaths)

#######################################################
### Data in QT's movies (total 8 movies) ###
#######################################################

### QT movies ###

movie <- c("Reservoir Dogs", "Pulp Fiction", "Jackie Brown", "Kill Bill Vol. 1", "Kill Bill Vol. 2", "Death Proof", "Inglorious Bastards", "Django Unchained")

#### body count in QT's movies ###
body <- c(11, 7, 4, 62, 13, 6, 396, 64, 18)

### movie duration in hours ###
time <- c(1.65, 2.567, 2.567, 1.85, 2.283, 1.883, 2.55, 2.75)

### 3 models ###

model.1 <- c(1,1,1,0,1,1,0,0,1)
model.2 <- c(0,0,0,1,0,0,0,1,0)
model.3 <- c(rep(0,6), 1, rep(0,2))

m1.body <- body*model.1
m2.body <- body*model.2
m3.body <- body*model.3

### plot body count vs. time ###
# install.packages("calibrate")
library(calibrate)

plot(time, body, pch=4, col=4, xlim=c(1.4,3))
text(time, body, labels=movie, cex=0.7, pos=1)

### kill rate per hour per moive ####
# kill.rate <- body/time

### plot kill rate per movie ###
# plot(kill.rate, xaxt='n', xlab="", ylab="Kill Rate Per Hour", col='red', pch=13)
# axis(side = 1, at=c(1:8), labels=F)
# text(seq(1,8,by=1), par("usr")[3]-1, labels=movie, srt=-45, pos=1, xpd=T)
# 
# sum(kill.rate)

l0 <- 0
u0 <- 200
p0 <- 0.99
m0 <- 17.55
center0 <- "mode"

f <- function(a, m, l, u, center = "mode", p = 0.99) {
  if (center == "mode") b <- (a - 1) / m else
    if (center == "mean") b <- a / m
    pgamma(u, shape = a, rate = b) -
      pgamma(l, shape = a, rate = b) - p
}
a <- uniroot(f, interval = c(1, 5), m = m0,
             l = l0, u = u0,
             center = center0, p = p0)$root
if (center0 == "mode") b <- (a - 1) / m0 else b <- a / m0

c(a,b)

## create theta ##
m <- 100000
theta <- seq(min(deaths)-1, max(deaths)+1, length=100000) 
length(theta)

## Creat Prior ##
prior <- dgamma(theta, a, b)

## Create Posterior ##
posterior.a <- a+sum(body)
posterior.b <- b+sum(time)

posterior <- dgamma(theta, posterior.a, posterior.b)



plot(theta, posterior, type='l', ylab='Density', xlab=expression(theta), axes=F)
lines(theta, prior, lty=2, type='l', col=2, pch=22)
Axis(side=2)
Axis(side=1)



### theoretical prior vs. obs. prior ###

hist(deaths, breaks=50, freq=F,ylim=c(0,0.020), ann=F)
lines(theta,prior,type='l',col=3, lty=2)
f <- density(deaths)
lines(f$x, f$y, type='l', col=2)
Axis(side=1)
Axis(side=2)

plot(f$x,f$y, type='l')
### plot prior vs. posterior ###

plot(theta, posterior, type='l', col='royalblue')
lines(theta, prior, col='red')

### ###
set.seed(2016)
m <- 100000
n.obs <- length(body)
theta.sim <- rgamma(m, shape = posterior.a, rate = posterior.b)
yrep <- mapply(rpois, n = n.obs, lambda = theta.sim*2.567)

# minimum
obs.min <- min(body)      # observed minimum
sim.min <- apply(yrep, 2, min)   # simulated minimum
n.sim <- length(sim.min)
pval.min <- length(sim.min[sim.min >= obs.min]) / n.sim
hist(sim.min,freq=F,main="Posterior Predictive Checking - Minimum",xlim=c(0,140))
lines(rep(obs.min, 2), c(0, 1.1), col = "red", lwd = 1.5)

# maximum
obs.max <- max(body)      # observed minimum
sim.max <- apply(yrep, 2, max)   # simulated minimum
n.sim <- length(sim.max)
pval.max <- length(sim.max[sim.max >= obs.max]) / n.sim
hist(sim.max,freq=F,main="Posterior Predictive Checking - Max", xlim=c(0,400))
lines(rep(obs.max, 2), c(0, 0.5), col = "red", lwd = 1.5)
