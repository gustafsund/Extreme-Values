library(extRemes)
library(in2extRemes)
in2extRemes()
time=(fremantle$data$Year-min(fremantle$data$Year))/
  (max(fremantle$data$Year)-min(fremantle$data$Year))
timeSq=time^2
fremantle$data <- cbind(fremantle$data, time=time, timeSq=time^2)
names(fremantle$data)

###### wooster time!
x = 1:length(wooster$data[,2])
usin = function(x, a, b, d)
{
  a + b * sin(((x - d) * 2 * pi)/365.25)
}
wu = usin(x, -30, 25, -75)
winter = c(rep(c(rep(1, 61), rep(0, 273), rep(1, 31)), 5),
           1)
spring = c(rep(c(rep(0, 61), rep(1, 91), rep(0, 365 - 91 -
                                               61)), 5), 0)
summer = c(rep(c(rep(0, 61 + 91), rep(1, 91), rep(0, 365 -
                                                    91 - 61 - 91)), 5), 0)
fall = c(rep(c(rep(0, 61 + 91 + 91), rep(1, 91), rep(0, 365 -
                                                       91 - 61 - 91 - 91)), 5), 0)
rescale.covariate  = function(x)
{
  r.x = range(x)
  x.01 = (x-r.x[1])/diff(r.x)
  2*x.01-1
}
ydat = cbind(wu, sin((x * 2 * pi)/365.25), cos((x * 2 * pi)/365.25
), rescale.covariate(x), winter, spring, summer, fall)

wooster$data[,"value"] <- - wooster$data[,"value"]
wooster$data <- cbind(wooster$data,ydat)
colnames(wooster$data)[3:6] <- c("wu", "sin","cos","time")
wooster$data <- as.data.frame(wooster$data)

