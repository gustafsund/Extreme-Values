library(extRemes)
library(in2extRemes)
in2extRemes()

names(portpirie)
portpirie$models$fit1$results
summary(portpirie$models$fit1)
ci(portpirie$models$fit1,type="parameter",which.par=3,xrange=c(-0.5,0.5))
V = unname(solve(portpirie$models$fit1$results$hessian))


P = unname(portpirie$models$fit1$results$par)

y10 = -log(1-(1/10))
del10 = matrix(ncol=1,nrow=3)
del10[1,1] = 1
del10[2,1] = -((P[3])^(-1))*(1-(y10^(-P[3])))
del10[3,1]=((P[2])*((P[3])^(-2))*(1-((y10)^(-P[3]))))-
  ((P[2])*((P[3])^(-1))*((y10)^(-(P[3])))*log(y10))


var10 = t(del10)%*%V%*%del10

y100=-log(1-(1/100))

del100 = matrix(ncol=1,nrow=3)
del100[1,1] = 1
del100[2,1] = -((P[3])^(-1))*(1-(y100^(-P[3])))
del100[3,1]=((P[2])*((P[3])^(-2))*(1-((y100)^(-P[3]))))-
  ((P[2])*((P[3])^(-1))*((y100)^(-(P[3])))*log(y100))


var100 = t(del100)%*%V%*%del100


z10 = P[1] - (P[2]/P[3])*(1-(y10)^(-P[3]))
z100 = P[1] - (P[2]/P[3])*(1-(y100)^(-P[3]))

ci10 = c(z10-1.96*sqrt(var10),z10+1.96*sqrt(var10))
ci100 = c(z100-1.96*sqrt(var100),z100+1.96*sqrt(var100))

z0 = P[1]-P[2]/P[3]
del0 = matrix(ncol=1,nrow=3)
del0[1] = 1
del0[2] = -(P[3]^-1)
del0[3] = P[2]*(P[3]^-2)

var0 = t(del0)%*%V%*%del0
ci0 = c(z0-1.96*sqrt(var0),z0+1.96*sqrt(var0))


ci(portpirie$models$fit2,type="parameter",which.par=1:2,xrange=c(-0.5,0.5))
