library(extRemes)
library(in2extRemes)
in2extRemes()

u0 = 30
names(rain)
fevd(x=value,data = rain$data, threshold = u0, type = "GP")
cov_mat = solve(rain$models$fit1$results$hessian)

snabel_hat = length(which(rain$data[,2]>u0))/length(rain$data[,2])

P = unname(rain$models$fit1$results$par);
sigma_hat = P[1];
xi_hat = P[2];

n = length(rain$data[,2])



x10 = u0 + (sigma_hat/xi_hat)*((10*365*snabel_hat)^xi_hat -1)
x100 = u0 + (sigma_hat/xi_hat)*((100*365*snabel_hat)^xi_hat -1)

#V = matrix(c(snabel_hat*(1-snabel_hat)/n, 0,0,0,
#             0.9187666,-0.06550540,0,-0.0655054,0.01024179),nrow=3,ncol=3,byrow=TRUE)
V = matrix(ncol=3,nrow=3)
V[1,1] = snabel_hat*(1-snabel_hat)/n
V[2,2] = cov_mat[1,1]
V[2,3] = cov_mat[1,2]
V[3,2] = cov_mat[2,1]
V[3,3] = cov_mat[2,2]
V[1,2] = V[1,3]=V[2,1]=V[3,1] = 0


del=matrix(ncol=1,nrow=3)


m = 10*365;
m = 100*365;

del[1,1] = sigma_hat*m^(xi_hat)*snabel_hat^(xi_hat-1)
del[2,1] = xi_hat^(-1)*((m*snabel_hat)^xi_hat-1)
del[3,1] = -sigma_hat*xi_hat^(-2)*((m*snabel_hat)^xi_hat-1)+
  sigma_hat*xi_hat^(-1)*(m*snabel_hat)^xi_hat*log(m*snabel_hat)
del.transpose=t(del)
var_x_m = del.transpose%*%V%*%del

c(x10 - 1.96*sqrt(var_x_m),x10 + 1.96*sqrt(x_m))
c(x100 - 1.96*sqrt(var_x_m),x100 + 1.96*sqrt(x_m))





# fixa del

y=rain$models$fit1$x[rain$models$fit1$x > u0]-u0
sigma_tilde = mean(y)
ny = length(y)
d = sd(y)/sqrt(ny)

ci_15 <-c(sigma_tilde-1.96*d,sigma_tilde+1.96*d)

fevd(x=value,data = rain$data, threshold = u0, type = "Exponential")
neg_log_exp = 487.3937
neg_log_GP = 485.0937

(L_ratio = -2*(neg_log_GP - neg_log_exp))
df <- 1
(chi_quant = qchisq(0.95,df))

# Reject H0????? OR NOT Probably reject
probs = seq(1,length(y),length.out = ny) / ( length(y)+1 )
  quantiles = qexp(probs,rate=sigma_tilde)

plot(sort(y), quantiles)
#lines(sort(y),sort(y)*max(quantiles)/max(y))

empiric = sort(y) /  (length(y)+1)


(ci(rain$models$fit1, type = "parameter", method = "proflik", 
   which.par = 1, xrange = c(-0.5,15),nint=100))
(ci(rain$models$fit1, type = "parameter", method = "proflik", 
    which.par = 2, xrange = c(-0.5,1),nint=100))






