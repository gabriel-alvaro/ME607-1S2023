## Gabriel Alvaro Batista
## RA 171822

### generates GARCH(1,1) series
garch_generator = function(n, mu, omega, alpha, beta){
  ret = c()
  sigma2 = c()
  epsilon = c()
  
  z = rnorm(n, 0, 1)
  sigma2[1] = omega/(1 - alpha - beta)
  ret[1] = rnorm(1, mu, sigma2[1])
  epsilon[1] = sqrt(sigma2[1]) * z[1]
  
  for(t in 2:n){
    sigma2[t] = omega + alpha * epsilon[t-1] ^ 2 + beta * sigma2[t-1]
    epsilon[t] = sqrt(sigma2[t]) * z[t]
    ret[t] = rnorm(1, mu, sigma2[t])
  }
  
  return (ret)
}

### likelihood function for GARCH(1,1)
garch_likelihood = function(parameters, y){
  omega = parameters[1]
  alpha = parameters[2]
  beta = parameters[3]
  sigma2 = var(y)
  epsilon = y - mean(y)
  loglikelihood = 0
  
  for(t in 2:length(y)){
    sigma2[t] = omega + alpha * (epsilon[t-1] ^ 2) + beta * (sigma2[t-1])
    loglikelihood[t] = (-1/2) * (log(sigma2[t]) + ((epsilon[t] ^ 2)/sigma2[t]))
  }
  
  return (sum(loglikelihood))
}

garch_neg_likelihood = function(parameters, y){
  return (-garch_likelihood(parameters, y))
}

# generating data
y = garch_generator(n = 1000,
                    mu = 1.1,
                    omega = 0.008,
                    alpha = 0.172,
                    beta = 0.763)

initial_params = c(0.2, 0.3, 0.6)

estimated_parameters = optim(par = initial_params,
                             fn = garch_neg_likelihood,
                             y = y,
                             method = "BFGS")$par

## comparing with rugarch library
library(rugarch)
model = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                   mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                   distribution.model = "norm")
setstart(model) = list(omega = 0.2, alpha1 = 0.3, beta1 = 0.6)

fit = ugarchfit(data = y, spec = model, method = "BFGS")

coef(fit)
estimated_parameters
