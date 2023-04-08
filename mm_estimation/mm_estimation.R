## ME607
## Gabriel Alvaro Batista - RA 171822
## https://github.com/gabriel-alvaro/ME607-1S2023/tree/main/mm_estimation

## AR(P) Parameter Estimation by Method of Moments
mm_estimation = function(x, p){
  # parameters
  mu = mean(x)
  sigma2 = var(x)
  n = length(x)
  rho = acf(x, p, plot = FALSE)$acf[-1]
  
  if (p == 1){ # for AR(1)
    phi = rho
    rho_matrix = rho
  }
  
  if (p > 1){ # for AR(P > 1)
    # creates rho matrix
    rho_matrix = matrix(0, nrow = p, ncol = p)
    for(i in 1:(p-1)){
      rho_matrix[(i+1):p,i] = rho[0:(p-i)]
    }
    rho_matrix[upper.tri(rho_matrix)] <- t(rho_matrix)[upper.tri(rho_matrix)]
    diag(rho_matrix) = 1
    
    # estimates phi
    phi = solve(rho_matrix) %*% rho
  }
  
  # estimates sigma2_epsilon
  sigma2_eps = sigma2*(1-(sum(phi*rho)))
  
  # phi variance
  gamma_matrix = sigma2*rho_matrix
  ACOV = n^{-1} * sigma2_eps * solve(gamma_matrix)
  
  # estimates CI for phi
  phi_CI = matrix(0, nrow = p, ncol = 2)
  for(i in 1:p){
    phi_CI[i,] = c(phi[i] - 1.96*sqrt(ACOV[i,i]), phi[i] + 1.96*sqrt(ACOV[i,i]))
  }
  
  rownames(phi_CI) = paste0("phi", 1:p)
  colnames(phi_CI) = c("Lower CI", "Upper CI")
  
  parameters = matrix(c(mu, sigma2, sigma2_eps, phi), ncol = 1)
  rownames(parameters) = c("mu", "sigma2", "sigma2_eps", paste0("phi", 1:p))
  
  est_params = list("parameters" = parameters,
                    "phi_CI" = phi_CI)
  
  return (est_params)
}

# generating AR(1)
set.seed(171822)
x_1 <- 0.05 + arima.sim(n = 1000, list(ar = 0.5), sd = 2)
mm_estimation(x_1, 1)

# generating AR(2)
set.seed(171822)
x_2 <- arima.sim(n = 1000, list(ar = c(1.5, -0.75)), sd = 1)
mm_estimation(x_2, 2)

# generating AR(6)
set.seed(171822)
x_6 <- arima.sim(n = 1000, list(ar = c(-0.5, -0.3, -0.1,  0.1,  0.3,  0.5)), sd = 1)
mm_estimation(x_6, 6)
