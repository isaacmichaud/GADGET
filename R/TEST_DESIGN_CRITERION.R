d_optimal_regression <- function(eta) {
  sizes    <- dim(eta)
  info_mat <- matrix(0,ncol = (sizes[2]-1) , nrow = sizes[2]-1) 
  for(i in 1:sizes[1]) {
    browser()
    temp_vec = t(as.matrix(c(1,eta[i,-1])))
    info_mat = info_mat + eta[i,1]*(temp_vec)%*%t(temp_vec)
  }
  return(-log(det(info_mat)))  
} 

x = seq(0,0.5,0.001)
eta = data.frame(weights = c(0.5,0.5),x1 = c(1,1),x2 = c(0.5,1))
print(d_optimal_regression(eta))


optim_wrapper <- function(x) {
  eta = data.frame(weights = c(x[1],1-x[1]),x1 = c(1,1),x2 = x[2:3])
  return(d_optimal_regression(eta))
}

#library(pso)
#
#psoptim(runif(3),optim_wrapper,lower=0,upper = 1)


result = design_experiment(d_optimal_regression, lower_bound = c(0),upper_bound = c(1),batch = 2)
