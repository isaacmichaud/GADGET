# compute the pivoted-cholesky predictive errors for Gaussian process validation
#this will form the basis of the diagnostics for GADGET GP fitting
#' Title
#'
#' @param design 
#' @param response 
#' @param model 
#' @param plot 
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
#' # simple iid normal case
#' x        <- matrix(runif(20,-1.5,1.5),ncol=1) 
#'y        <- matrix(rnorm(20),ncol = 1)
#'my_model <- km(formula=~1,design=x,response=y,covtype='matern5_2',optim.method='BFGS',nugget.estim=FALSE)
#'# validation data
#'x        <- matrix(runif(20,-1,1),ncol=1)
#'y        <- matrix(rnorm(20),ncol = 1)
#'plotGP(my_model,c(-2,2),0.1)
#'predictive_errors(matrix(x,ncol=1),matrix(y,ncol=1),my_model)

#'f <- function(x) {
#'  x^2 
#'}
#'#fit model 
#'x        <- matrix(runif(20,-1.5,1.5),ncol=1) 
#'y        <- matrix(f(x),ncol = 1)
#'my_model <- km(formula=~.^2,design=x,response=y,covtype='matern5_2',optim.method='BFGS',nugget.estim=FALSE)
#'#validate model
#'x        <- matrix(runif(20,-1.5,1.5),ncol=1) 
#'y        <- matrix(f(x),ncol = 1)
#'plotGP(my_model,c(-5,5),0.1)
#'predictive_errors(matrix(x,ncol=1),matrix(y,ncol=1),my_model, type = "SK")
#'predictive_errors(matrix(x,ncol=1),matrix(y,ncol=1),my_model, type = "UK")

#'#y = matrix(f(x),ncol = 1)
#this example demonstrates that the model does not fit well on the outside of original design
predictive_errors <- function(design, response, model, plot = TRUE, type = "SK") {
  preds      <- predict(model,design,type=type,cov.compute=TRUE)  
  std_preds  <- (response - preds$mean)/sqrt(diag(preds$cov))
  #these only work in one dimension
  #plot(design,std_preds,pch=20, ylab="standardized residual")
  #title(main = "Standardized Residuals")
  
  
  D_MD = t(response - preds$mean)%*%solve(preds$cov)%*% (response - preds$mean) # Mahalanobis distance
  
  n = model@n
  q = 0 #mean parameters (0 if SK)
  m = nrow(design) # of validation 
  F_stat <- D_MD * (n-q)/(m*(n-q-2))
  print(c(F_stat,qf(0.025,m,n-q),qf(0.975,m,n-q)))
  
  #pivoted cholesky residuals
  pc <- chol(preds$cov,pivot = TRUE)
  D_pc <- solve(t(pc))%*%(response - preds$mean)
  pivots <- attr(pc,"pivot")
  
  if (plot == TRUE) {
    par(mfrow=c(2,2))
    
    #plot standardized residuals
    ind    <- (abs(std_preds) > 2)
    yrange <- 1.2*diff(range(std_preds))
    ylims  <- median(std_preds) + yrange*c(-0.5,0.5)
    plot(preds$mean[!ind],
         std_preds[!ind], 
         ylim=ylims, 
         ylab = "Std Residual",
         xlab = "Prediction")
    for (i in 1:m) {
      if(ind[i]) {
        text(preds$mean[i],
             std_preds[i],
             label=i,
             col='blue')
      }
    }
    abline(2,0,lty=2,col='red')
    abline(-2,0,lty=2,col='red')
    title(main = "Std Residuals")
    
    #plot QQ-normal standardized residuals
    qqnorm(std_preds, main = "QQ-plot Std Residuals")
    abline(0,1,lty=2,col='red')
    
    #plot pivoted cholesky residuals (uncorrelated)
    ind    <- (abs(D_pc) > 2)
    yrange <- 1.2*diff(range(D_pc))
    ylims  <- median(D_pc) + yrange*c(-0.5,0.5)
    index  <- 1:m
    plot(index[!ind],
         D_pc[!ind], 
         ylim=ylims, 
         xlim = c(0,m+2), 
         xlab='Pivot Index',
         ylab = "PC Residual")
    for (i in 1:m) {
      if(ind[i]) {
        text(index[i],D_pc[i],label=pivots[i],col='blue')
      }
    }
    abline(2,0,lty=2,col='red')
    abline(-2,0,lty=2,col='red')
    title(main = "PC Residuals")
    
    #plot QQ-normal standardized residuals
    qqnorm(D_pc, main = "QQ-plot PC Residuals")
    abline(0,1,lty=2,col='red')
    par(mfrow=c(1,1))
  }
  return(list(MD = D_MD, F_stat = c(F_stat,qf(0.025,m,n-q),qf(0.975,m,n-q)),D_pc = data.frame(error =  D_pc, pivot = pivots)))
}

#' Automated Gaussian Process Validation
#' 
#' Automatically validates a Gaussian process (GP) using a seperate validation dataset not used in the fitting of the GP. The Bastos and O'Hagan (2009) empirical fit statistics are used to determine if the GP accurately predicts the validation data. It then determines roughly whether the model fits well enough.  
#'
#' @param design validation design
#' @param response validation response 
#' @param model GP model from DiceKriging (class `km``)
#' @param type simple of universal kriging
#' @param verbose logical, print the resulting statistics and possible explainations for the GP's deficiency
#'
#' @references 
#' Bastos, L. S., & O’Hagan, A. (2009). Diagnostics for gaussian process emulators. Technometrics, 51(4), 425–438. https://doi.org/10.1198/TECH.2009.08019
#'
#' @return logical, TRUE the GP is a valid emulator, FALSE the GP is an invalid emulator
#' @export
#'
#' @examples 
#' #--- simple iid normal example (assumptions hold) ---#
#' # training data
#' x   <- matrix(runif(20,0,1),ncol=1) 
#' y   <- matrix(rnorm(20),ncol = 1)
#' # validation data
#' v_x <- matrix(runif(20,-1,1),ncol=1)
#' v_y <- matrix(rnorm(20),ncol = 1)
#' my_model <- km(formula=~1,design=x,response=y,covtype='matern5_2',optim.method='BFGS',nugget.estim=FALSE)
#' plot_gp(my_model,c(-0.1,1.1),0.01)
#' validate_gp(v_x,v_y,my_model,verbose = TRUE)
#' 
#' #--- Bastos and O'Hagan 2009 Two-Input Toy Example ---# 
#' set.seed(123)
#' # training data
#' x   <- lhs::randomLHS(20,2)
#' y   <- space_eval(x,bo09)
#' # validation data
#' v_x <- lhs::randomLHS(20,2)
#' v_y <- space_eval(v_x,bo09)
#' validate_gp(v_x,v_y,my_model,verbose = TRUE)

validate_gp <- function(design, response, model, type = "SK", verbose = FALSE) {
  #don't know how to do this exactly
  #easiest way is to just check the MD statistic
  #split the validation indices into the first half and the second half
  #if we have the extreme in either of the sections then we can report the request information...
  
}
  
  
  



