#' Gaussian Process Residuals
#'
#'This function computes standardized and pivoted-Cholesky residuals of a Gaussian process (GP) model on a validation data set.
#'Mahalanobis distance and Mahalanobis p-value are calculated.
#'These statistics provide evidence of lack-of-fit in the GP model.
#'The residuals can be plotted against predicted values as well as QQ-plots to check the normality assumption.
#'
#this will form the basis of the diagnostics for GADGET GP fitting -> need to put more details about interpretation
#'
#' @param design  A matrix of \code{n} rows and \code{d} columns.
#' @param response A column vector of length \code{n}.
#' @param model A GP model of class \code{km} (see \code{\link[DiceKriging]{km-class}}).
#' @param plot Plot residuals and QQ-plots (with outliers are highlighted)?
#' @param type Kriging type: Simple Kriging "SK" or Universal Kriging "UK".
#' @references
#' Bastos, L. S., & O'Hagan, A. (2009). Diagnostics for gaussian process emulators. Technometrics, 51(4), 425–438, <doi:10.1198/TECH.2009.08019>.
#' @return A list including the Mahalanobis distance (MD), MD F-statistic, MD p-value, pivoted-Cholesky residuals, and standardized residuals.
#' @export
#'
#' @examples
#' #--- Simple iid Normal Example ---#
#' #model assumputions hold
#' set.seed(123)
#' # training data
#' x           <- matrix(runif(20,-1.5,1.5),ncol=1)
#' y           <- matrix(rnorm(20),ncol = 1)
#' my_model    <- DiceKriging::km(formula=~1,
#'                                design=x,
#'                                response=y,
#'                                covtype='matern5_2',
#'                                optim.method='BFGS',
#'                                nugget.estim=TRUE)
#' # validation data
#' v_x         <- matrix(runif(25,-1,1),ncol=1)
#' v_y         <- matrix(rnorm(25),ncol = 1)
#' diagnostics <-gp_residuals(design = v_x, response = v_y,my_model)
#'
#'#--- Bastos and O'Hagan (2009) Two-input Toy Model ---#
#' # needs more than 20 training points
#' set.seed(123)
#' # training data
#' x   <- lhs::randomLHS(20,2)
#' y   <- space_eval(x,bo09_toy)
#' # validation data
#' v_x <- lhs::randomLHS(25,2)
#' v_y <- space_eval(v_x,bo09_toy)
#' my_model    <- DiceKriging::km(formula=~1,
#'                                design=x,
#'                                response=y,
#'                                covtype='matern5_2',
#'                                optim.method='BFGS',
#'                                nugget.estim=TRUE)
#' diagnostics <- gp_residuals(v_x,v_y,my_model)
gp_residuals <- function(design, response, model, plot = TRUE, type = "SK") {
  colnames(design) <- colnames(model@X)
  #design           <- data.frame(design = design) #get names correct
  #correlated residuals
  preds     <- stats::predict(model,design,type=type,cov.compute=TRUE)
  std_preds <- (response - preds$mean)/sqrt(diag(preds$cov))

  #mahalanobis distance
  D_MD      <- t(response - preds$mean)%*%solve(preds$cov)%*% (response - preds$mean)
  n         <- model@n      #training sample size
  q         <- model@p      #number mean process paramers
  m         <- nrow(design) #validation sample size
  F_stat    <- D_MD * (n-q)/(m*(n-q-2))
  F_pvalue  <- 2*min(stats::pf(F_stat,m,n-q),1 - stats::pf(F_stat,m,n-q))
  #c(F_stat,qf(0.025,m,n-q),qf(0.975,m,n-q))

  #pivoted cholesky residuals
  pc        <- chol(preds$cov,pivot = TRUE)
  D_pc      <- solve(t(pc))%*%(response - preds$mean)
  pivots    <- attr(pc,"pivot")

  if (plot == TRUE) {
    
    oldpar <-graphics::par(mfrow=c(2,2))
    on.exit(graphics::par(oldpar))
    
    #plot standardized residuals
    ind    <- (abs(std_preds) > 2)
    yrange <- 1.2*diff(range(std_preds))
    ylims  <- stats::median(std_preds) + yrange*c(-0.5,0.5)
    xlims  <- range(preds$mean)
    graphics::plot(preds$mean[!ind],
         std_preds[!ind],
         ylim = ylims,
         xlim = xlims,
         ylab = "Std Residual",
         xlab = "Prediction")
    for (i in 1:m) {
      if(ind[i]) {
        graphics::text(preds$mean[i],
             std_preds[i],
             label=i,
             col='blue')
      }
    }
    graphics::abline(2,0,lty=2,col='red')
    graphics::abline(-2,0,lty=2,col='red')
    graphics::title(main = "Std Residuals")

    #plot QQ-normal standardized residuals
    stats::qqnorm(std_preds, main = "QQ-plot Std Residuals")
    graphics::abline(0,1,lty=2,col='red')

    #plot pivoted cholesky residuals (uncorrelated)
    ind    <- (abs(D_pc) > 2)
    yrange <- 1.2*diff(range(D_pc))
    ylims  <- stats::median(D_pc) + yrange*c(-0.5,0.5)
    index  <- 1:m
    graphics::plot(index[!ind],
         D_pc[!ind],
         ylim=ylims,
         xlim = c(0,m+2),
         xlab='Pivot Index',
         ylab = "PC Residual")
    for (i in 1:m) {
      if(ind[i]) {
        graphics::text(index[i],D_pc[i],label=pivots[i],col='blue')
      }
    }
    graphics::abline(2,0,lty=2,col='red')
    graphics::abline(-2,0,lty=2,col='red')
    graphics::title(main = "PC Residuals")

    #plot QQ-normal standardized residuals
    stats::qqnorm(D_pc, main = "QQ-plot PC Residuals")
    graphics::abline(0,1,lty=2,col='red')
    
  }

  return(list(MD = D_MD,
              F_stat = F_stat,
              F_pvalue = F_pvalue,
              D_pc = data.frame(residual =  D_pc, pivot = pivots),
              D_I = std_preds))
}

#' Automated Gaussian Process Validation
#'
#' Automatically validates a Gaussian process (GP) using a separate validation dataset not used in the fitting of the GP.
#' The Bastos and O'Hagan (2009) empirical fit statistics are used to determine if the GP accurately predicts the validation data.
#' It then determines roughly whether the model fits well enough.
#' Currently, it uses the Mahalanobis distance (MD) p-value to determine the GP fit.
#'
#' @param design  A matrix of \code{n} rows and \code{d} columns.
#' @param response A column vector of length \code{n}.
#' @param model A GP model of class \code{km} (see \code{\link[DiceKriging]{km-class}}).
#' @param type Kriging type: Simple Kriging "SK" or Universal Kriging "UK".
#' @param verbose Print the conclusion of the validation?
#'
#' @references
#' Bastos, L. S., & O'Hagan, A. (2009). Diagnostics for gaussian process emulators. Technometrics, 51(4), 425–438, <doi:10.1198/TECH.2009.08019>.
#'
#' @return A logical. If \code{TRUE} the GP is considered a valid emulator, otherwise further training data will need to be collected to improve the emulator fit.
#' @export
#'
#' @examples
#' #--- Simple iid Normal Example ---#
#' # model assumputions hold
#' set.seed(123)
#' # training data
#' x   <- matrix(runif(20,0,1),ncol=1)
#' y   <- matrix(rnorm(20),ncol = 1)
#' # validation data
#' v_x <- matrix(runif(20,-1,1),ncol=1)
#' v_y <- matrix(rnorm(20),ncol = 1)
#' my_model    <- DiceKriging::km(formula=~1,
#'                                design=x,
#'                                response=y,
#'                                covtype='matern5_2',
#'                                optim.method='BFGS',
#'                                nugget.estim=TRUE)
#' gp_validate(v_x,v_y,my_model,verbose = TRUE)
#'
#'#--- Bastos and O'Hagan (2009) Two-input Toy Model ---#
#' # needs more than 20 training points
#' set.seed(123)
#' # training data
#' x        <- lhs::randomLHS(20,2)
#' y        <- space_eval(x,bo09_toy)
#' # validation data
#' v_x      <- lhs::randomLHS(25,2)
#' v_y      <- space_eval(v_x,bo09_toy)
#' my_model    <- DiceKriging::km(formula=~1,
#'                                design=x,
#'                                response=y,
#'                                covtype='matern5_2',
#'                                optim.method='BFGS',
#'                                nugget.estim=TRUE)
#' gp_validate(v_x,v_y,my_model,verbose = TRUE)
gp_validate <- function(design, response, model, type = "SK", verbose = FALSE) {
  res <- gp_residuals(design, response, model, plot = verbose, type = "SK")
  if (res$F_pvalue < 0.05) {
    if (verbose) {
      message("Mahalanobis distance statistic is extreme")
    }
    return(FALSE)
  } else {
    return(TRUE)
  }
}
