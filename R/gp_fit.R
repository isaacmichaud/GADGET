#' Fit a Gaussian Process Model
#'
#' This function fits a Gaussian process (GP) model to a set of possible designs and their corresponding design criterion evaluations. 
#' This GP will then be used to optimize the design criterion using an expected improvement criterion. 
#'
#' @param design   A matrix of \code{n} rows and \code{d} columns. 
#' @param response A column vector of length \code{n}.
#' @param options  A list specifying the type of GP model to fit (see \code{\link[DiceKriging]{km}}). 
#' 
#' @export
#' @return  A list of properties from class \code{km} (see \code{\link[DiceKriging]{km-class}}).  
#' @examples
#' x  <- matrix(seq(-1,1,0.3),ncol=1)
#' y  <- x^2 
#' gp <- gp_fit(x,
#'              y,
#'              options=list(formula=~1, 
#'              kernel = "matern5_2", 
#'              optimizer = "gen", 
#'              nuggetUse = FALSE))
#' gp_plot(gp$km.model,seq(-1.5,1.5,0.1))
#'   
gp_fit <- function(design,response,options = list(formula=~1,
                                              kernel = "matern5_2",
                                              optimizer = "gen",
                                              nuggetUse = TRUE)) {

  formula   <- options$formula
  covKernel <- options$kernel
  nuggetUse <- options$nugget

  if (options$optimizer == "BFGS") {
    model <- DiceKriging::km(formula,
                             design,
                             response,
                             covKernel,
                             nugget.estim = nuggetUse,
                             optim.method = "BFGS", 
                             control = list(trace = FALSE)
                            )
  } else {
    model <- DiceKriging::km(formula,
                             design,
                             response,
                             covKernel,
                             nugget.estim = nuggetUse,
                             optim.method = "gen",
                             control = list(trace = FALSE,
                                            pop.size = 1000,
                                            max.generations = 100,
                                            wait.generations = 10,
                                            hard.generation.limit = FALSE
                                            )
                             )
  }

  #create a list containing the information in the model
  model_list = list(nugget = model@covariance@nugget,
                       n = model@n,
                  design = design,
                response = response,
                       d = model@d,
               noise.var = model@noise.var,
                km.model = model)
  return(model_list)
  }





