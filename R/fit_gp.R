#' Fit a Gaussian process to the design criterion surface
#'
#' The next batch of design points in the experiment are chosen by optimizing the
#' design criterion over the batch design space. This function updates the Gaussian
#' process model which is used by EQI to sequentially find the best batch design
#' given the current design and prior distribution.
#'
#' @param design a matrix of design points
#' @param response a column of responses
#' @param options a list of options for the GP fitting

fit_gp <- function(design,response,options) {

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





