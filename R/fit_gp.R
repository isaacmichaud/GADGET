#' Fit a Gaussian process to the design criterion surface
#'
#' The next batch of design points in the experiment are chosen by optimizing the
#' design criterion over the batch design space. This function updates the Gaussian
#' process model which is used by EQI to sequentially find the best batch design
#' given the current design and prior distribution.
#'
#' @param experiment GADGET experiment object
#' @param design a matrix of design points
#' @param response a column of responses

fit_gp <- function(experiment,design,response) {

  #require(DiceKriging)
  options   <- experiment$gp_options
  formula   <- options$formula
  covKernel <- options$kernel
  nuggetUse <- options$nugget

  ##This function will obtain the km model
  ##model is the formula with y~1 as default
  ##design is a dataframe representing the design of experiment
  ##response is a data frame representing the objective function
  ##covtype giving the covariance structure (see ?km for default)
  ##nugget.estim returns the nugget estimate if set to true

  # Create kriging model using two different search algorithms and pick the best

  model1 <- km(formula, design, response, covKernel, nugget.estim=nuggetUse,
               optim.method="BFGS", control=list(trace=FALSE))

  model2 <- km(formula, design, response, covKernel, nugget.estim=nuggetUse,
               optim.method="gen",control=list(trace=FALSE,pop.size=1000,
               max.generations=100,wait.generations=10,
               hard.generation.limit=FALSE))

  #do we really need to have both fitting procedures?
  mle1 <- logLik.km(model1)

  mle2 <- logLik.km(model2)

  #in the future this two model thing should be removed
  if (mle2 > mle1) {
    model <- model2
  } else {
    model <- model1
  }

  #create a list containing the information in the model
  #need to figure out what each of these things are
  mod.list = list(nugget = model@covariance@nugget,
                       n = model@n,
                  design = design,
                response = response,
                       d = model@d,
               noise.var = model@noise.var,
                km.model = model)
  return(mod.list)
  }





