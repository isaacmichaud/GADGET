#' Design and Run Sequential Computer Experiment
#'
#' This function implements a sequential version of the method by Weaver et al. (2016) that uses Gaussian process (GP) optimization to estimate an optimal design for a stochastic design criterion. 
#' The function \code{\link[GADGET]{design_experiment}} is used repeatedly to design each stage of the experiment. 
#' After the design for each stage is estimated, an inexpensive simulator is run to collect data on the design. 
#' New posterior samples are drawn, and the process is repeated. 
#' Validation of the fitted GP models is provided by the statistics described in Bastos and O'Hagan (2009). 
#' 
#' @param criterion  A function with vector input of length \code{d} (see details). 
#' @param lower_bound       A vector of length \code{d}.
#' @param upper_bound       A vector of length \code{d}.
#' @param stochastic        Is the design criterion stochastic or deterministic (see details)? 
#' @param simulator         Computer simulator being explored. 
#' @param sampler       A function that sample from the posterior distribution given current observations (see details). 
#' @param init_design   A matrix of design points that data have been collected.
#' @param init_response A matrix of responses that have been collected.
#' @param design_budget Number of sequential experiments to perform. 
#' @param batch         Number of design points per experiment stage (batch = 1 is sequential) (see details)
#' @param init_budget   An integer defining the size of the initial training dataset and the size of the validation dataset for the GP model. 
#' @param optim_budget  An integer defining the number of GP optimizations iterations.
#' @param gp_options    A list specifying the type of GP model to fit (see \code{\link[DiceKriging]{km}}).  
#' @param genoud_options A list specifying the control options to optimizer (see \code{\link[rgenoud]{genoud}}).  
#' @param diagnostics    Type of GP diagnostics to perform before optimization occurs. There are currently three options: 0 (none), 1 (automatic) a simple Mahalanobis distance significance test, 2 (user inspected) execution is paused for visual inspection of pivoted-Cholesky residuals and QQ-plots.
#' @param max_augment    An integer defining the maximum number of design augmentations before terminating GP fitting.
#' @param verbose        Print extra output during execution? 
#' @param cluster        A \code{\link[parallel]{parallel}} cluster object.
#' 
#' @details 
#' The design criterion (DC) is a stochastic or deterministic univariate function that measures the quality of a proposed design. 
#' \code{GADGET} assumes the design criterion must be minimized. For example, instead of maximizing the determinant of the Fisher-information matrix, \code{GADGET} would minimize the negative determinant of the Fisher-information matrix. 
#' If the DC is stochastic then the GP model is fit with a nugget effect and expected quantile improvement (EQI) is used to perform the GP optimization. 
#' The optimal design is taken to be the design that maximizes EQI on the final optimization iteration. 
#' If the DC is deterministic then the GP model is fit without a nugget effect and expected improvement (EI) is used to perform the optimization.  
#' The optimal design is taken to be the design with smallest observed DC over all evaluation of the DC. 
#' 
#' The GADGET represents designs as a d-length vector. The user supplied DC function must translate this vector into the apporiate form for computing the DC. The \code{upper_bound} and \code{lower_bound} arguments define bounds of each element in the vectorized design.  
#' 
#' The \code{batch} allows for more than one design point to be optimized in a single step of \code{GADGET}. 
#' To use this feature, the design criterion must be able to accept multiple design points stack in a matrix with each row being a single design point.
#' 
#' The function \code{sampler} must accept the currently observed design and response (including the initial design and response) to produce posteriors for the simulator's parameters.
#' The design criterion also requires a second argument accepting the posterior sample so that the utility function is computed with respect to the current posterior distribution.    
#' 
#' @export
#' @examples
#' #--- Synthetic Design Problem ---#
#' \donttest{ 
#' #demonstration design criterion
#' dc <- function(x,theta) {sum(x^2) + rnorm(1,0.1)}
#' #demonstration posterior sampler 
#' post <- function(design,response) {rnorm(1000)}
#' #demonstration simulatior 
#' sim <- function(x) {x}
#' my_result = sequential_experiment(criterion    = dc,
#'                                  stochastic    = TRUE,
#'                                  sampler       = post,
#'                                  lower_bound   = -3,
#'                                  upper_bound   =  3,
#'                                  simulator     = sim,
#'                                  design_budget = 2,
#'                                  optim_budget  = 1,
#'                                  batch         = 2)}

sequential_experiment <- function(criterion,
                                  sampler, 
                                  lower_bound,       
                                  upper_bound,       
                                  stochastic, 
                                  simulator, 
                                  init_design    = NULL,
                                  init_response  = NULL,
                                  design_budget  = 10, 
                                  batch          = 1,
                                  init_budget    = 10, 
                                  optim_budget   = 10,
                                  gp_options     = list(formula=~1,
                                                        kernel = "matern5_2",
                                                        optimizer = "gen"),
                                  genoud_options = list(pop.size = 1000,
                                                        max.generations = 100, 
                                                        wait.generations = 10),
                                  diagnostics    = 1, 
                                  verbose        = 1,
                                  max_augment    = 10,
                                  cluster  = NULL) 
{
  
  #--- input checks ---# 
  
  if (design_budget < 1) stop("design budget must be greater than zero")
  if (!is.vector(lower_bound)) stop("lower_bound must be a vector") 
  if (!is.vector(upper_bound)) stop("upper_bound must be a vector")
  if (prod((upper_bound - lower_bound) > 0) == 0) {
    stop("upper_bound and lower_bound are incompatible, check that lower_bound < upper_bound")  
  }
  
  if (is.null(gp_options$formula))    stop("gp_options does not specify a mean process formula")
  if (is.null(gp_options$kernel))     stop("gp_options does not specify a covariance kernel")
  if (is.null(gp_options$optimizer))  stop("gp_options does not specify a optimizer")
  
  #--- initializations ---#

  design   <- init_design
  response <- init_response
  
  experiment = list(
    design            = design,
    response          = response,
    upper_bound       = upper_bound,
    lower_bound       = lower_bound,
    sampler = sampler,
    criterion         = criterion,
    posterior_samples = list(),
    stage_output      = list(),
    init_budget       = init_budget,
    optim_budget      = optim_budget,
    design_budget     = design_budget, #number of point to add to the design
    batch             = batch, #number in a single batch, need to address if this affects other budget
    gp_options        = gp_options,
    genoud_options    = genoud_options
  )
  class(experiment) <- "GADGET_EXP"
  
  if (verbose) {
  print_logo()
  }

  #--- design loop ---#
  for (i in 1:design_budget) {
    
    if (verbose) {
      print_separate()
      cat(sprintf("Stage %d\n",i))
      #cat("Sampling Posterior\n")
    }
    
    #compute posterior
    post_sample <- sampler(design,response)
    
    dc <- function(design) {
      criterion(matrix(design,nrow = batch, byrow = TRUE), post_sample)
    }
    
    #optimize for the batch  
    
    if (verbose) {
      #cat("Optimizing Design\n")
    }
    #browser()
    next_batch <- design_experiment(criterion        = dc,
                                    stochastic       = stochastic, 
                                    lower_bound      = rep(lower_bound,batch),
                                    upper_bound      = rep(upper_bound,batch),
                                    init_budget      = init_budget,
                                    optim_budget     = optim_budget,
                                    gp_options       = gp_options,
                                    genoud_options   = genoud_options, 
                                    diagnostics      = diagnostics,
                                    verbose          = verbose, 
                                    max_augment      = max_augment, 
                                    cluster          = cluster)
    
    if (next_batch$error) { #max_augment failed, something is wrong
      return(list(experiment = experiment,failed_stage = next_batch))
    }
    
    #collect new data
    #print(next_batch$experiment)
    
    if (verbose) {
      cat("Next Observation(s):\n")
      print((matrix(next_batch$experiment, nrow = batch, byrow = TRUE)))
      cat("\n")
      cat("Running Simulator\n")
    }
    
    new_data <- pbapply::pbapply(matrix(as.matrix(next_batch$experiment), nrow = batch, byrow = TRUE), 1, simulator,cl = cluster)
    
    if (verbose) {
      cat("\n")
      cat("New Simulation Result(s):\n")
      print(t(new_data))
    }    
    
    design   <- rbind(design,next_batch$experiment)
    response <- rbind(response,t(new_data))
    
    #update experiment
    experiment$design                 <- design
    experiment$response               <- response
    experiment$posterior_samples[[i]] <- post_sample 
    experiment$stage_output[[i]]      <- next_batch
  }
  
  return(experiment)
}


