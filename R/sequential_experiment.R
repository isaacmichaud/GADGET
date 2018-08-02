#' Design and Run Sequential Computer Experiment
#'
#' @param design_criteria function which evaluates the utility of a proposed design
#' @param design list of design points that data have been collected
#' @param response list of responses that have been collected
#' @param posterior_sampler function that returns a sample from the posterior distribution
#' @param posterior_parms list of parameters to pass to posterior_sampler
#' @param lower_bound  vector containing the lower bound(s) of the design space
#' @param upper_bound  vector containing the upper bound(s) of the design space
#' @param batch number of design points per experiment stage (batch = 1 is sequential)
#' @param explore_budget vector contain the number of initial space filling points and the number of EQI exploration points
#' @param design_budget number of observation the experimental design should contain
#' @param gp_options options that specify the type of Gaussian process to fit to the expected utility surface
#' @param simulator computer simulator being explored (make it so that it is just a function of the design point)
#' @param simulator_parms extra parameters to call the computer simulator with
#' @export
sequential_experiment <- function(design_criterion,
                                  stochastic, 
                                  posterior_sampler, #if we have a posterior_sampler, assume that design_criterion can accept it as an argument
                                  lower_bound,       #design point lower bound
                                  upper_bound,       #design point upper bound
                                  simulator, 
                                  init_design    = NULL,
                                  init_response  = NULL,
                                  batch          = 1,
                                  init_budget    = 10, #per iteration dc budget
                                  optim_budget   = 10,
                                  design_budget  = 10, #number of batches
                                  gp_options     = list(formula=~1,
                                                        kernel = "matern5_2",
                                                        optimizer = "gen"),
                                  genoud_options = list(pop.size = 1000,
                                                        max.generations = 100, 
                                                        wait.generations = 10),
                                  diagnostics    = 1, #1 is recommended just for simpliciy
                                  verbose        = 1,
                                  max_augment    = 10,
                                  cluster  = NULL) #if null then physical experiment if extra parameters are needed create an anonymus function
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
    posterior_sampler = posterior_sampler,
    design_criterion  = design_criterion,
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
      print_seperator()
      cat(sprintf("Stage %d\n",i))
      #cat("Sampling Posterior\n")
    }
    
    #compute posterior
    post_sample <- posterior_sampler(design,response)
    
    dc <- function(design) {
      design_criterion(matrix(design,nrow = batch, byrow = TRUE), post_sample)
    }
    
    #optimize for the batch  
    
    if (verbose) {
      #cat("Optimizing Design\n")
    }
    #browser()
    next_batch <- design_experiment(design_criterion = dc,
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
    print(next_batch$experiment)
    new_data <- pbapply::pbapply(matrix(next_batch$experiment, nrow = batch, byrow = TRUE), 1, simulator,cl = cluster)
    
    if (verbose) {
      cat("Next Observation(s):\n")
      print((matrix(next_batch$experiment, nrow = batch, byrow = TRUE)))
      cat("\n")
      cat("Running Simulator\n")
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


