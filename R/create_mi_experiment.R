#' Creates a Mutual Information Experiment object
#'
#' A GADGET experiment object is created with can be processed to sequentially to
#' pick new batches of design points. All needed functions are collected together
#' so that the user does not need to handle any function calls.
#'
#' @param MI mutual information algorithm to be used as a design criteria and additional parameters
#' @param design list of design points that data have been collected
#' @param response list of responses that have been collected
#' @param posterior_sampler function that returns a sample from the posterior distribution
#' @param posterior_parms list of parameters to pass to posterior_sampler
#' @param predictive_sampler function that produces a sample from the predictive distribution
#' @param predictive_parms list of parameters to pass to predictive_sampler
#' @param lower_bound  vector containing the lower bound(s) of the design space
#' @param upper_bound  vector containing the upper bound(s) of the design space
#' @param batch number of design points to be found per iteration
#' @param explore_budget vector contain the number of initial space filling points and the number of EQI exploration points
#' @param design_budget number of batches that GADGET will compute
#' @param gp_options options that specify the type of Gaussian process to fit to the mutual information surface
#' @param simulation computer simulation being explored
#' @param simulation_parms extra parameters to call the computer simulation with
#' @export
#'

create_mi_experiment <- function(MI=list('knn',6),
                                 design,
                                 response,
                                 posterior_sampler,
                                 posterior_parms = NULL,
                                 predictive_sampler,
                                 predictive_parms = NULL,
                                 lower_bound,
                                 upper_bound,
                                 batch=1,
                                 explore_budget = c(10,10),
                                 design_budget = 10,
                                 gp_options = list(),
                                 simulation=NULL,
                                 simulation_parms=NULL
                                 ) {

if (MI[[1]] == 'knn' ) { #using Kraskov et.al. 2005 knn algorithm
  k <- MI[[2]];
  design_criteria <- function(predictive_sample,posterior_sample) {
    if (nrow(predictive_sample) == nrow(posterior_sample)) {
      require(FNN)
      return(-FNN::mutinfo(predictive_sample,posterior_sample, k, direct=TRUE))
    } else {
      stop("Predictive and posterior samples are of different dimensions")
    }
  }
} else {
 stop("Undefined mutual information algorithm")
}

if (is.null(pred_parms)) {
  pred_sim <- predictive_sampler
} else {
  pred_sim <- function(design_point,theta) {
    predictive_sampler(design_point,theta,predictive_parms)
  }
}

if(is.null(simulation)) { #this will tell us if we should call the simulation or
  sim <- NULL
} else {
  if (is.null(simulation_parms)) {
    sim <- simulation
  } else {
    sim <- function(design_point) {simulation(design_point,simulation_parms)}
  }
}

if(length(gp_options)==0){
  gp_options = list(formula=~1,kernel = "matern5_2",nugget=TRUE)
}

experiment = list(
  design = design,
  response = response,
  upper = upper_bound,
  lower = lower_bound,
  post_sim = posterior_sampler,
  post_parms = posterior_parms,
  pred_sim   = pred_sim,
  pred_parms = predictive_parms,
  dc = design_criteria,
  sim  = sim,
  post = list(),
  stage_output = list(),
  stage = 1,
  design_budget  = design_budget, #number of point to add to the design
  explore_budget = explore_budget, #number of evaluation of the design criterion allowed
  batch = batch, #number in a single batch, need to address if this affects other budget
  gp_options = gp_options,
  next_action = 1,
  next_batch = list(),
  num_parms = length(upper_bound)
  )
class(experiment) <- "GADGET_MI_EXP"
return(experiment)
}
