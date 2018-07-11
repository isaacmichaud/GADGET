#' Creates a utility based Experiment object
#'
#' @param utility function which evaluates the utility of a proposed design
#' @param design list of design points that data have been collected
#' @param response list of responses that have been collected
#' @param posterior_sampler function that returns a sample from the posterior distribution
#' @param posterior_parms list of parameters to pass to posterior_sampler
#' @param lower_bound  vector containing the lower bound(s) of the design space
#' @param upper_bound  vector containing the upper bound(s) of the design space
#' @param batch number of design points to be found per iteration
#' @param explore_budget vector contain the number of initial space filling points and the number of EQI exploration points
#' @param design_budget number of observation the experimental design should contain
#' @param gp_options options that specify the type of Gaussian process to fit to the expected utility surface
#' @param simulation computer simulation being explored
#' @param simulation_parms extra parameters to call the computer simulation with
#' @export
#'

create_ut_experiment <- function(utility,
                                 design,
                                 response,
                                 posterior_sampler,
                                 posterior_parms = NULL,
                                 lower_bound,
                                 upper_bound,
                                 batch=1,
                                 explore_budget = c(10,10),
                                 design_budget = 10,
                                 gp_options = list(),
                                 simulation=NULL,
                                 simulation_parms=NULL
                                 ) {

ut <- function(batch_design,theta,prior_design,prior_response) {
  matrix_batch_design <- matrix(batch_design,nrow=batch,byrow=TRUE)
  return(utility(matrix_batch_design,theta,prior_design,prior_response))
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
  ut = ut,
  sim  = sim,
  post = list(),
  stage_output = list(),
  stage = 1,
  design_budget  = design_budget, #number of point to add to the design
  explore_budget = explore_budget, #number of evaluation of the design criterion allowed
  batch = batch, #number in a single batch, need to address if this affects other budget
  gp_options = gp_options,
  next_action = 1,
  next_batch = NULL,
  num_parms = length(upper_bound)
  )
class(experiment) <- "GADGET_UT_EXP"
return(experiment)
}
