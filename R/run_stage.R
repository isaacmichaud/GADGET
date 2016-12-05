#' Run experiment stage
#'
#' This function takes a GADGET experiment, computes the next batch of design
#' points. It uses a space-filling design to evalute the design criterion on a
#' parsimonious set of points. A Gaussian process is fit to these points and EQI
#' is used to sequentially find new points to update the Gaussian process to find
#' its minimum. The best batch design is then returned.
#'
#' @param experiment GADGET experiment object
#' @param design_criteria valid GADGET design criterion
#'
run_stage <- function(experiment,design_criteria) {

  require(DiceOptim) #needed for EQI routine
  require(utils) #needed for tail function

  budget_LHS <- experiment$explore_budget[1]
  budget_EQI <- experiment$explore_budget[2]
  budget     <- budget_LHS + budget_EQI
  stage      <- experiment$stage
  posterior  <- experiment$post[[stage]] #let's suppose that the posterior sample is a list of parameters values
  batch      <- experiment$batch
  upper      <- experiment$upper
  lower      <- experiment$lower
  dc         <- design_criteria

# --- result collection data structures --- #
  all_models    <- list()
  design_points <- list() #use a list instead because it will be more general than a vector
  eqi           <- NULL;#list()
  nugget        <- NULL;#list()
  var           <- NULL;#list()
  response      <- NULL;#list()

# if ( verbose == TRUE ) {
#   report to the user what is currently being worked on
#   print("we are currently working on stage X")
#   print the current stage that we are working on
# }

# --- Get inital Latin Hypercube Design --- #
  xlhs  <- space_fill(experiment)
  ylhs  <- matrix(0, nrow = budget_LHS, ncol = 1)
  for (i in 1:budget_LHS) {
    ylhs[i,1] <- dc(xlhs[i,])
  }

# --- Run through the EQI budget --- #
  for (i in 1:budget_EQI) { #inner loop
    gp_model <- fit_gp(experiment,design=xlhs,response=ylhs)
    model    <- gp_model$km.model

#   if (diagnostics == TRUE) {
#     #how often should the gp be validated?
#     #create validationdataset
#     #save in the data frame for possible reuse later
#     #gpValidate(exp,validationData)
#   }

    # Maximize using EQI
    n      <- gp_model$n
    tau_sq <- model@covariance@nugget/(budget-n)

    EQI_controls <- list(trace = FALSE,
                         pop.size = 1000,
                         max.generations = 100,
                         wait.generations = 10,
                         hard.generation.limit = FALSE,
                         print.level=1,
                         nvars = batch*length(upper)
                         #cluster = TRUE
                        )

    res <- max_EQI(model,
                   new.noise.var = tau_sq,
                   beta  = 0.9, #this should be made into an option
                   type  = "UK",
                   lower = rep(lower,batch),
                   upper = rep(upper,batch),
                   control = EQI_controls
                   )

    #collecting the information from eqi
    xlhs          <- rbind(xlhs,res$par)
    ylhs          <- rbind(ylhs,dc(res$par))
    eqi           <- rbind(eqi,unlist(res$value[1])) #need to check how to get this cleaner
    nugget        <- rbind(nugget,model@covariance@nugget) #could be removed because we are already saving this
    var           <- rbind(var,model@covariance@sd2) #this could be removed because we are already saving this
  }

  experiment$stage_output[[stage]] <- list(data.frame(
                                          design=xlhs[1:budget_LHS,],
                                          design_criteria=ylhs[1:budget_LHS,]
                                          ),data.frame(
                                          design=xlhs[-(1:budget_LHS),],
                                          design_criteria=ylhs[-(1:budget_LHS),],
                                          EQI = eqi,
                                          Nugget = nugget,
                                          Var = var
                                          ))

  experiment$next_batch <- tail(xlhs,1)
  #experiment$next_batch <- matrix(tail(xlhs,1),ncol=num_parms,byrow=TRUE)  #need something here
  return(experiment)
}
