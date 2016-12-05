#' Run GADGET utility experiment
#'
#' Sequentially designs and executes an experiment using the expected utility
#' criterion supplied by the user.
#'
#' @param experiment GADGET utility experiment object
#' @param verbose logical. If FALSE extra output is suppressed.
#' @export
run_dc_experiment <- function(experiment, verbose = FALSE) {
  EXP         <- experiment
  budget      <- EXP$design_budget
  stage       <- EXP$stage
  next_action <- EXP$next_action
  batch       <- EXP$batch
  
  while (stage <= budget) { #this is the outer-loop
    
    if(verbose == TRUE) {
      summary(EXP)
    }
    
    if (next_action == 1) { #get new posterior sample
      
      post_sample       <- EXP$post_sim(EXP$design,EXP$response,EXP$post_parms)
      EXP$post[[stage]] <- post_sample
      EXP$next_action   <- 2
      
    } else if (next_action == 2) { #iterative use EQI to find next batch design
      
      #the following function could be changed in alternative versions of run
      my_dc <- function(batch_design) {
        design   <- EXP$design
        response <- EXP$response
        return(dc(batch_design,post_sample,design,response))
      }
      
      EXP             <- run_stage(EXP,design_criteria = my_dc);
      EXP$next_action <- 3
      
    } else if (next_action == 3) { #get new data
      
      if (is.null(EXP$sim)) {
        print("GADGET experiment is waiting for new experimental data")
        print("Please collect data for the following design point(s)")
        print(EXP$next_batch)
        print("Record the new data using the function add_data")
      } else {
        EXP           <- run_simulation(EXP)
      }
    } else {
      print("This should not have happened")
    }
    
    next_action <- EXP$next_action
    stage       <- EXP$stage
  }
  
  # GADGET is done running the experiment
  if (verbose ==TRUE) {
    summary(EXP)
  }
  return(EXP)
}
