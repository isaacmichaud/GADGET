#' Run GADGET mutual information experiment
#'
#' Sequentially designs and executes an experiment using the mutual information
#' design criterion.
#'
#' @param experiment GADGET mutual information experiment object
#' @param verbose logical. If FALSE extra output is suppressed.
#' @export
run_mi_experiment <- function(experiment, verbose = FALSE) {
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
      mi <- function(batch_design) {
        num_parms   <- length(EXP$upper)
        num_pts     <- length(post_sample[,1])
        pred_vals   <- NULL
        for (i in 0:(batch-1)) {
            design_point <- batch_design[(i*num_parms+1):((i+1)*num_parms)]
            temp_pred    <- matrix(0,ncol = 1)
            for(j in 1:num_pts) {
              temp_pred[j] <- EXP$pred_sim(design_point,post_sample[j,])
            }
            pred_vals    <- cbind(pred_vals,temp_pred)
        }
       EXP$dc(post_sample,pred_vals)
      }

      EXP             <- run_stage(EXP,design_criteria = mi);
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
