#' Run GADGET utility experiment
#'
#' Sequentially designs and executes an experiment using the expected utility
#' criterion supplied by the user.
#'
#' @param experiment GADGET utility experiment object
#' @param verbose logical. If FALSE extra output is suppressed.
#' @export
run_ut_experiment <- function(experiment, verbose = FALSE) {
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
      Expected_UT <- function(batch_design) {
        design   <- EXP$design
        response <- EXP$response
        num_pts  <- length(post_sample[,1])
        #total    <- 0
        temp     <- rep(0,num_pts)
        for (i in 1:num_pts) {
          temp[i] <- EXP$ut(batch_design,post_sample[i,],design,response)
          #total   <- total + EXP$ut(batch_design,post_sample[i,],design,response)
        }
        return(mean(temp,na.rm=TRUE))
        #return(total/num_pts)
      }

      EXP             <- run_stage(EXP,design_criteria = Expected_UT);
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
