#' Run GADGET utility experiment
#'
#' Sequentially designs and executes an experiment using the expected utility
#' criterion supplied by the user.
#'
#' @param experiment GADGET utility experiment object
#' @param verbose logical. If FALSE extra output is suppressed.
#' @export
run_dc_experiment <- function(experiment, verbose = FALSE, cluster = NULL) {
  EXP         <- experiment
  budget      <- EXP$design_budget
  stage       <- EXP$stage
  next_action <- EXP$next_action
  batch       <- EXP$batch
  
  if (is.integer(cluster)) {
    cl                <- parallel::makePSOCKcluster(cluster)
    cluster_flag      <- TRUE
    made_cluster_flag <- TRUE
  } else if (!is.null(cluster)) {
    cl                <- cluster
    cluster_flag      <- TRUE
    made_cluster_flag <- FALSE
  } else {
    cl                <- NULL
    cluster_flag      <- FALSE
    made_cluster_flag <- FALSE
  }
  
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
        return(EXP$dc(batch_design,post_sample,design,response))
      }
      
      #if (cluster_flag) {
      #  parallel::clusterExport(cl, 'my_dc')
      #}
  
      EXP             <- GADGET:::run_stage(EXP, design_criteria = my_dc, cluster = cl);
      EXP$next_action <- 3
      
    } else if (next_action == 3) { #get new data
      
      if (is.null(EXP$sim)) {
        print("GADGET experiment is waiting for new experimental data")
        print("Please collect data for the following design point(s)")
        print(EXP$next_batch)
        print("Record the new data using the function add_data")
        return(EXP) #return object for new data
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
  
  # clean up the cluster if GADGET made one
  if (made_cluster_flag) {
    parallel::stopCluster(cl)
  }
  
  return(EXP)
}
