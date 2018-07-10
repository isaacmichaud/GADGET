#' Manually Add Experimental Data
#'
#' Merge newly collected data with the current stage of the GADGET experiment and prepares the experiment for the next stage of the experiment. This function is used to add collected data from either physical experiments or long-running computer experiments.
#' 
#' @param experiment GADGET experiment object 
#' @param new_response matrix where each row is the response for the corresponding row of the current batch design
#' @param confirm a logical value indicating whether to prompt user for confirmation before adding new data
#'
#' @return next design points if no response provided; GADGET experiment if response provided  
#' @export
#'
#' @examples 
#' experiment <- list(lower=0,upper=50,batch=4,explore_budget=c(20,2),num_parms=1,next_batch=c(35,47,2,40))
#' add_data(experiment) #no data added
#' add_data(experiment, new_response = c(1,2,3,4), confirm = FALSE) #data added
add_data <- function(experiment, new_response = NULL, confirm = TRUE) {
  batch         <- experiment$batch
  batch_design  <- experiment$next_batch
  num_parms     <- experiment$num_parms
  temp_design   <- NULL
  temp_response <- NULL
  
  if (is.null(batch_design)) { #Experiment doesn't need data
    message("The experiment is not expecting new data.")
    return(invisible(NULL))
  }
  
  next_runs     <- matrix(0, ncol = num_parms, nrow = batch)
  for (j in 0:(num_parms-1)) {
    next_runs[,j+1] <- batch_design[seq(1,batch,num_parms)+j]
  }
  my_names <- c()
  for (i in 1:num_parms) {
    my_names[i] <- sprintf("p_%d",i)
  }
  
  colnames(next_runs) <- my_names
  
  
  if (is.null(new_response)) { #User didn't pass new data
    message("Please collect data for the returned design point(s)")
    return(next_runs)
  } else {
    
    if (!is.matrix(new_response)) {
      stop("new_response is not a matrix")
    }
    
    if (length(new_response[,1]) != batch) {
      stop("new_response is not the same size as the batch size")
    } else {
      
      if (confirm) {
        print(data.frame(next_runs = next_runs,new_response = new_response))
        confirm <- readline(prompt="Confirm adding data to experiment(y/n):")
      } else {
        confirm = 'y'
      }
      
      if (confirm == 'y') {
        print("add data operation accepted, new experimental data added")
        experiment$design      <- rbind(experiment$design,next_runs)
        experiment$response    <- rbind(experiment$response,new_response)
        experiment$next_runs   <- NULL
        experiment$next_action <- 1
        experiment$stage       <- experiment$stage + 1
        return(experiment)
      } else {
        message("add_data operation terminated, experiment not modified")
        return(experiment)
      }
    }
  }
}
