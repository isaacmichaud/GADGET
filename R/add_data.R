#' Add Experimental Data
#'
#' Merges newly collected data with the current stage of the GADGET experiment.
#'
#' @param experiment GADGET experiment object
#' @param new_response matrix where each row is the response for the corresponding row of the current batch design computed by GADGET
#' @export

add_data <- function(experiment,new_response) {
  batch         <- experiment$batch
  batch_design  <- experiment$next_batch
  num_parms     <- experiment$num_parms
  temp_design   <- NULL
  temp_response <- NULL

  if (length(new_response[,1]) != batch) {
    stop("new_response is not the same size as the batch size")
  } else {
   for (i in 0:(batch-1)) {
    design_point  <- batch_design[i*num_parms+1:(i+1)*num_parms]
    temp_design   <- rbind(temp_design,design_point)
    temp_response <- rbind(temp_response,new_response[i,])
   }
  }

  print(data.frame(new_batch = temp_design,new_response = temp_response))
  confirm <- readline(prompt="Confirm adding data to experiment(y/n):")
  if (confirm == 'y') {
    print("add data operation accepted, new experimental data added")
    experiment$design      <- rbind(experiment$design,temp_response)
    experiment$response    <- rbind(experiment$response,temp_design)
    experiment$next_batch  <- list()
    experiment$next_action <- 1
    experiment$stage       <- experiment$stage + 1
    return(experiment)
  } else {
    print("add data operation terminated, experiment not modified")
    return(experiment)
  }

}
