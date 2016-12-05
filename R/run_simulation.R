#' Run simulation
#'
#' A computer experiment will involve an associated compute code that needs to be
#' run to collect new data. This function automatically collects new data to allow
#' GADGET to continue sequentially designing and running an experiment.
#'
#' @param experiment GADGET experiment object

run_simulation <- function(experiment) {
  #still need to be able to handle batch runs as well, possibly in parallel
  batch        <- experiment$batch
  batch_design <- experiment$next_batch
  num_parms    <- experiment$num_parms
  sim          <- experiment$sim
  for (i in 0:(batch-1)) {
    design_point        <- batch_design[i*num_parms+1:(i+1)*num_parms]
    experiment$design   <- rbind(experiment$design,design_point)
    experiment$response <- rbind(experiment$response,sim(design_point))
  }
  experiment$next_batch    <- list()
  experiment$next_action   <- 1
  experiment$stage         <- experiment$stage + 1
  experiment
}
