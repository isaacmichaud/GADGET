#' Summary of mutual information GADGET experiment
#'
#' Prints a summary of a mutual information based GADGET experiment.
#'
#' @param experiment GADGET mutual information experiment object
#' @param stage the stage of the GADGET experiment to display
#' @export
summary.GADGET_MI_EXP <- function(experiment,stage=NULL) {
  #output at each state, out for overview
  print(data.frame(design=experiment$design,response=experiment$response))
  #
  #   if (verbose == TRUE) {
  #   print extra material that we may want
  #   }
  #
  #   print("***Experiment Summary***")
  #   print("------------------------")
  #   print(data.frame(init_design=experiment$iDesign,
  #                    init_response=experiment$iResponse))
  #   for( i in 1:experiment$stage) {
  #     print("stage")
  #     print(i)
  #     print("something")
  #  }

}
