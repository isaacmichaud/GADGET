#' Summary of utility GADGET experiment
#'
#' Prints a summary of a utility based GADGET experiment.
#'
#' @param experiment GADGET utility experiment object
#' @param stage the stage of the GADGET experiment to display
#' @export
summary.GADGET_UT_EXP <- function(experiment,stage=NULL) {
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
