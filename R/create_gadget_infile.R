#' Create GADGET input files
#' 
#' This utility function will save batch evaluated design criterion (DC) responses to seed the design_experiment function.
#'
#' @param lhs_design design matrix of DC evaluations
#' @param response vector of DC responses
#' @param filename save design to this file 
#' @export
#'
#' @examples 
#' \dontrun{
#' #produces an Rdata file containing previous runs of the design criterion
#' set.seed(123)
#' lhs_design         <- lhs::maximinLHS(10,1)
#' criterion_response <- (lhs_design - 1)^2
#' create_gadget_infile(lhs_design,criterion_response, filename = "gadget_demo_infile.Rdata")
#' }
create_gadget_infile <- function(lhs_design,response,filename = "gadget_infile.Rdata") {
  gadget_lhs_design = list(x_lhs = lhs_design, y_lhs = response)
  save(gadget_lhs_design, file = filename)
}