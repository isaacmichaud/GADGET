#' Create GADGET input files
#' 
#' This function will save batch evaluated DC responses to seed the design_experiment function
#'
#' @param lhs_design design matrix of DC evaluations
#' @param response vector of DC responses
#' @param filename save design to this file 
#'
#' @return
#' @export
#'
#' @examples
create_gadget_infile <- function(lhs_design,response,filename = "gadget_infile.Rdata") {
  gadget_lhs_design = list(x_lhs = lhs_design, y_lhs = response)
  save(gadget_lhs_design, file = filename)
}