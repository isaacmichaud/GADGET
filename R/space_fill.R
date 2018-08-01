#' Create Space-Filling Design
#'
#' Each stage of a GADGET experiment starts by estimated the current design criterion
#' surface using a maximin latin hypercude sample (LHS). This function computes the multidimensional
#' LHS given the initial exploration budget, boundary constraints, and batch size. 
#'
#' @param experiment GADGET experiment object
#'
#' @return LHS design as matrix of size n by k*b 
#'
#' @examples
#' experiment <- list(lower=0,upper=50,batch=4,explore_budget=c(20,2),num_parms=1)
#' my_lhs     <- space_file(experiment)

space_fill <- function(lower_bound, upper_bound, budget, init_lhs) {
  n       <- budget
  k       <- length(lower_bound)
  my_lhs  <- DiceDesign::lhsDesign(n,k)$design
  opt_lhs <- DiceDesign::maximinSA_LHS(my_lhs,T0=10,c=0.99,it=2000)$design
  for (j in 1:k) { #shift and scale the space filling design
      opt_lhs[,j] <- (upper_bound[j]-lower_bound[j])*opt_lhs[,j] + lower_bound[j]
  }
  return(opt_lhs)
}

#evaluates the design criterion on a space filling design (or actually an matrix of observations)
#simplifies the code within design_experiment so that stuff isn't repeated

#' Title
#'
#' @param lhs 
#' @param design_criteria 
#' @param cluster 
#'
#' @return
#' @export
#'
#' @examples
space_eval <- function(lhs,design_criteria,cluster = NULL) {
  pbapply::pbapply(lhs,1,design_criteria,cl = cluster)
}
