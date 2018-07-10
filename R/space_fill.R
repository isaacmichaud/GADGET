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

space_fill <- function(experiment) {

  batch   <- experiment$batch
  upper   <- experiment$upper
  lower   <- experiment$lower
  n       <- experiment$explore_budget[1]
  k       <- experiment$num_parms
  my_lhs  <- DiceDesign::lhsDesign(n,k*batch)$design
  opt_lhs <- DiceDesign::maximinSA_LHS(my_lhs,T0=10,c=0.99,it=2000)$design
  
  for (i in 0:(batch-1)) {
    for (j in 1:k) { #shift and scale the space filling design
     opt_lhs[,k*i + j] <- (upper[j]-lower[j])*opt_lhs[,k*i + j] + lower[j]
    }
  }  
  return(opt_lhs)
}
