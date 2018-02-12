#' Create space-filling design
#'
#' Each stage of a GADGET experiment starts by estimate the current design criterion
#' surface using a latin hypercude sample (LHS). This function computes the multidimensional
#' LHS given the initial design budget and the boundary constraints of the design space.
#'
#' @param experiment a GADGET experiment object

space_fill <- function(experiment) {
  #require(lhs) #get Latin Hypercube functions
  batch  <- experiment$batch
  upper  <- experiment$upper
  lower  <- experiment$lower
  n      <- experiment$explore_budget[1]
  k      <- experiment$num_parms
  design <- NULL;

  for (i in 1:batch) {
    temp_design = lhs::maximinLHS(n,k)
    for (j in 1:k) { #shift and scale the space filling design
      temp_design[,j] <- (upper[j]-lower[j])*temp_design[,j] + lower[j]
    }
    design <- cbind(design,temp_design)
  }

  return(design)
}



