#' Create Space-Filling Design
#'
#' Generates a maximin latin hypercube sample (LHS) covering the space defined by \code{lower_bound} and \code{upper_bound} with \code{budget} number of points. The resulting LHS is shifted and scaled to the domain defined by  \code{lower_bound} and \code{upper_bound}.   
#'
#' @param lower_bound A vector of length \code{d}.
#' @param upper_bound A vector of length \code{d}.
#' @param budget      An integer.  
#'
#' @return A matrix with \code{budget} rows and \code{d} columns.
#' @export
#' @examples
#' my_lhs <- space_fill(c(0,0),c(10,10),20)
#' plot(my_lhs)
space_fill <- function(lower_bound, upper_bound, budget) {
  n       <- budget
  k       <- length(lower_bound)
  #my_lhs  <- DiceDesign::lhsDesign(n,k)$design
  #opt_lhs <- DiceDesign::maximinSA_LHS(my_lhs,T0=10,c=0.99,it=2000)$design
  opt_lhs  <- lhs::maximinLHS(n,k)
  for (j in 1:k) { #shift and scale the space filling design
      opt_lhs[,j] <- (upper_bound[j]-lower_bound[j])*opt_lhs[,j] + lower_bound[j]
  }
  return(data.frame(opt_lhs))
}

#' Evaluate Design Criterion on LHS
#' 
#' This function simplifies the evaluation of design criteria on space-filling designs (matrices where each row is a possible design). The design criterion is parallelized as well using the \code{parallel} package. A progress bar gives a visual indicator when the design criterion is expensive.
#'
#' @param lhs A matrix with \code{n} rows and \code{d} columns.
#' @param criterion  A function with vector input of length \code{d}.
#' @param cluster A \code{parallel} cluster object.
#' 
#' @details Only univariate design criteria can be optimized \code{GADGET} currently. 
#'
#' @return A row vector of length \code{n}.
#' @export
#'
#' @examples my_lhs   <- space_fill(c(0,0),c(10,10),20)
#' dc       <- function(x){sum(x^2)}
#' response <- space_eval(my_lhs,dc)
#' print(response) 
#' 
space_eval <- function(lhs,criterion,cluster = NULL) {
  pbapply::pbapply(lhs,1,criterion,cl = cluster)
}
