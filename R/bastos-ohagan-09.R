#' Two-Input Toy Model from Bastos and O'Hagan 2009 
#' 
#' A toy model for the demonstration of Gaussian process validation plot and statistics. 
#'
#' @param x vector from [0,1] x [0,1]
#'
#' @return scalar
#' @export
#'
#' @references 
#' Bastos, L. S., & O’hagan, A. (2009). Diagnostics for gaussian process emulators. Technometrics, 51(4), 425–438. https://doi.org/10.1198/TECH.2009.08019
#'
#' @examples
#' bo09_toy(c(0.5,0.25))
#' 
bo09_toy <- function(x) {
  x1    <- x[1]
  x2    <- x[2]
  part1 <- (1- exp(-0.5/x2))
  part2 <- 2300*x1^3 + 1900*x1^2 + 2092*x1 + 60
  part3 <- 100*x1^3 + 500*x1^2 + 4*x1 + 20
  return(part1*part2/part3)  
}




