#' Two-Input Toy Model from Bastos and O'Hagan (2009)
#' 
#' A toy model for the demonstration of Gaussian process validation plots and statistics. It is a product of a rational function in x[1] and an exponential function in x[2]. The function is smoother in x[2] than x[1]. The function's domain is the unit square. 
#'
#' @param x  A vector of length 2. 
#'
#' @return A scalar.
#' @export
#'
#' @references 
#' Bastos, L. S., & O'Hagan, A. (2009). Diagnostics for gaussian process emulators. Technometrics, 51(4), 425–438, <doi:10.1198/TECH.2009.08019>.
#'
#' @examples
#' print(bo09_toy(c(0.5,0.25)))
#' 
#' n <- 100
#' a <- seq(0,1,length.out = n)
#' x <- expand.grid(a,a)
#' z <- matrix(apply(x,1,bo09_toy),n,n)
#' graphics::contour(a,a,z, xlab = expression(x[1]), ylab = expression(x[2]))
#' 
bo09_toy <- function(x) {
  x1    <- x[1]
  x2    <- x[2]
  part1 <- (1- exp(-0.5/x2))
  part2 <- 2300*x1^3 + 1900*x1^2 + 2092*x1 + 60
  part3 <- 100*x1^3 + 500*x1^2 + 4*x1 + 20
  return(part1*part2/part3)  
}


