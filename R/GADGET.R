#' Gaussian Process Approximations for Designing Experiment
#'
#' The \code{GADGET} package computes near-optimal Bayesian experimental designs using Gaussian process optimization. At its core is the ability to calculate static designs that maximize a design criterion that may be either deterministic or stochastic. In particular, stochastic design criteria could be a Monte Carlo estimator of an expected utility based on MCMC posterior draws. \code{GADGET} utilizes the algorithm proposed by Weaver et al. (2016) <doi:10.1214/15-BA945> and performs Gaussian process validation using the statistics introduced by  Bastos and O’Hagan  (2009) <doi:10.1198/TECH.2009.08019>. The \code{parallel} package is integrated to parallelize the evaluation of the user's design criterion.  Additionally, \code{GADGET} has wrapped the optimization into a sequential routine to perform sequential computer experiments that automatically call simulator code that is available in R.
#'
#' @references Weaver, B. P., Williams, B. J., Anderson-Cook, C. M., Higdon, D. M. (2016). Computational enhancements to Bayesian design of experiments using Gaussian processes. Bayesian Analysis, 11(1), 191–213, <doi:10.1214/15-BA945>.
#'
#' @docType package
#' @author Isaac Michaud, Brian Weaver, and Brian Williams
#' @name GADGET
NULL

#' Print ASCII GADGET Logo
#'
#' Prints the GADGET ASCII logo in the center of the console.
#'
#' @return Nothing
#' @keywords internal
#' @export
#' @examples
#' print_logo()
print_logo <- function() {
  logo      <- list()
  logo[[1]] <- "  ____    _    ____   ____ _____ _____\n "
  #logo[[2]] <- "/ ___|  / \\  |    \\ / ___| ____|_   _|\n"
  logo[[2]] <- "/ ___|  / \\  |  _ \\ / ___| ____|_   _|\n"
  #logo[[3]] <- "| |  _  / _ \\ | |\U203E| | |  _|  _|   | | \n"
  logo[[3]] <- "| |  _  / _ \\ | | | | |  _|  _|   | | \n"
  logo[[4]] <- "| |_| |/ ___ \\| |_| | |_| | |___  | |\n"
  logo[[5]] <- " \\____/_/   \\_\\____/ \\____|_____| |_|\n"
  subtitle  <- "Gaussian Process Approximations for Designing Experiments\n"
  width     <- getOption("width")
  ws        <- rep(" ", floor((width - nchar(logo[[1]]))/2))

  for (i in 1:5){
    cat(ws,logo[[i]],sep = "")
  }
  cat(rep("-",width),'\n',sep = "")
  ws <- rep(" ", floor((width - nchar(subtitle))/2))
  cat(ws,subtitle,sep="")
}

#' Print Separator
#'
#' Prints a string of dashes to separate output. 
#'
#' @return Nothing
#' @keywords internal
#' @export
#' @examples
#' print_separate()
print_separate <-function() {
  width     <- getOption("width")
  cat(rep("-",width),'\n',sep = "")
}

