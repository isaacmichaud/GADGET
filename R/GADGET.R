#' Gaussian Process Approximations for Designing Experiment
#'
#' The \code{GADGET} package offers
#'
#' @section References:
#' Gao, S., Ver Steeg G., & Galstyan A. (2015). Efficient estimation of mutual information for strongly dependent variables. Artificial Intelligence and Statistics: 277-286.
#'
#' @docType package
#' @author Isaac Michaud
#' @name GADGET
NULL

#' Print ASCII GADGET Logo
#'
#' Prints the GADGET ASCII logo in the center of the console.
#'
#' @return Nothing
#' @keywords internal
#'
#' @examples
#' print_logo()
print_logo <- function() {
  logo      <- list()
  logo[[1]] <- "  ____    _    ____   ____ _____ _____\n "
  logo[[2]] <- "/ ___|  / \\  |    \\ / ___| ____|_   _|\n"
  logo[[3]] <- "| |  _  / _ \\ | |\U203E| | |  _|  _|   | | \n"
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

#' Print Seperator
#'
#' Prints a string of dashes to seperate output. 
#'
#' @return Nothing
#' @keywords internal
#'
#' @examples
#' print_seperator()
print_seperator <-function() {
  width     <- getOption("width")
  cat(rep("-",width),'\n',sep = "")
}

