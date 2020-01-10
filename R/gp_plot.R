#' Plot 1D Gaussian Process Model
#'
#' Plots univariate Gaussian process (GP) models with 95\% credible bands. For extremely simply visualizations.
#'
#' @param model     A GP model of class \code{km} (see \code{\link[DiceKriging]{km-class}}).
#' @param inputs    An vector of values at which to evaluate the GP model.
#' @param plot_data Plot data used to fit the GP as well?
#' 
#' @return A list of GP predictions and uncertainties used to generate the figure. 
#' 
#' @export
#' @examples 
#' x  <- matrix(seq(-1,1,0.3),ncol=1)
#' y  <- x^2 
#' gp <- gp_fit(x,
#'              y,
#'              options=list(formula=~1, 
#'              kernel = "matern5_2", 
#'              optimizer = "gen", 
#'              nuggetUse = FALSE))
#' gp_plot(gp$km.model,seq(-1.5,1.5,0.1)) 
gp_plot <- function(model,inputs, plot_data = FALSE) {
  inputs <- sort(inputs) 
  y      <- stats::predict(model,data.frame(design = inputs),type="SK")
  top    <- max(y$upper95)
  bottom <- min(y$lower95)
  graphics::plot(inputs,y$mean,type='l',ylim=c(bottom - abs(top-bottom)*0.25,top + abs(top-bottom)*0.25),xlab="design",ylab="response")
  graphics::lines(inputs,y$upper95,lty=2,col='red')
  graphics::lines(inputs,y$lower95,lty=2,col='blue')
  graphics::points(model@X,model@y,pch=20)
  return(y)
}