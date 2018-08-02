#' Plot 1-D DiceKriging Gaussian Process Model
#'
#' Plots simple Gaussian process (GP) models with 95% credible bands. 
#'
#' @param model a km GP model from DiceKriging 
#' @param inputs a vector of values at which to evaluate the GP model 
#' @param plot_data logical, should the data used to generate the GP be plotted too? 
#' 
#' @export
#' @example 
gp_plot <- function(model,inputs, plot_data = FALSE) {
  inputs <- sort(inputs) 
  y      <- predict(model,data.frame(design = inputs),type="SK")
  top    <- max(y$upper95)
  bottom <- min(y$lower95)
  plot(inputs,y$mean,type='l',ylim=c(bottom - abs(top-bottom)*0.25,top + abs(top-bottom)*0.25),xlab="design",ylab="response")
  lines(inputs,y$upper95,lty=2,col='red')
  lines(inputs,y$lower95,lty=2,col='blue')
  points(model@X,model@y,pch=20)
  return(y)
}