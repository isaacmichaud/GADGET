# compute the pivoted-cholesky predictive errors for Gaussian process validation

# simple iid normal case
x        <- matrix(runif(20,-1.5,1.5),ncol=1) 
y        <- matrix(rnorm(20),ncol = 1)
my_model <- km(formula=~1,design=x,response=y,covtype='matern5_2',optim.method='BFGS',nugget.estim=FALSE)
# validation data
x        <- matrix(runif(20,-1,1),ncol=1)
y        <- matrix(rnorm(20),ncol = 1)
plotGP(my_model,c(-2,2),0.1)
predictive_errors(matrix(x,ncol=1),matrix(y,ncol=1),my_model)



f <- function(x) {
  x^2 
}
#fit model 
x        <- matrix(runif(20,-1.5,1.5),ncol=1) 
y        <- matrix(f(x),ncol = 1)
my_model <- km(formula=~.^2,design=x,response=y,covtype='matern5_2',optim.method='BFGS',nugget.estim=FALSE)
#validate model
x        <- matrix(runif(20,-1.5,1.5),ncol=1) 
y        <- matrix(f(x),ncol = 1)
plotGP(my_model,c(-5,5),0.1)
predictive_errors(matrix(x,ncol=1),matrix(y,ncol=1),my_model, type = "SK")
predictive_errors(matrix(x,ncol=1),matrix(y,ncol=1),my_model, type = "UK")

#y = matrix(f(x),ncol = 1)
#this example demonstrates that the model does not fit well on the outside of original design

predictive_errors <- function(design, response, model, plot = TRUE, type = "SK") {
  browser()
  preds      <- predict(model,design,type=type,cov.compute=TRUE)  
  std_preds  <- (response - preds$mean)/sqrt(diag(preds$cov))
  #these only work in one dimension
  #plot(design,std_preds,pch=20, ylab="standardized residual")
  #title(main = "Standardized Residuals")
  #points(design,rep(2,length(design)),type='l',lty=2, col='red')
  #points(design,rep(-2,length(design)),type='l',lty=2, col='red')
  qqnorm(std_preds)
  D_MD = t(response - preds$mean)%*%solve(preds$cov)%*% (response - preds$mean) # Mahalanobis distance
  
  n = model@n
  q = 0 #mean parameters (0 if SK)
  m = nrow(design) # of validation 
  F_stat <- D_MD * (n-q)/(m*(n-q-2))
  print(c(F_stat,qf(0.025,m,n-q),qf(0.975,m,n-q)))
  
  #pivoted chol
  browser()
  pc <- chol(preds$cov,pivot = TRUE)
  solve(t(pc))%*%(response - preds$mean)
}


plotGP = function(model,limits,steps) {
  require(DiceKriging)
  t = seq(limits[1],limits[2],steps)
  #print(t)
  result = predict(model,data.frame(design = t),type="SK")
  top = max(result$upper95)
  bottom = min(result$lower95)
  plot(t,result$mean,type='l',ylim=c(bottom - abs(top-bottom)*0.25,top + abs(top-bottom)*0.25))
  lines(t,result$upper95,lty=2,col='red')
  lines(t,result$lower95,lty=2,col='blue')
  return(result)
}