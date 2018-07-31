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

getGP = function(doe,response) {
  return(km(formula=~1,design=doe,response=response,covtype='matern5_2',optim.method='BFGS',nugget.estim=TRUE))
}

x = matrix(seq(-1,1,0.1),ncol=1)
y = matrix(x^2,ncol=1)

my_model <- getGP(x,y)
