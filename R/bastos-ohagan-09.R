#this is the first example in bastos-ohagan-09
toy <- function(x) {
  x1    <- x[1]
  x2    <- x[2]
  part1 <- (1- exp(-0.5/x2))
  part2 <- 2300*x1^3 + 1900*x1^2 + 2092*x1 + 60
  part3 <- 100*x1^3 + 500*x1^2 + 4*x1 + 20
  return(part1*part2/part3)  
}

library(lhs)
library(DiceKriging)

training            <- lhs::randomLHS(100,2)
#training[,1]        <- 0.75
training_response   <- apply(training, 1, toy)
validation          <- maximinLHS(25,2)
colnames(validation)   <- c("X1","X2")
validation_response <- apply(validation, 1, toy)

plot(training,pch=20,xlim=c(0,1),ylim=c(0,1),ylab=expression(x[2]),xlab=expression(x[1]))
points(validation,pch=18,col = "red")

plot(training[,1],training_response,pch=20) #projection isn't as informative


plot(training[,2],training_response,pch=20)

my_model <- km(response~design,design=matrix(training[,2],ncol = 1),response=training_response,covtype='matern5_2',optim.method='gen',nugget.estim=FALSE)
my_model <- km(response~X1 + X2,design=training,response=training_response,covtype='matern5_2',optim.method='gen',nugget.estim=FALSE)
my_model2 <- km(response~X1 + X2,design=training,response = training_response,coef.cov = c(0.2421, 0.4240), coef.var = 3.3316, covtype='matern5_2',optim.method='gen',nugget.estim=FALSE)

my_model@logLik
my_model2@logLik
predictive_errors(validation,matrix(validation_response,ncol=1),my_model2)
