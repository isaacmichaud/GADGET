# #test sequential experiment
# 
# design_criterion <- function(x,theta) {
#   sum(x^2)# + rnorm(1,0.1)
# }
# 
# posterior_sampler <- function(design,response) {
#   rnorm(1000)
# }
# 
# simulator <- function(x) {
#   x
# }
# 
# my_result = sequential_experiment(design_criterion = design_criterion,
#                                   stochastic = FALSE, 
#                                   posterior_sampler = posterior_sampler,
#                                   lower_bound = -3,     
#                                   upper_bound = 3,      
#                                   simulator = simulator, 
#                                   diagnostics = 2,
#                                   design_budget = 50,
#                                   optim_budget = 5,
#                                   max_augment = 2,
#                                   batch       = 2) 
# 
# 
# #test sequential experiment
# 
# design_criterion <- function(x,theta) {
#   sum(x^2) + rnorm(1,0.1)
# }
# 
# posterior_sampler <- function(design,response) {
#   rnorm(1000)
# }
# 
# simulator <- function(x) {
#   x
# }
# 
# my_result = sequential_experiment(design_criterion = design_criterion,
#                                   stochastic = TRUE, 
#                                   posterior_sampler = posterior_sampler,
#                                   lower_bound = -3,     
#                                   upper_bound =  3,      
#                                   simulator = simulator, 
#                                   design_budget = 50,
#                                   optim_budget = 5,
#                                   batch        = 2) 
# 
# 
design_criterion <- function(design) {
 fisher_mat <- (1-design[3])*c(1,design[2]) %*% t(c(1,design[2]))
 fisher_mat <- fisher_mat + design[3]*c(1,design[1]) %*% t(c(1,design[1]))
 return(-log(det(fisher_mat)))
}

 my_result <- design_experiment(design_criterion,FALSE,c(0,0.5,0),c(0.5,1,1),init_budget = 200, optim_budget = 2)




