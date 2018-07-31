#this function will essentially be the same as run_stage
#this will perform the essential functions of Weaver's paper

design_experiment <- function(design_criteria,
                              stochastic = TRUE, 
                              lower_bound,
                              upper_bound,
                              init_budget = 10,
                              optim_budget = 10,
                              gp_options = list(formula=~1,
                                                kernel = "matern5_2",
                                                nugget=TRUE),
                              diagnostics = FALSE,
                              cluster = NULL
) {
  
  #--- input checks ---# 
  
  if (init_budget < 1)  stop("initial lhs budget must be greater than zero")
  if (optim_budget < 1) stop("optimization budget must be greater than zero")
  if (!is.vector(lower_bound)) stop("lower_bound must be a vector") 
  if (!is.vector(upper_bound)) stop("upper_bound must be a vector")
  if (prod((upper_bound - lower_bound) > 0) == 0) {
    stop("upper_bound and lower_bound are incompatible, check that lower_bound < upper_bound")  
  }
  
  if (stochastic == FALSE) {
    gp_options$nugget <- FALSE
  }
  
  if (is.null(gp_options$formula)) stop("gp_options does not specify a mean process formula")
  if (is.null(gp_options$kernal))  stop("gp_options does not specify a covariance kernel")
  if (is.null(gp_options$nugget))  stop("gp_options does not specify whether nugget is present")
  
  # --- result collection data structures --- #
  all_models    <- list()
  design_points <- list() #use a list instead because it will be more general than a vector
  eqi           <- NULL;
  nugget        <- NULL;
  var           <- NULL;
  response      <- NULL;
  
  optimize   <- TRUE
  lhs_budget <- init_budget
  
  while (optim_budget > 0) {
    if (lhs_budget > 0) {
      xlhs  <- space_fill(lower_bound,upper_bound,lhs_budget)
      if (is.null(cluster)) {
        ylhs  <- matrix(0, nrow = budget_LHS, ncol = 1)
        for (i in 1:lhs_budget) {
          ylhs[i,1] <- design_criteria(xlhs[i,])
        }
      } else {
        ylhs <- matrix(parallel::parApply(cl = cluster, xlhs, 1, dc), nrow = lhs_budget, ncol = 1)
      }
      lhs_budget <- 0
    }
    
    gp_model <- fit_gp(design=xlhs, response=ylhs, experiment$gp_options)
    model    <- gp_model$km.model
    
    if (diagnostics == TRUE) {
      #gp_validate(exp,validationData)
      bad_validate <- TRUE
      if (bad_validate) { 
        optimize   <- FALSE
        lhs_budget <- init_budget
        }
    }
    
    if (optimize == TRUE) {
      # Maximize using EQI
      n      <- gp_model$n
      tau_sq <- model@covariance@nugget/(budget-n)
      
      EQI_controls <- list(trace = FALSE,
                           pop.size = 1000,
                           max.generations = 100,
                           wait.generations = 10,
                           hard.generation.limit = FALSE,
                           print.level=0,
                           nvars = batch*length(upper)
                           )
      
      res <- DiceOptim::max_EQI(model,
                                new.noise.var = tau_sq,
                                beta  = 0.9, #this should be made into an option
                                type  = "UK",
                                lower = rep(lower,batch),
                                upper = rep(upper,batch),
                                control = EQI_controls
                                )
      
      print(paste("EQI Step",i,":",toString(res$par),res$value))
      
      #collecting the information from eqi
      xlhs          <- rbind(xlhs,res$par)
      ylhs          <- rbind(ylhs,dc(as.vector(res$par)))
      eqi           <- rbind(eqi,unlist(res$value[1])) #need to check how to get this cleaner
      nugget        <- rbind(nugget,model@covariance@nugget) #could be removed because we are already saving this
      var           <- rbind(var,model@covariance@sd2) #this could be removed because we are already saving this     
    }
    
  }

  #export the relevant information to the user
  experiment$stage_output[[stage]] <- list(data.frame(
    design=xlhs[1:budget_LHS,],
    design_criteria=ylhs[1:budget_LHS,]
  ),data.frame(
    design=xlhs[-(1:budget_LHS),],
    design_criteria=ylhs[-(1:budget_LHS),],
    EQI = eqi,
    Nugget = nugget,
    Var = var
  ))
  
  experiment$next_batch <- utils::tail(xlhs,1)
  #if (experiment$verbous) {
  cat("Next Batch:")
  cat(experiment$next_batch)
  #}
  #experiment$next_batch <- matrix(tail(xlhs,1),ncol=num_parms,byrow=TRUE)  #need something here
  return(experiment)    
  
}
  
  




