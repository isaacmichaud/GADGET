#' Design Optimal Static Experiment
#'
#' @param design_criteria function which evaluates the utility of a proposed design
#' @param design list of design points that data have been collected
#' @param response list of responses that have been collected
#' @param posterior_sampler function that returns a sample from the posterior distribution
#' @param posterior_parms list of parameters to pass to posterior_sampler
#' @param lower_bound  vector containing the lower bound(s) of the design space
#' @param upper_bound  vector containing the upper bound(s) of the design space
#' @param batch number of design points to be found per iteration
#' @param explore_budget vector contain the number of initial space filling points and the number of EQI exploration points
#' @param design_budget number of observation the experimental design should contain
#' @export
#'

design_experiment <- function(design_criteria,
                              lower_bound,
                              upper_bound,
                              batch = 1,
                              design_type = "continuous",
                              explore_budget = c(10, 10),
                              design_budget = 10,
                              gp_options = list()) {
  
  if(length(gp_options)==0){
    gp_options = list(formula=~1,kernel = "matern5_2",nugget=TRUE)
  }
  
  require(lhs)
  
  #generate data structures
  
  #get LHS sample of the thing
  
 #get Latin Hypercube functions
  n        <- explore_budget[1]
  k        <- length(lower_bound)
  
  eta_init <- list() 

  if (design_type == "continuous") {
    explore_design <- maximinLHS(n,(k+1)*batch)
    for (i in 1:n) {
      eta_temp <- matrix(explore_design[i,],ncol = k+1)
      eta_temp <- data.frame(weights = eta_temp[,1]/sum(eta_temp[,1]),x = eta_temp[,-1])
      eta_init <- c(eta_init,list(eta_temp))
    }
    dim_design_point = (k+1)*batch
  } else if(design_type == "exact") {
    explore_design <- maximinLHS(n,k*batch)
    for (i in 1:n) {
      eta_temp <- data.frame(matrix(explore_design[i,],ncol = k))
      eta_init <- c(eta_init,list(eta_temp))
    }
    dim_design_point = (k)*batch
  } else {
    stop("Invalid design type, use either 'exact' or 'continuous'")
  }
  
  browser()
  cat("1 - Exploration design selected",sep="\n")
  
  cat("2 - Computing design criterion on exploration design",sep="\n")

  pb      <- txtProgressBar(min = 0, max = n, style = 3)
  dc_response <- rep(0,batch) 
  for(i in 1:n){
    dc_response[i] <- design_criteria(eta_init[[i]])
    setTxtProgressBar(pb, i)
  }
  close(pb)
  # us unlist to convert to the approriate vector of design points
  #test to see if the function doesn't have any limits that are zero? is that needed? 
  browser()
  #compute design criteria for the points selected
  
  #fit gp to LHS
  
  #return a list of design criterion evaluations and the associated designs. 
  
  for (i in 1:explore_budget[2]) { #inner loop
    gp_model <- fit_gp(list(gp_options=gp_options),design=xlhs,response=dc_response)
    model    <- gp_model$km.model
    
    #   if (diagnostics == TRUE) {
    #     #how often should the gp be validated?
    #     #create validationdataset
    #     #save in the data frame for possible reuse later
    #     #gpValidate(exp,validationData)
    #   }
    
    # Maximize using EQI
    n      <- gp_model$n
    tau_sq <- model@covariance@nugget/(budget-n)
    
    EQI_controls <- list(trace = FALSE,
                         pop.size = 1000,
                         max.generations = 100,
                         wait.generations = 10,
                         hard.generation.limit = FALSE,
                         print.level=1,
                         nvars = batch*length(upper)
                         #cluster = TRUE
    )
    
    res <- max_EQI(model,
                   new.noise.var = tau_sq,
                   beta  = 0.9, #this should be made into an option
                   type  = "UK",
                   lower = rep(0,dim_design_point),
                   upper = rep(1,dim_design_point),
                   control = EQI_controls
    )
    
    #collecting the information from eqi
    xlhs          <- rbind(xlhs,res$par)
    ylhs          <- rbind(ylhs,design_criteria(res$par))
    eqi           <- rbind(eqi,unlist(res$value[1])) #need to check how to get this cleaner
    nugget        <- rbind(nugget,model@covariance@nugget) #could be removed because we are already saving this
    var           <- rbind(var,model@covariance@sd2) #this could be removed because we are already saving this
  }
  
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
  
  
  
  
  
  
  # experiment = list(
  #   design = design,
  #   response = response,
  #   upper = upper_bound,
  #   lower = lower_bound,
  #   post_sim = posterior_sampler,
  #   post_parms = posterior_parms,
  #   dc = design_criteria,
  #   sim  = sim,
  #   post = list(),
  #   stage_output = list(),
  #   stage = 1,
  #   design_budget  = design_budget, #number of point to add to the design
  #   explore_budget = explore_budget, #number of evaluation of the design criterion allowed
  #   batch = batch, #number in a single batch, need to address if this affects other budget
  #   gp_options = gp_options,
  #   next_action = 1,
  #   next_batch = list(),
  #   num_parms = length(upper_bound)
  # )
  # class(experiment) <- "GADGET_EXP"
  #return(experiment)
return(0)
  }



