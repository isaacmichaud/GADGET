#this function will essentially be the same as run_stage
#this will perform the essential functions of Weaver's paper
#actually this can be used to optimize any black box stochastic function
#just put the objective function as the design_criterion and the resulting
#design will be the argmin of the 

# if it is a deterministic criterion then the minimum is taken over the entire set of dc runs instead of taking the last one. 
#this is the simpliest implimentation of the method by Weaver et. al. 
#' Title
#'
#' @param design_criterion 
#' @param stochastic 
#' @param lower_bound 
#' @param upper_bound 
#' @param init_budget 
#' @param optim_budget 
#' @param gp_options 
#' @param genoud_options 
#' @param diagnostics 
#' @param verbose 
#' @param max_augment 
#' @param cluster 
#'
#' @return
#' @export
#'
#' @examples
design_experiment <- function(design_criterion,
                              stochastic = TRUE, 
                              lower_bound,
                              upper_bound,
                              init_budget    = 10,
                              optim_budget   = 10,
                              gp_options     = list(formula=~1,
                                                    kernel = "matern5_2",
                                                    optimizer = "gen"),
                              genoud_options = list(pop.size = 1000,
                                                    max.generations = 100, 
                                                    wait.generations = 10),
                              diagnostics    = 1,
                              verbose        = 1, 
                              max_augment    = 10, 
                              cluster        = NULL) {
  
  #--- input checks ---# 
  
  if (init_budget  < 1) stop("initial lhs budget must be greater than zero")
  if (optim_budget < 1) stop("optimization budget must be greater than zero")
  if (!is.vector(lower_bound)) stop("lower_bound must be a vector") 
  if (!is.vector(upper_bound)) stop("upper_bound must be a vector")
  if (prod((upper_bound - lower_bound) > 0) == 0) {
    stop("upper_bound and lower_bound are incompatible, check that lower_bound < upper_bound")  
  }
  
  if (stochastic) {
    gp_options$nugget <- TRUE  
  } else {
    gp_options$nugget <- FALSE
  }
  
  if (is.null(gp_options$formula))    stop("gp_options does not specify a mean process formula")
  if (is.null(gp_options$kernel))     stop("gp_options does not specify a covariance kernel")
  if (is.null(gp_options$optimizer))  stop("gp_options does not specify a optimizer")
  
  # --- result collection data structures --- #
  all_models    <- list()
  design_points <- list() #use a list instead because it will be more general than a vector
  eqi           <- NULL
  nugget        <- NULL
  var           <- NULL
  response      <- NULL
  
  optimize            <- TRUE
  lhs_budget          <- init_budget
  validation_design   <- NULL
  validation_response <- NULL
  num_augment         <- max_augment
  augment             <- 0
  counter             <- 0
  
  EQI_controls        <- genoud_options
  EQI_controls$nvar   <- length(upper_bound)
  if(is.null(EQI_controls$trace)) {
    EQI_controls$trace                 <- FALSE
  }
  if(is.null(EQI_controls$print.level)) {
    EQI_controls$print.level           <- 0
  } 
  if(is.null(EQI_controls$hard.generation.limit)) {
    EQI_controls$hard.generation.limit <- FALSE
  }
  
  while (optim_budget > 0) {
    
    if (lhs_budget > 0) {
      message("Evaluating Initial LHS Design")
      x_lhs       <- space_fill(lower_bound,upper_bound,lhs_budget)
      y_lhs       <- space_eval(x_lhs,design_criterion,cluster)
      lhs_budget  <- 0
    }
    
    gp_model <- fit_gp(design   = x_lhs, 
                       response = y_lhs, 
                       options  = gp_options)
    model    <- gp_model$km.model
    
    if (diagnostics !=0) {
      #compute validation data if needed    
      if (is.null(validation_response)) {
        message("Evaluating Validation LHS Design")
        validation_design   <- space_fill(lower_bound,
                                          upper_bound,
                                          init_budget)
        validation_response <- space_eval(validation_design,
                                          design_criterion,
                                          cluster)
      }
      
      #automatic diagnostics 
      if (diagnostics == 1) {
        #gp_validate()
      } 
      
      #manual diagnostics
      if (diagnostics == 2) {
        #plot residuals 
        #add more points if desired
        #look at the current parameters and the optimization routine results
        #suspend the operation for further study (save object too?)
      }
      
      if (augment) {#add the validation data to the design, reattempt validation
        x_lhs <- rbind(x_lhs, validation_design)
        y_lhs <- rbind(y_lhs, validation_response)
        validation_response <- NULL
        validation_design   <- NULL
        gp_model    <- fit_gp(design=x_lhs, 
                              response=y_lhs, 
                              experiment$gp_options)
        optimize    <- FALSE
        num_augment <- num_augment - 1
      }
      
      if (num_augment == 0) {
        warnings("Maximum number of design augmentations reached. Manually Inspect GP fit.")
        #dump the x_lhs and y_lhs 
      }
        
      if (!augment) {
        num_augment  <- max_augment
        optimize     <- TRUE
        diagnositics <- FALSE
      }
    }
    
    if (optimize == TRUE) {
      
      if (counter == 0) message("GP validated, Minimizing Design Criterion")
      
      # Maximize using EQI
      if (stochastic) {
        my_beta <- 0.9
        tau_sq  <- model@covariance@nugget/(optim_budget)
      } else {
        my_beta <- 0.5
        tau_sq  <- 0
      }
      
      
      n            <- gp_model$n
      res          <- DiceOptim::max_EQI(model,
                                         new.noise.var = tau_sq,
                                         beta    = my_beta, 
                                         type    = "SK",
                                         lower   = lower_bound,
                                         upper   = upper_bound,
                                         control = EQI_controls)
      counter      <- counter + 1
      new_y        <- design_criterion(as.vector(res$par))
      
      
      if (verbose > 0) {
        if (length(res$par) > 1){
          des <- paste("(",toString(round(res$par,3)),")",sep='')
        } else {
          des <- toString(round(res$par,3))
        }
        cat(paste("Optimization Step ",
                  counter,
                  " - design: " ,
                  des,
                  " criterion: ",
                  round(new_y,3),
                  " EQI: ",
                  round(res$value[1],5),
                  "\n",
                  sep=''))
      }
      optim_budget <- optim_budget - 1
      #collecting the information from eqi
      x_lhs   <- rbind(x_lhs,res$par)
      y_lhs   <- c(y_lhs,new_y)
      eqi     <- rbind(eqi,unlist(res$value[1]))
      nugget  <- rbind(nugget,model@covariance@nugget)
      var     <- rbind(var,model@covariance@sd2)
    }
  }
  
  if (stochastic) {
    return(list(experiment   = utils::tail(x_lhs,1),
                gp           = model, 
                optimization = data.frame(design    = utils::tail(x_lhs,counter), 
                                          criterion = utils::tail(y_lhs,counter), 
                                          EQI       = eqi, 
                                          Nugget    = nugget,
                                          Var       = var)))
  } else {
    return(list(experiment   = x_lhs[which.min(y_lhs),],
                gp           = model, 
                optimization = data.frame(design    = utils::tail(x_lhs,counter), 
                                          criterion = utils::tail(y_lhs,counter), 
                                          EQI       = eqi, 
                                          Var       = var)))
  }
}
  
  




