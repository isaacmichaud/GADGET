#' Design Optimal Experiment using Gaussian Process Optimization
#' 
#' This function implements the method by Weaver, et al. (2016) that uses Gaussian process (GP) optimization to estimate an optimal design for a stochastic design criterion.  
#' This function can also optimize a deterministic design criterion as well. Validation of the fitted GP models is provided by the statistics described in Bastos and O'Hagan (2009). 
#' For sequential physical experiments or computer experiments with an expensive simulator, one can repeatedly use this function to compute the next step's optimal design point. 
#' For sequential computer experiments with inexpensive simulators, see \code{\link[GADGET]{sequential_experiment}} which will automatically run the simulator and continue the sequential design automatically. 
#'
#' @param criterion    A function with vector input of length \code{d} (see details). 
#' @param lower_bound  A vector of length \code{d}.
#' @param upper_bound  A vector of length \code{d}.
#' @param stochastic   Is the design criterion stochastic or deterministic (see details)? 
#' @param init_budget  An integer defining the size of the initial training dataset and the size of the validation dataset for the GP model.  
#' @param optim_budget An integer defining the number of GP optimization iterations.  
#' @param gp_options   A list specifying the type of GP model to fit (see \code{\link[DiceKriging]{km}}).  
#' @param genoud_options A list specifying the control options to optimizer (see \code{\link[rgenoud]{genoud}}).  
#' @param diagnostics Type of GP diagnostics to perform before optimization occurs. There are currently three options: 0 (none), 1 (automatic) a simple Mahalanobis distance significance test, 2 (user inspected) execution is paused for visual inspection of pivoted-Cholesky residuals and QQ-plots.
#' @param max_augment An integer defining the maximum number of design augmentations before terminating GP fitting.
#' @param verbose     Print extra output during execution?
#' @param infile      Saved evaluations of the DC from previous GADGET run
#' @param outfile     File to save all DC evaluations to while running
#' @param cluster A \code{\link[parallel]{parallel}} cluster object.
#' 
#' @details
#' The design criterion (DC) is a stochastic or deterministic univariate function that measures the quality of a proposed design. 
#' \code{GADGET} assumes the design criterion must be minimized. For example, instead of maximizing the determinant of the Fisher-information matrix, \code{GADGET} would minimize the negative determinant of the Fisher-information matrix.
#' If the DC is stochastic then the GP model is fit with a nugget effect and expected quantile improvement (EQI) is used to perform the GP optimization. 
#' The optimal design is taken to be the design that maximizes EQI on the final optimization iteration. 
#' If the DC is deterministic then the GP model is fit without a nugget effect and expected improvement (EI) is used to perform the optimization.  
#' The optimal design is taken to be the design with smallest observed DC over all evaluation of the DC. 
#'
#' @return A list containing the optimal design, diagnostic results, and final GP model.
#' @references Weaver, B. P., Williams, B. J., Anderson-Cook, C. M., Higdon, D. M. (2016). Computational enhancements to Bayesian design of experiments using Gaussian processes. Bayesian Analysis, 11(1), 191–213, <doi:10.1214/15-BA945>.
#' @export 
#' @examples 
#' #--- Deterministic D-Optimal Design ---#
#' # simple linear regression model
#' # design = c(x1,x2,p) (two point design with weight) 
#' \dontrun{
#' dc <- function(design) {
#' fisher_mat <- (1-design[3])*c(1,design[2]) %*% t(c(1,design[2]))
#' fisher_mat <- fisher_mat + design[3]*c(1,design[1]) %*% t(c(1,design[1]))
#' return(-log(det(fisher_mat)))
#' }
#' #optim_budget is small for demonstration purposes 
#' my_result <- design_experiment(dc,
#'                                c(0,0.5,0),
#'                                c(0.5,1,1),
#'                                FALSE,
#'                                optim_budget = 2) 
#' 
#' #- optimal design -#
#' print(my_result$experiment)
#' #- optimization report - #
#' print(my_result$optimization)
#' #- final gp model -#
#' print(my_result$gp)}
#' 
design_experiment <- function(criterion,
                              lower_bound,
                              upper_bound,
                              stochastic     = TRUE, 
                              init_budget    = 10,
                              optim_budget   = 10,
                              gp_options     = list(formula=~1,
                                                    kernel = "matern5_2",
                                                    optimizer = "gen"),
                              genoud_options = list(pop.size = 1000,
                                                    max.generations = 100, 
                                                    wait.generations = 10),
                              diagnostics    = 1,
                              max_augment    = 10,
                              infile         = NULL,
                              outfile        = NULL,
                              verbose        = TRUE, 
                              cluster        = NULL) {
  
  #--- input checks ---# 
  
  if (init_budget  < 1) stop("initial lhs budget must be greater than zero")
  if (!(init_budget == as.integer(init_budget))) {
    stop("init_budget must be an integer")
  }
  if (optim_budget < 1) stop("optimization budget must be greater than zero")
  if (!(optim_budget == as.integer(optim_budget))) {
    stop("optim_budget must be an integer")
  }
  if (!is.vector(lower_bound)) stop("lower_bound must be a vector") 
  if (!is.vector(upper_bound)) stop("upper_bound must be a vector")
  if (!(length(lower_bound) == length(upper_bound))) {
    stop("lower_bound and upper_bound are not the same length")
  }
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
  
  if (!is.null(infile)) {
    message("Loading Initial Design")
    load(infile) #this needs to be more robust
    init_x_lhs = gadget_lhs_design$x_lhs
    init_y_lhs = gadget_lhs_design$y_lhs
  }
  
  while (optim_budget > 0) {
    
    if (lhs_budget > 0) {
      if (exists("init_x_lhs")) {
        message("Extending Initial LHS Design")
      } else {
        message("Evaluating Initial LHS Design") 
      }
      
      x_lhs       <- space_fill(lower_bound,upper_bound,lhs_budget)
      lhs_budget  <- 0
      y_lhs       <- space_eval(x_lhs,criterion,cluster)
      
      if (exists("init_x_lhs")) {
        x_lhs = rbind(init_x_lhs,x_lhs)
        y_lhs = c(init_y_lhs,y_lhs)
      } 
      
      if (!is.null(outfile)) {
        gadget_lhs_design = list(x_lhs = x_lhs,y_lhs = y_lhs) 
        save(gadget_lhs_design,file = outfile)
      }
    }
    
    gp_model <- gp_fit(design   = x_lhs, 
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
                                          criterion,
                                          cluster)
      }
      
      if (num_augment == 0) {
        warning("Maximum number of design augmentations reached. Manually Inspect GP fit.")
        return(list(x_lhs,y_lhs,gp = model,gp_residuals(validation_design,validation_response,model,plot = FALSE), error = TRUE))
      }
      
      #automatic diagnostics 
      if (diagnostics == 1) {
        if(gp_validate(validation_design,validation_response,model)) {
          augment     <- FALSE
        } else {
          #cat(sprintf(" F-statistic: %f (p-value = %f)\n \n Choose Action: \n", res$F_stat, res$F_pvalue))
          augment     <- TRUE
        }
      }
      
      #manual diagnostics
      if (diagnostics == 2) {
        res    <- gp_residuals(validation_design,validation_response,model,plot = TRUE)
        
        choice <- utils::menu(c("Augment Design","Skip Validation", "Terminate Design"),
                              title = sprintf(" F-statistic: %f (p-value = %f)\n \n Choose Action: \n", res$F_stat, res$F_pvalue))
        
        if(choice == 1) {
          augment     <- TRUE
        }
        
        if (choice == 2) { #skip further validation
          augment     <- FALSE
        }
        
        if (choice == 3) { #terminate design 
          message("Design Optimization Terminated")
          return(list(x_lhs,y_lhs,gp_residuals(validation_design,validation_response,model,plot = FALSE), error = TRUE))
        }

      }
      
      if (augment) {#add the validation data to the design, reattempt validation
        message("Gaussian Process Failed to validate; augmenting design")
        x_lhs <- rbind(x_lhs, validation_design)
        y_lhs <- c(y_lhs, validation_response)
        
        if (!is.null(outfile)) {
          gadget_lhs_design = list(x_lhs = x_lhs,y_lhs = y_lhs) 
          save(gadget_lhs_design,file = outfile)
        }
        
        validation_response <- NULL
        validation_design   <- NULL
        optimize            <- FALSE
        num_augment         <- num_augment - 1
      } else {
        num_augment <- max_augment
        optimize    <- TRUE
        diagnostics <- 0
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
      new_y        <- criterion(as.vector(res$par))
      
      
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
      
      if (!is.null(outfile)) {
        gadget_lhs_design = list(x_lhs = x_lhs,y_lhs = y_lhs) 
        save(gadget_lhs_design,file = outfile)
      }
      
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
                                          Var       = var),error = FALSE))
  } else {
    return(list(experiment   = x_lhs[which.min(y_lhs),],
                gp           = model, 
                optimization = data.frame(design    = utils::tail(x_lhs,counter), 
                                          criterion = utils::tail(y_lhs,counter), 
                                          EQI       = eqi, 
                                          Var       = var), error = FALSE))
  }
}
  
  




