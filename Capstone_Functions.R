
# This function doesnt work just yet,
# their is a problem with too many infinite values so the hessian can not be 
# estimated.

ensemble_parameters <- function(Ens_Obs_Joined){
  
  # DATA MANIPULATION ---------------------------------------------------------
  
  # extracting both predicted and observed air temp from the long format data
  air_temp = Ens_Obs_Joined[which(Ens_Obs_Joined$variable == 'air_temperature'),]
  
  # arrange them all by parameter (ensemble num) and ordering by ensemble
  air_temp = arrange(air_temp, parameter)
  
  # creating wide tables for both observed data and predicted
  obs_temp = pivot_wider(data = air_temp, id_cols = c(horizon, datetime), 
                              names_from = parameter, 
                              values_from = 'observation')
  air_temp_ens = pivot_wider(data = air_temp, id_cols = c(horizon, datetime,
                                                          reference_datetime), 
                              names_from = parameter, 
                              values_from = 'prediction')
  
  # orderign by date time not horizon 
  air_temp_ens = arrange(air_temp_ens, datetime)
  
  # removing the horizon, datetime, and final column (NA present)
  horizon = air_temp_ens$horizon #saving for later usage
  ref_datetime <- air_temp_ens[-2]
  date_time <- air_temp_ens[-3]
  air_temp_ens = air_temp_ens[-1:-3]
  air_temp_ens = air_temp_ens[-31]
  
  # convert all the dataframes to matrices
  pred_air_temp <<- as.matrix(air_temp_ens)
  
  # selecting just one column of observed data
  obs_temp = arrange(obs_temp, datetime)
  obs_temp = obs_temp$`1`
  obs <<- as.numeric(obs_temp)
  
  # OBJECTIVE FUNCTION --------------------------------------------------------
  
  # This is the objective function that is used in the optimization function
  # 
  # INPUT:
  #   b is the initial set of parameters of length 5
  #   X is the predicted air temperature ensembles
  #   Y is the observed air temperature at the specific location
  #   days is the horizon divided by 24
  #
  # OUTPUT:
  #   returns the sum of the log score to be optimized
  #
  
  bias_spread_param_optim <- function(b, X, Y, days){
    
    # debiases the ensembles
    debiased_ens <- b[1] * X + b[2]
    
    # stores the row means for later use
    rowM <- rowMeans(debiased_ens)
    
    # this is the exponential decay function that is used to adjust the spread
    # of each horizon in the next step
    inflate = b[1] + b[2]*exp(-b[3] * days)
    
    # adjusts each row by the corresponding horizon value found in previous step
    debias_inflated_ens <- inflate * (debiased_ens - rowM) + rowM
    
    # sum of log score
    score = scoringRules::logs_sample(y = Y, dat = debias_inflated_ens)
    score[is.infinite(score)] <- mean(
      score[!is.infinite(scoringRules::logs_sample(y = obs, dat = debias_inflated_ens))])
    
    return(sum(score))
  }
  
  # PARAMETER OPTIMIZATION ----------------------------------------------------
  
  # initial vector of parameters
  b <- c(1,0,0.6, 2.5, 0.15)
  
  # setting upper and lower bounds so optim values are finite
  
  
  
  # optimization call to find the best parameters for the above objective
  # function
  bias_horizon_optim <- optim(par = b, fn = bias_spread_param_optim, 
                         X = pred_air_temp, Y = obs,
                         days = (horizon/24), method = "BFGS"
                         )
  
  
  return(bias_horizon_optim$par)
  
}


# -----------------------------------------------------------------------------

# This function optimizes both the debiasing values and the horizon spread 
# values in separate objective functions. It takes an input of the inner joined
# ensemble data and observations in long table format. 
#
# INPUT:
#     Ens_Obs_Joined is a inner joined long format table that includes both 
#     NOAA GEFS air temperature predictions and the temperature recordings for 
#     the specific location the user would like to adjust the ensembles for. 
# 
# OUTPUT: 
#     Parameters is a vector of length 5, with the first two entries being the 
#     slope and intercept that debias the ensemble. The next 3 entries are used
#     in the exponential decay function. These parameters can be passed into
#     the ensemble_converter() function to adjust the ensemble.

ensemble_parameters_seperate <- function(Ens_Obs_Joined){
  
  # DATA MANIPULATION ---------------------------------------------------------
  
  # extracting both predicted and observed air temp from the long format data
  air_temp = Ens_Obs_Joined[which(Ens_Obs_Joined$variable == 'air_temperature'),]
  
  # arrange them all by parameter (ensemble num) and ordering by ensemble
  air_temp = arrange(air_temp, parameter)
  
  # creating wide tables for both observed data and predicted
  obs_temp = pivot_wider(data = air_temp, id_cols = c(horizon, datetime), 
                         names_from = parameter, 
                         values_from = 'observation')
  air_temp_ens = pivot_wider(data = air_temp, id_cols = c(horizon, datetime,
                                                          reference_datetime), 
                             names_from = parameter, 
                             values_from = 'prediction')
  
  # orderign by date time not horizon 
  air_temp_ens = arrange(air_temp_ens, datetime)
  
  # removing the horizon, datetime, and final column (NA present)
  horizon = air_temp_ens$horizon #saving for later usage
  ref_datetime <- air_temp_ens[-2]
  date_time <- air_temp_ens[-3]
  air_temp_ens = air_temp_ens[-1:-3]
  air_temp_ens = air_temp_ens[-31]
  
  # convert all the dataframes to matrices
  pred_air_temp <<- as.matrix(air_temp_ens)
  
  # selecting just one column of observed data
  obs_temp = arrange(obs_temp, datetime)
  obs_temp = obs_temp$`1`
  obs <<- as.numeric(obs_temp)
  
  # OBJECTIVE FUNCTION --------------------------------------------------------
  
  # This is the objective function that is used in the optimization function
  # 
  # INPUT:
  #   b is the initial set of parameters of length 5
  #   X is the predicted air temperature ensembles
  #   Y is the observed air temperature at the specific location
  #   days is the horizon divided by 24
  #
  # OUTPUT:
  #
  #   returns the sum of the log score to be optimized
  #
  
  spread_param_optim <- function(b, X, Y, days){
    
    # stores the row means for later use
    rowM <- rowMeans(X)
    
    # this is the exponential decay function that is used to adjust the spread
    # of each horizon in the next step
    inflate = b[1] + b[2]*exp(-b[3] * days)
    
    # adjusts each row by the corresponding horizon value found in previous step
    debias_inflated_ens <- inflate * (X - rowM) + rowM
    
    # sum of log score
    return(sum(scoringRules::logs_sample(y = Y, dat = debias_inflated_ens)))
  }
  
  # This function attempts to debias the ensembles to better fit the recorded
  # temperatures. It is an objective function that is used 
  bias_param_optim <- function(b, X, Y){
    
    # debias function
    X1 <- b[1] * X + b[2]
    # this removes INF values and sets them to the mean to the optim can keep 
    # looking for the minimum
    score = scoringRules::logs_sample(y = Y, dat = X1)
    score[is.infinite(score)] <- mean(
      score[!is.infinite(scoringRules::logs_sample(y = obs, dat = X1))])
    return(sum(score))
    
  }
  
  # PARAMETER OPTIMIZATION ----------------------------------------------------
  
  # initial vector of parameters
  b1 <- c(1,0)
  b2 <- c(0.6, 2.5, 0.15)
  
  
  # optimization call to find the best parameters for the above objective
  # function
  bias_optim <- optim(par = b1, fn = bias_param_optim, X = pred_air_temp,
                      Y = obs, method = "BFGS")
  
  # debiases the ensembles
  adj_ensemble <- bias_optim$par[1] * pred_air_temp + bias_optim$par[2]
  
  # optimization function to find the optimal horizon spread values
  horizon_optim <- optim(par = b2, fn = spread_param_optim, 
                              X = adj_ensemble, Y = obs,
                              days = (horizon/24), method = "L-BFGS")
  
  # joining the set of optimized parameters
  b <- c(bias_optim$par, horizon_optim$par, horizon_optim$value)
  
  return(b)
  
}



# ------------------------------------------------------------------------------

# This function accepts two parameters, one in the same form as above, a long 
# format table with the NOAA GEFS predicted air temperature, and a length 5
# 5 vector found in the previous function. This function adjusts the predicted 
# temperature ensembles, and then returns that data frame, with the corresponding 
# horizon, datetime, and reference datetime binded to the front of the data.
#
# INPUT:
#     Ens_Obs_Joined is a inner joined long format table that includes NOAA 
#     GEFS air temperature predictions. It does not need to include the observed
#     air temperature. 
#     Parameters is a length 5 vector that adjusts the ensembles. This is most
#     likeley found in the previous function.
#
# TODO: switch to ensemble and observed data, then inner join them 

# Output:
#     Returns an adjusted air temperature ensemble, with horizon, datetime, 
#     and reference datetime binded to the front of the data.
      
ensemble_converter <- function(Ens_Obs_Joined, Parameters){
  # DATA MANIPULATION ---------------------------------------------------------
  
  # extracting both predicted and observed air temp from the long format data
  air_temp = Ens_Obs_Joined[which(Ens_Obs_Joined$variable == 'air_temperature'),]
  
  # arrange them all by parameter (ensemble num) and ordering by ensemble
  air_temp = arrange(air_temp, parameter)
  
  # creating wide tables for both observed data and predicted
  obs_temp = pivot_wider(data = air_temp, id_cols = c(horizon, datetime), 
                         names_from = parameter, 
                         values_from = 'observation')
  air_temp_ens = pivot_wider(data = air_temp, id_cols = c(horizon, datetime,
                                                          reference_datetime), 
                             names_from = parameter, 
                             values_from = 'prediction')
  
  # orderign by date time not horizon 
  air_temp_ens = arrange(air_temp_ens, datetime)
  
  # removing the horizon, datetime, and final column (NA present)
  horizon = air_temp_ens$horizon #saving for later usage
  first3_col <- air_temp_ens[1:3]
  #ref_datetime <- as.Date.POSIXct(ref_datetime_v, format = '%Y-%m-%d', tz = 'UTC')
  #date_time <- as.Date.POSIXct(date_time_v, format = '%Y-%m-%d %H', tz = 'UTC')
  air_temp_ens = air_temp_ens[-1:-3]
  air_temp_ens = air_temp_ens[-31]
  
  # convert all the dataframes to matrices
  pred_air_temp <<- as.matrix(air_temp_ens)
  
  # ENSEMBLE CONVETING --------------------------------------------------------
  
  # changes days to a numeric value to multiply 
  days = as.numeric(horizon/24)
  
  # debiases the ensembles
  Adj_Ensemble <- Parameters[1] * pred_air_temp + Parameters[2]
  
  # calculates the spread factor for each horizon, following the 
  # exponential decay function 
  inflation <- Parameters[3] + Parameters[4] * exp(-Parameters[5]*days)
  
  # saves the row means to use in the next step
  M <- rowMeans(Adj_Ensemble)
  
  # adjusts the rows of the ensemble by the corresponding horizon spread 
  # factor
  final_ens <- inflation * (Adj_Ensemble - M) + M

  # this binds the horizon, datetime, and reference datetime back onto the 
  # ensembles, for usage later
  date_final_ens <- as.data.frame(cbind(first3_col, final_ens))
  
  # returns the adjusted ensembles
  
  
  # TODO switch to long format
  return(date_final_ens)
}




# ------------------------------------------------------------------------------



# This function utilizes the ggmatplot package to plot the columns of the 
# the ensemble and the observed values over each other
ensemble_plot <- function(Ensemble, observed, Ens_Obs_Joined, horizon_interval = c(1:840),
                          ref_datetime){
  
  air_temp = Ens_Obs_Joined[which(Ens_Obs_Joined$variable == 'air_temperature'),]
  
  # arrange them all by parameter (ensemble num) and ordering by ensemble
  air_temp = arrange(air_temp, parameter)
  
  air_temp_ens = pivot_wider(data = air_temp, id_cols = c(horizon, datetime,
                                                          reference_datetime), 
                             names_from = parameter, 
                             values_from = 'prediction')
  air_temp_ens = arrange(air_temp_ens, datetime)
  
  if (is.Date(ref_datetime)){
    library(ggmatplot)
    
    # gets the specific indices of the reference date
    indices = which(air_temp_ens$reference_datetime == ref_datetime)
    
    adj_obs <- as.data.frame(cbind(Ensemble, observed))
    
    adj_obs <- (adj_obs - 273.15) * 9/5 + 32
    
    one_ref_adj_obs <- adj_obs[indices,]
    
    plt <- ggmatplot(horizon_interval, y = one_ref_adj_obs[horizon_interval,], 
                     plot_type = "line",
                     linetype = c(rep("solid", 31)),
                     color = c(rep('black', 30), 'red')) 
    plt = plt + xlab("Horizon") + ylab("Air Temperature (F)") +
      ggtitle("Adjusted Ensemble Versus Observed Temp")
    
    return(plt)
  }
  else {
    print("Please enter a valid reference date time")
    
  }
}
