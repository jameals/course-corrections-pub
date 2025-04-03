#' functions for simulating the model, generating errors, and getting the timeseries of K and r (forecast
#' or true) for the gradual change scenarios and periodic update scenarios

#' function for simulating the model:

# years = number of years to run model
# error_set = vector of multiplicative errors each year (output of var_fun)
# r = true intrinsic population growth rate at each time step
# r_for2 = population growth rate used in HCRs for abrupt climate adaptive strategy
# r_for3 = population growth rate used in HCRs for gradual climate adaptive strategy
# Binit = initial population biomass
# Uinit = initial population harvest rate (note this was Finit in earlier versions)
# K = true population carrying capacity at each time step
# K_for2 = carrying capacity used in HCRs for abrupt climate adaptive strategy
# K_for3 = carrying capacity used in HCRs for gradual climate adaptive strategy
# change_ref = which harvest strategy to use (0 = status quo, 1 = abrupt climate adaptive, 2 = gradual climate adaptive)
# delta_set = vector of discount rates for which to calculate revenue
# p = price per unit biomass
# HCR_fun = function specifying the HCR rule to use
# HCR_fixed_pars = named list with all the fixed (not time varying) parameters for the different possible HCR functions
# use_HCR = whether or not to harvest the population (defaults to TRUE)
# fix_neg = whether or not to force negative biomass to be 0 in stochastic simulations (defaults to TRUE)

proj_pop<-function(years, error_set, r, r_for2, r_for3, Binit, Uinit, K, K_for2, K_for3, change_ref,
                   delta_set, p, HCR_fun, HCR_fixed_pars, use_HCR=TRUE, fix_neg = TRUE) {

  # holding vectors
  biomass<-rep(NA,years)
  yield<-rep(NA,years)
  harvest_rate<-rep(NA,years)
  #revenue <- rep(NA, years) # revenue

  B_neg <-rep(NA,years) # holding vector to check how often biomass goes negative (if fix_neg = T)

  # set first elements
  biomass[1]<-Binit
  yield[1] <- Binit * Uinit # changed Finit to Uinit to match the equations we've been writing out
  harvest_rate[1] <- Uinit
  #revenue[1] <- yield[1]*p*exp(-delta*1)l

  # simulate the model with HCR
  for(i in 2:years){

    # the full equation for biomass is:
    # B[i] = biomass[i-1]+(r[i-1]*biomass[i-1]*(1-biomass[i-1]/K[i-1]))*exp(epsilon[i-1]-sigma2/2) - U*biomass[i-1]

    # first calculate change in biomass from growth

    # use error structure following Shelton et al. 2014 herring mode
    #epsilon[i] <- rnorm(1, rho*epsilon[i-1], sigma2) # epsilon is defined as a rondom variable drawn from a normal distribution in log space

    B_t <- biomass[i-1]+(r[i-1]*biomass[i-1]*(1-biomass[i-1]/K[i-1]))*error_set[i-1]
    # error_set[i-1] = exp(rho*epsilon[i-1]-sigma2/2), from error_fun (epilson is in log space, so exponentiate it to apply it to biomass in real space)
    # B_t is just the biomass before adding the harvest term (which depends on the HCR/management strategy and is calculated below) so it does not need to be tracked over time

    # now calculate the harvest term (yield)
    if(use_HCR==FALSE){

      biomass[i] <- B_t
      yield[i] <- 0
      harvest_rate[i] <- 0

      # check whether biomass went negative
      if(biomass[i] < 0){

        if(fix_neg == T){ # if you want to fix it
          # record the negative biomass value
          B_neg[i] <- biomass[i]

          biomass[i] <- 0 # force biomass to be zero
        }

      }


    } else{ # if the population is harvested

      # get the harvest component
      if(change_ref ==0){ # status quo scenario

        # B, K, U_max, B_target
        HCR_pars <- c(HCR_fixed_pars, list(B = biomass[i-1], K = K[1], U_max = r[1]/2, B_target = K[1]/2))

        yield[i] <- HCR_fun(HCR_pars) # catch (U*B) at time t

      }


      if(change_ref==1){ # abrupt climate adaptive scenario

        HCR_pars <- c(HCR_fixed_pars, list(B = biomass[i-1], K = K_for2[i-1], U_max = r_for2[i-1]/2, B_target = K_for2[i-1]/2))
        #HCR_pars <- c(HCR_fixed_pars, list(B = biomass[i-1], K = K_for2[i], U_max = r_for2[i-1]/2, B_target = K_for2[i]/2))

        yield[i] <- HCR_fun(HCR_pars) # catch at time t

      }

      if(change_ref==2){ # gradual climate adaptive scenario

        HCR_pars <- c(HCR_fixed_pars, list(B = biomass[i-1], K = K_for3[i-1], U_max = r_for3[i-1]/2, B_target = K_for3[i-1]/2))
        #HCR_pars <- c(HCR_fixed_pars, list(B = biomass[i-1], K = K_for3[i], U_max = r_for3[i]/2, B_target = K_for3[i]/2))

        yield[i] <- HCR_fun(HCR_pars) # catch at time t

      }

      biomass[i] <- B_t - yield[i] # biomass remaining after harvest

      # check whether biomass went negative
      if(biomass[i] < 0){

        if(fix_neg == T){ # if you want to fix it
          # record the negative biomass value
          B_neg[i] <- biomass[i]

          biomass[i] <- 0 # force biomass to be zero

          #yield[i] <- B_t # only take as many fish as were left
        }

      }


      # calculate harvest rate
      #harvest_rate[i] <- ifelse(biomass[i] != 0, yield[i]/biomass[i], NA)
      harvest_rate[i] <- ifelse(biomass[i-1] != 0, yield[i]/biomass[i-1], NA)

      # revenue[i] <- yield[i]*p*exp(-delta*i)

    }



  }


  # calculate revenue for the discount values in delta_set (since there is currently no effect of delta on
  # the biomass dynamics, we can evaluate the effect of delta without running multiple simulations-- this is just to save computing time)
  revenue_mat <- matrix(NA, nrow = years, ncol = length(delta_set))

  for(j in 1:length(delta_set)){

    revenue_mat[ ,j] <-  yield*p*exp(-delta_set[j]*c(1:years)) #revenue = price x yield x discount factor

  }

  # get the forecast carrying capacities and productivities to return
  if(change_ref==0){ # fixed
    K_for_out <- rep(K[1], length(K))
    r_for_out <- rep(r[1], length(r))
  } else if(change_ref ==1){ # abrupt climate adaptive
    K_for_out <- K_for2
    r_for_out <- r_for2
  } else{ # gradual climate adaptive or periodic updates
    K_for_out <- K_for3
    r_for_out <- r_for3
  }

  # return biomass, yield, harvest_rate, and revenues
  results <- list(biomass=biomass, yield=yield, harvest_rate=harvest_rate, revenue_mat = revenue_mat, B_neg = B_neg,
                  K_true = K, K_fixed = rep(K[1], length(K)), r_true = r,
                  r_fixed = rep(r[1], length(r)), K_for = K_for_out, r_for = r_for_out)
# UPDATED the results to also output the true and fixed values of r and K (these are just the inputs
# into the function but it makes processing the results simpler later on) and the forecast r and K set used in management

  return(results)

}



#' error function: function to get the error components at each time step that then get input into the
#' population simulation function (doing this outside the simulation function to make it easier to save
#' different realizations)

# parameters:
# cv = coefficient of variation (=sd/mean) of the variability in population growth, in normal space
# rho = strength of autocorrelation in process error
# years = number of years to get error terms
# nreps = number of replicate simulations to do
# seed = seed for random draws


error_fun <- function(cv, rho, years, nreps, seed){

  # from shelton_etal_2014/Herring Scenarios FIN.R: function for turning CV on the log-normal scale into
  # standard deviations of the normal distribution on log scale
  #log.norm.cv		<- function(cv){
  #sigma2	<-	 log(cv^2 + 1)
  #return(sqrt(sigma2))
  #}

  sigma2	<-	log(cv^2 + 1)
  sigma <- sqrt(sigma2)

  # set the seed
  set.seed(seed)

  # holding list for the error time series for each replicate
  error_list <- list()

  for(j in 1:nreps){ # for each replicate simulation

    epsilon <- rep(NA, years) # holding vector for errors drawn from normal distribution in log space
    error_mult <- rep(NA, years) # holding vector for the multiplicative process uncertainty term in the population dynamics equation

    epsilon[1] <- rnorm(1, -sigma2/2, sigma) # error in log space

    error_mult[1] <- exp(epsilon[1]-sigma2/2)


    for(i in 2:years){

      # error structure following Shelton et al. 2014 herring model
      epsilon[i] <- rnorm(1, rho*epsilon[i-1]-sigma2/2, sigma) # epsilon is defined as a random variable drawn from a normal distribution in log space

      # epsilon is in log space, so exponentiate it to get the error term that is multiplied to biomass growth in real space
      error_mult[i] <- exp(epsilon[i]-sigma2/2)

    }

    error_list[[j]] <- error_mult

  }

  return(error_list) # return the list with the time series of multiplicative process error terms for each simulation replicate

}


#' functions for generating timeseries of K or r for gradual change scenarios
# piece-wise function for specifying gradual change in a parameter (K or r)
# chng_pt = time point at which the parameter starts changing
# chng_time2 = time point at which the parameter stops changing and reaches its new value
# years = total number of years for which to return values of the parameter
# par = vector of parameter values where the first element is the initial value and the last element is the end value
par_for_fun <- function(chng_pt, chng_time2, years, par){

  par_for <- rep(NA, years)

  for(i in 1:years){

    if(i <= chng_pt){ # if the year is less than the chng_pt
      par_for[i] <- par[1] # par_for is equal to par[1] (haven't started gradual change yet)

    } else if(i > chng_pt & i <= chng_time2){
      # after the chng_pt, use equation of a line to capture gradual change in par_for
      # y = (y2-y1)/(x2-x1)*x + b -> here y2 = par[length(par)] (the end value), y1 = par[1] (the initial value), b is par[1], and the difference x2-x1 is the amount of time it takes for par_for to change from the high to low value
      # and also need to shift to this line to the right by the amount chng_pt so the decline starts at chng_pt rather than 0 (or set chng_pt = 0)
      par_for[i] <- par[1] + (par[length(par)] - par[1])/(chng_time2-chng_pt)*(i-chng_pt)

    } else {
      par_for[i] <- par[length(par)] # after t = chng_time2, par_for is equal to par_B
    }

  }

  return(par_for)

}


# piece-wise function for specifying periodic changes in a parameter (K or r)
# chng_pt = time point at which the parameter starts changing
# chng_time2 = time point at which the parameter stops changing and reaches its new value
# years = total number of years for which to return values of the parameter
# par = vector of parameter values where the first element is the initial value and the last element is the end value
# update_int = frequency at which the estimate of the parameter is updated

update_par_fun <- function(chng_pt, chng_time2, years, par, update_int){

  # first get the continuously changing parmaeter values
  par_for <- rep(NA, years)

  for(i in 1:years){

    if(i <= chng_pt){ # if the year is less than the chng_pt
      par_for[i] <- par[1] # par_for is equal to par[1] (haven't started gradual change yet)

    } else if(i > chng_pt & i <= chng_time2){
      # after the chng_pt, use equation of a line to capture gradual change in par_for
      # y = (y2-y1)/(x2-x1)*x + b -> here y2 = par[length(par)] (the end value), y1 = par[1] (the initial value), b is par[1], and the difference x2-x1 is the amount of time it takes for par_for to change from the high to low value
      # and also need to shift to this line to the right by the amount chng_pt so the decline starts at chng_pt rather than 0 (or set chng_pt = 0)
      par_for[i] <- par[1] + (par[length(par)] - par[1])/(chng_time2-chng_pt)*(i-chng_pt)

    } else {
      par_for[i] <- par[length(par)] # after t = chng_time2, par_for is equal to par_B
    }

  }

  # now get the parmeter values that are updated to match the continuously changing values evey update_int years

  update_par <- rep(NA, years)
  update_par[1] <- par_for[1]

  update_pts <- seq(from = 1, to = years, by = update_int)

  for(i in 2:years){

    if(i %in% update_pts){

      update_par[i] <- par_for[i]

    } else{
      update_par[i] <- update_par[i-1]
    }


  }

  return(update_par)

}







