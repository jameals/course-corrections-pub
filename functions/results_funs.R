# functions for processing the output of model simulations

# function for summarizing the results (means, cv's, etc.) and saving them as a data frame
# results = output of proj_pop function (model simulation)
#' par_set = list of parameter values that were used in the simulations:
#' hcr_type (which HCR was used),
#' climate strategy (sq, aca, gca, update_6, or update_12),
#' parameter changing (K, r, both),
#' change_type (grad or step),
#' change_direction (increase or decrease),
#' chng_pt2 (timepoint at which true value of the changing parameter(s) reaches its new value)
#' years (total number of years in simulation)
#' Binit (initial population biomass),
#' chng_time2 (timepoint at which forecasted parameter reaches the new value; controls whether course correction was synchronous, leading, or lagging)
#' delta_set (vector of discount rates for which revenue was calculated),
#' error_rho (autocorrelation in process error),
#' error_cv (cv of process error),
#' error_rep (which replicate the results correspond to),
#' B_cons (minimum biomass below which the population is of conservation concern,
#' expressed as fraction of BMSY)

#' t_start = beginning of time window over which to summarize results
#' t_end = end of time window over which to summarize results
#'
#' for stochastic simulations with error_rep >1, need to repeat this for all error reps, rbind results,
#' and then after getting summaries group by error rep and get the mean and CIs across the error reps

results_fun <- function(results, par_set, t_start, t_end){
  #list(biomass=biomass, yield=yield, harvest_rate=harvest_rate, revenue_mat = revenue_mat)
  B <- results$biomass[t_start:t_end]
  H <- results$yield[t_start:t_end]
  Hr <- results$harvest_rate[t_start:t_end]
  Rmat <- as.matrix(results$revenue_mat[t_start:t_end, ])

  K_true <- results$K_true[t_start:t_end] # true carrying capacity
  K_fixed <- results$K_fixed[t_start:t_end] # fixed carrying capacity
  r_true <- results$r_true[t_start:t_end] # true productivity
  r_fixed <- results$r_fixed[t_start:t_end] # fixed productivity

  N_neg <- length(which(is.na(results$B_neg[t_start:t_end])==F)) # did biomass go below zero?

  # #C_lim <- length(which(B < par_set$B_cons)) # how many years was biomass below the conservation limit?
  # C_lim1 <- length(which(B[1:par_set$chng_pt2] < par_set$B_cons*par_set$K_A))
  # C_lim2 <- length(which(B[(par_set$chng_pt2+1):length(B)] < par_set$B_cons*par_set$K_B))
  # C_lim <- sum(C_lim1, C_lim2)

  # calculate proportion years below true and fixed Bcons, B_MSY, 0.2*B_MSY, and U_MSY
  propl_Bcons_true <- length(which(B < par_set$B_cons*K_true/2))/length(B)
  propl_Bcons_fixed <- length(which(B < par_set$B_cons*K_fixed/2))/length(B)
  propl_BMSY_true <- length(which(B < K_true/2))/length(B)
  propl_BMSY_fixed <- length(which(B < K_fixed/2))/length(B)
  propl_0.2BMSY_true <- length(which(B < (0.2*K_true)/2))/length(B)
  propl_0.2BMSY_fixed <- length(which(B < (0.2*K_fixed)/2))/length(B)
  propl_UMSY_true <- length(which(Hr < r_true/2))/length(B)
  propl_UMSY_fixed <- length(which(Hr < r_fixed/2))/length(B)

  delta_set <- par_set$discount_rates # vector of delta values for which revenue was calculated

  if(t_start == 1 & t_end == par_set$years){
    period <-  "tot"
  } else if(t_start ==1 & t_end == par_set$chng_pt2 - 1){
    period <-  "pre"
  } else if(t_start == par_set$chng_pt2 & t_end == par_set$years){
    period <- "post"
  } else {
    period <- paste(as.character(t_start), as.character(t_end), sep = ":")
  }

  # calculate mean + cv of biomass, cumulative + cv of harvest (=yield w/o discount), mean harvest rate, cumulative + cv of revenue w/ discount

  # get cumulative revenue and cv in revenue for each value of delta (discount rate) in delta_set
  Rcm <- rep(NA, length(delta_set))
  Rcv <- rep(NA, length(delta_set))

  for(i in 1:length(delta_set)){
    # get Rcm and Rcv
    #revenue_mat[,i]
    Rcm[i] <- sum(Rmat[,i])
    Rcv[i] <- sd(Rmat[,i])/mean(Rmat[,i])
  }

  # columns are the parameters in par_set and then the summary statistics (mean and cv of biomass,
  # total harvest and cv of harvest, mean harvest rate, total revenue and cv of revenue for each
  # discount rate in delta_set, etc.)
  # note only mean revenue and cv of revenue depend on delta, so the rest of the summary metrics are just
  # replicated for each value of delta in delta_set
  df <- data.frame(
    period = rep(period, length(delta_set)), # pre, post, or tot
    hcr_type = rep(par_set$hcr_type, length(delta_set)),
    strategy = rep(par_set$strategy, length(delta_set)), # sq (fixed), gca, update_6, update_12, aca
    change_par = rep(par_set$change_par, length(delta_set)), # r, K, both
    change_type = rep(par_set$change_type, length(delta_set)), # true change: grad or step
    direction = rep(par_set$direction, length(delta_set)), # increase or decrease
    B_cons = rep(par_set$B_cons, length(delta_set)), # biomass conservation limit (expressed as a fraction of BMSY)
    Binit = rep(par_set$Binit, length(delta_set)), # initial biomass
    chng_time2 = rep(par_set$chng_time2, length(delta_set)), # timepoint when forecast parameter reaches its new value
    discount_rate = delta_set, # discount values
    error_rho = rep(par_set$error_rho, length(delta_set)), # error autocorrelation
    error_cv = rep(par_set$error_cv, length(delta_set)), # error cv
    error_rep = rep(par_set$error_rep, length(delta_set)), # error replicate
    Bmn=rep(mean(B), length(delta_set)), # mean biomass
    Bcv = rep(sd(B)/mean(B), length(delta_set)), # cv in biomass
    Hcm = rep(sum(H), length(delta_set)), # cumulative harvest
    Hcv = rep(sd(H)/mean(H), length(delta_set)), # cv in harvest
    Hrmn = rep(mean(Hr, na.rm = T), length(delta_set)), # mean harvest rate
    Rcm = Rcm, # cumulative revenue
    Rcv = Rcv, # cv in revenue
    N_neg = rep(N_neg, length(delta_set)), # number of years biomass went negative (just a check)
    propl_Bcons_true = rep(propl_Bcons_true, length(delta_set)), # prop. yrs B < true conservation limit
    propl_Bcons_fixed = rep(propl_Bcons_fixed, length(delta_set)), # prop. yrs B < fixed conservation limit
    propl_BMSY_true = rep(propl_BMSY_true, length(delta_set)), # prop. yrs B < true B_MSY
    propl_BMSY_fixed = rep(propl_BMSY_fixed, length(delta_set)), # prop. yrs B < fixed B_MSY
    propl_0.2BMSY_true = rep(propl_0.2BMSY_true, length(delta_set)), # prop. yrs B < 0.2*true B_MSY
    propl_0.2BMSY_fixed = rep(propl_0.2BMSY_fixed, length(delta_set)), # prop. yrs B < 0.2*fixed B_MSY
    propl_UMSY_true = rep(propl_UMSY_true, length(delta_set)), # prop. yrs harvest rate < true U_MSY
    propl_UMSY_fixed = rep(propl_UMSY_fixed, length(delta_set))) # prop. yrs harvest rate < fixed U_MSY
    #C_lim = rep(C_lim, length(delta_set)))


  # covert from wide to long
  # df_long <- gather(df, metric, value, Bmn:C_lim)
  df_long <- gather(df, metric, value, Bmn:propl_UMSY_fixed)

  return(df_long)

}


# function for turning the full model output (timeseries) into a dataframe
ts_results_fun <- function(results, par_set){
  #list(biomass=biomass, yield=yield, harvest_rate=harvest_rate, revenue_mat = revenue_mat)
  B <- results$biomass
  H <- results$yield
  Hr <- results$harvest_rate
  #Rmat <- as.matrix(results$revenue_mat[t_start:t_end, ])

  years <- length(results$biomass)

  BMSY_true <- results$K_true/2 # true carrying capacity
  BMSY_fixed <- results$K_fixed/2 # fixed carrying capacity
  UMSY_true <- results$r_true/2 # true productivity
  UMSY_fixed <- results$r_fixed/2 # fixed productivity

  BMSY_for <- results$K_for/2 # forecast carrying capacity
  UMSY_for <- results$r_for/2 # forecast productivity

  B_cons_true <- B_cons*BMSY_true # true conservation biomass limit
  B_cons_fixed <- B_cons*BMSY_fixed # fixed conservation biomass limit

  # growth rate:
  growth <- results$r_true*B*(1-B/results$K_true)
  # growth rate: (B_t+1 - B_t)/B_t
  # growth <- (B[2:years]-B[1:(years-1)])/B[1:(years-1)]
  # growth <- c(growth, NA) # no growth rate at last timepoint since don't have B_t+1

  df <- data.frame(
    hcr_type = rep(par_set$hcr_type, years),
    strategy = rep(par_set$strategy, years), # sq (fixed), gca, update_6, update_12, aca
    change_par = rep(par_set$change_par, years), # r, K, both
    change_type = rep(par_set$change_type, years), # true change: grad or step
    direction = rep(par_set$direction, years), # increase or decrease
    B_cons = rep(par_set$B_cons, years), # biomass conservation limit (expressed as a fraction of BMSY)
    Binit = rep(par_set$Binit, years), # initial biomass
    chng_time2 = rep(par_set$chng_time2, years), # timepoint when forecast parameter reaches its new value
    error_rho = rep(par_set$error_rho, years), # error autocorrelation
    error_cv = rep(par_set$error_cv, years), # error cv
    error_rep = rep(par_set$error_rep, years), # error replicate
    time = c(1:years), # timestep
    Biomass=B, # biomass
    Harvest = H, # harvest (yield)
    Harvest_rate = Hr, # harvest rate
    B_cons_true = B_cons_true, # true conservation biomass limit
    B_cons_fixed = B_cons_fixed, # fixed conservation biomass limit
    growth = growth, # growth rate
    BMSY_true = BMSY_true, # true carrying capacity
    BMSY_fixed = BMSY_fixed, # fixed carrying capacity
    UMSY_true = UMSY_true, # true productivity
    UMSY_fixed = UMSY_fixed, # fixed productivity
    BMSY_for = BMSY_for, # forecast carrying capacity
    UMSY_for = UMSY_for) # forecast productivity

  return(df)

}


# function for summarizing the % differences in summary metrics across scenarios
# arguments:
# dataframe with the summary metrics for all the different scenarios
# change_par = which parameter changed (r, K, rk)
# change_type = type of change (step, grad)
# direction = direction of change (increase, decrease)
# sim_pars = parameters used in simulation (named list)

summ_dfs <- function(dt, change_par1, change_type1, direction1, sim_pars){

  Binit.f <- sim_pars$Binit
  hcr_type.f <- sim_pars$hcr_type
  chng_time2.f <- sim_pars$chng_time2
  discount_rate.f <- sim_pars$discount_rate
  error_rho.f <- sim_pars$error_rho
  error_cv.f <- sim_pars$error_cv
  errors.f <- sim_pars$errors

  dt$error_rho <- ifelse(is.na(dt$error_rho)==T, error_rho.f, dt$error_rho)
  dt$error_cv <- ifelse(is.na(dt$error_cv)==T, error_cv.f, dt$error_cv)

  dt <- dt %>% filter(change_par == change_par1, change_type == change_type1,
                      direction == direction1, Binit == Binit.f, hcr_type == hcr_type.f,
                      chng_time2 == chng_time2.f, discount_rate == discount_rate.f,
                      error_rho == error_rho.f, error_cv == error_cv.f, errors == errors.f)

  dt <- dt %>% select(period, strategy, metric, value)

  dt <- dt %>% filter(metric %in% c("Bmn", "Bcv", "Hcm", "Hcv"))

  dt <- dt %>% spread(metric, value)

  # make the comparison data frames
  # start with pre period

  dt_pre <- dt %>% filter(period == "pre")

  dt_sq <- dt_pre %>% filter(strategy == "SQ") %>% select(-period, -strategy)
  dt_aca <- dt_pre %>% filter(strategy == "ACA") %>% select(-period, -strategy)
  dt_gca <- dt_pre %>% filter(strategy == "GCA") %>% select(-period, -strategy)

  # holding vectors for percent differences
  sq_aca <- rep(NA, ncol(dt_sq))
  sq_gca <- rep(NA, ncol(dt_sq))
  aca_gca <- rep(NA, ncol(dt_sq))

  for(i in 1:length(sq_aca)){
    sq_aca[i] <- round((dt_aca[i] - dt_sq[i])/dt_sq[i], 3)
    sq_gca[i] <- round((dt_gca[i] - dt_sq[i])/dt_sq[i], 3)
    aca_gca[i] <- round((dt_gca[i] - dt_aca[i])/dt_aca[i], 3)
  }

  #sq_aca <- round(sq_aca, 2)
  #sq_gca <- round(sq_gca, 2)
  #aca_gca <- round(aca_gca, 2)
  # setNames(rbind.data.frame(c(1, 2, 3), c(4, 5, 6)), c("x", "y", "z"))

  # turn into data frame
  pre_diffs <- setNames(rbind.data.frame(c("sq_aca", "pre", sq_aca), c("sq_gca", "pre", sq_gca), c("aca_gca", "pre", aca_gca)), c("strategies", "period", colnames(dt_sq)))


  # total
  dt_tot <- dt %>% filter(period == "tot")

  dt_sq <- dt_tot %>% filter(strategy == "SQ") %>% select(-period, -strategy)
  dt_aca <- dt_tot %>% filter(strategy == "ACA") %>% select(-period, -strategy)
  dt_gca <- dt_tot %>% filter(strategy == "GCA") %>% select(-period, -strategy)

  # holding vectors for percent differences
  sq_aca <- rep(NA, ncol(dt_sq))
  sq_gca <- rep(NA, ncol(dt_sq))
  aca_gca <- rep(NA, ncol(dt_sq))

  for(i in 1:length(sq_aca)){
    sq_aca[i] <- round((dt_aca[i] - dt_sq[i])/dt_sq[i], 3)
    sq_gca[i] <- round((dt_gca[i] - dt_sq[i])/dt_sq[i], 3)
    aca_gca[i] <- round((dt_gca[i] - dt_aca[i])/dt_aca[i], 3)
  }

  #sq_aca <- round(sq_aca, 2)
  #sq_gca <- round(sq_gca, 2)
  #aca_gca <- round(aca_gca, 2)

  tot_diffs <- setNames(rbind.data.frame(c("sq_aca", "tot", sq_aca), c("sq_gca", "tot", sq_gca), c("aca_gca", "tot", aca_gca)), c("strategies", "period", colnames(dt_sq)))

  # post
  dt_post <- dt %>% filter(period == "post")

  dt_sq <- dt_post %>% filter(strategy == "SQ") %>% select(-period, -strategy)
  dt_aca <- dt_post %>% filter(strategy == "ACA") %>% select(-period, -strategy)
  dt_gca <- dt_post %>% filter(strategy == "GCA") %>% select(-period, -strategy)

  # holding vectors for percent differences
  sq_aca <- rep(NA, ncol(dt_sq))
  sq_gca <- rep(NA, ncol(dt_sq))
  aca_gca <- rep(NA, ncol(dt_sq))

  for(i in 1:length(sq_aca)){
    sq_aca[i] <- round((dt_aca[i] - dt_sq[i])/dt_sq[i], 3)
    sq_gca[i] <- round((dt_gca[i] - dt_sq[i])/dt_sq[i], 3)
    aca_gca[i] <- round((dt_gca[i] - dt_aca[i])/dt_aca[i], 3)
  }

  #sq_aca <- round(sq_aca, 2)
  #sq_gca <- round(sq_gca, 2)
  #aca_gca <- round(aca_gca, 2)

  post_diffs <- setNames(rbind.data.frame(c("sq_aca", "post", sq_aca), c("sq_gca", "post", sq_gca), c("aca_gca", "post", aca_gca)), c("strategies", "period", colnames(dt_sq)))

  all_diffs <- rbind(tot_diffs, pre_diffs, post_diffs)

  all_diffs$calculation <- ifelse(all_diffs$strategies == "sq_aca", "(aca-sq)/sq", ifelse(all_diffs$strategies == "sq_gca", "(gca-sq)/sq", "(gca-aca)/aca"))


  return(all_diffs)

}

# function for calculating the time window over which a specified metric differs between two strategies
# also specify tolerance for how different it needs to be (don't consider years when percent diff is less than diff_thresh)
# output all the years over which they differ, and whether first one was bigger or smaller

# arguments:
# full simulation outputs for each management strategy
# diff_thresh = threshold for how different (in % difference) the strategies need to be from one another to count as being different

tdiff_fun <- function(sq_ts, aca_ts, gca_ts, diff_thresh){

  # biomass timeseries
  B_sq <- sq_ts$biomass
  B_aca <- aca_ts$biomass
  B_gca <- gca_ts$biomass

  # harvest (yield) timeseries
  H_sq <- sq_ts$yield
  H_aca <- aca_ts$yield
  H_gca <- gca_ts$yield

  # compare biomasses
  B_sq_aca <- (B_aca - B_sq)/B_sq # status quo vs aca
  B_sq_gca <- (B_gca - B_sq)/B_sq # status quo vs gca
  B_aca_gca <- (B_gca - B_aca)/B_aca # aca vs gca

  # compare harvest
  H_sq_aca <- (H_aca - H_sq)/H_sq # status quo vs aca
  H_sq_gca <- (H_gca - H_sq)/H_sq # status quo vs gca
  H_aca_gca <- (H_gca - H_aca)/H_aca # aca vs gca

  # make empty holding vectors to fill in with whether the strategies differ and whether the first is bigger or smaller
  years <- c(1:length(B_sq))

  B_sq_aca2 <- ifelse(abs(B_sq_aca) < diff_thresh, NA, ifelse(B_sq_aca >= diff_thresh, "bigger", "smaller")) # status quo vs aca
  B_sq_gca2 <- ifelse(abs(B_sq_gca) < diff_thresh, NA, ifelse(B_sq_gca >= diff_thresh, "bigger", "smaller")) # status quo vs gca
  B_aca_gca2 <- ifelse(abs(B_aca_gca) < diff_thresh, NA, ifelse(B_aca_gca >= diff_thresh, "bigger", "smaller")) # aca vs gca

  # compare harvest
  H_sq_aca2 <- ifelse(abs(H_sq_aca) < diff_thresh, NA, ifelse(H_sq_aca >= diff_thresh, "bigger", "smaller")) # status quo vs aca
  H_sq_gca2 <- ifelse(abs(H_sq_gca) < diff_thresh, NA, ifelse(H_sq_gca >= diff_thresh, "bigger", "smaller")) # status quo vs gca
  H_aca_gca2 <- ifelse(abs(H_aca_gca) < diff_thresh, NA, ifelse(H_aca_gca >= diff_thresh, "bigger", "smaller")) # aca vs gca


  sq_aca_dt <- data.frame(
   years = years,
   B_diff = B_sq_aca2,
   H_diff = H_sq_aca2
  )

  sq_gca_dt <- data.frame(
   years = years,
   B_diff = B_sq_gca2,
   H_diff = H_sq_gca2
  )

  aca_gca_dt <- data.frame(
    years = years,
    B_diff = B_aca_gca2,
    H_diff = H_aca_gca2
  )

  return(list(sq_aca_dt, sq_gca_dt, aca_gca_dt))

}









