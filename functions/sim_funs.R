# functions for running model simulations and outputing summarized results



# function for running sensitivity analyses to difference in initial and final K and/or r
# function of which parameter(s) to change (r, K, rK) and sets of percent decreases and percent increases
# also a function of the set of update intervals for the periodic update scenario
sensABCD_fun <- function(change_par_x, dec_set, inc_set, K_A, K_B, r_A, r_B, update_ints){

  # sets of parameter values: 1 = step decrease, 2 = step increase, 3 = grad decrease, 4 = grad increase
  #K_set <- list(K_stepd, K_stepi, K_gradd, K_gradi)
  #K_for2_set <- list(K_stepd, K_stepi, K_stepd, K_stepi) # abrupt climate adaptive
  #K_for3_set <- list(K_gradd, K_gradi, K_gradd, K_gradi) # gradual climate adaptive

   # make the fixed parameter sets
   K_fixed <- rep(K_A, years)
   r_fixed <- rep(r_A, years)

  # step decrease (true parameter has step decrease)
  change_type_x <- "step"
  direction_x <- "decrease"

  for(j in 1:length(dec_set)){

    # get the parameters to use
    if(change_par_x == "K"){

      K_low <- (-dec_set[j]*K_A + K_A)
      K_stepd <- c(rep(K_A,length(1:chng_pt2)),rep(K_low,length((chng_pt2+1):years)))
      K_gradd <- par_for_fun(chng_pt, chng_pt2, years, K_stepd)

      K <- K_stepd; K_for2 <- K_stepd; K_for3 <- K_gradd
      #Binit <- 0.4*K[1]
      Binit <- K[1]/2

      r <- r_fixed; r_for2 <- r_fixed; r_for3 <- r_fixed
      Uinit <- r[1]/2

      par_A <- as.character(K_A); par_B <- as.character(K_low)
    }

    if(change_par_x == "r"){
      r_low <- (-dec_set[j]*r_A + r_A)
      r_stepd <- c(rep(r_A,length(1:chng_pt2)),rep(r_low,length((chng_pt2+1):years)))
      r_gradd <- par_for_fun(chng_pt, chng_pt2, years, r_stepd)

      K <- K_fixed; K_for2 <- K_fixed; K_for3 <- K_fixed
      Binit <- K[1]/2

      r <- r_stepd; r_for2 <- r_stepd; r_for3 <- r_gradd
      Uinit <- r[1]/2

      par_A <- as.character(r_A); par_B <- as.character(r_low)

    }

    if(change_par_x == "rK"){
      K_low <- (-dec_set[j]*K_A + K_A)
      K_stepd <- c(rep(K_A,length(1:chng_pt2)),rep(K_low,length((chng_pt2+1):years)))
      K_gradd <- par_for_fun(chng_pt, chng_pt2, years, K_stepd)

      r_low <- (-dec_set[j]*r_A + r_A)
      r_stepd <- c(rep(r_A,length(1:chng_pt2)),rep(r_low,length((chng_pt2+1):years)))
      r_gradd <- par_for_fun(chng_pt, chng_pt2, years, r_stepd)

      K <- K_stepd; K_for2 <- K_stepd; K_for3 <- K_gradd
      Binit <- K[1]/2

      r <- r_stepd; r_for2 <- r_stepd; r_for3 <- r_gradd
      Uinit <- r[1]/2

      par_A <- paste(as.character(r_A), as.character(K_A), sep = "_"); par_B <- paste(as.character(r_low), as.character(K_low), sep = "_")

    }


    # run the simulations for each management strategy
    for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # pre, post, and total periods
      tot_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = years)
      pre_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = chng_pt2 - 1)
      post_jmm <- results_fun(sim_jmm, pars, t_start = chng_pt2, t_end = years)

      all_jmm <- rbind(tot_jmm, pre_jmm, post_jmm)

      all_jmm$par_A <- rep(par_A, length(all_jmm[,1]))
      all_jmm$per_change <- rep(-dec_set[j]*100, length(all_jmm[,1]))

      # join all the results for the mm^th strategy:
      if(mm == 1){
        dfs_mm <- all_jmm
      } else{
        dfs_mm <- rbind(all_jmm, dfs_mm)
      }

    } # end of iterations for each management strategy

    if(j==1){
      dfs <- dfs_mm
    } else{
      dfs <- rbind(dfs_mm, dfs)
    }

  }


  step_dec_s <- dfs


  # step increase (true parameter has step increase)
  change_type_x <- "step"
  direction_x <- "increase"

  for(j in 1:length(inc_set)){
  # get the parameters to use
  if(change_par_x == "K"){

    K_high <- (inc_set[j]*K_B + K_B)
    K_stepi <- rev(c(rep(K_high,length(1:chng_pt2)),rep(K_B,length((chng_pt2+1):years))))
    K_gradi <- par_for_fun(chng_pt, chng_pt2, years, K_stepi)

    K <- K_stepi; K_for2 <- K_stepi; K_for3 <- K_gradi
    Binit <- K[1]/2

    r <- r_fixed; r_for2 <- r_fixed; r_for3 <- r_fixed
    Uinit <- r[1]/2

    par_A <- as.character(K_B); par_B <- as.character(K_high)
  }

  if(change_par_x == "r"){
    r_high <- (inc_set[j]*r_B + r_B)
    r_stepi <- rev(c(rep(r_high,length(1:chng_pt2)),rep(r_B,length((chng_pt2+1):years))))
    r_gradi <- par_for_fun(chng_pt, chng_pt2, years, r_stepi)

    K <- K_fixed; K_for2 <- K_fixed; K_for3 <- K_fixed
    Binit <- K[1]/2

    r <- r_stepi; r_for2 <- r_stepi; r_for3 <- r_gradi
    Uinit <- r[1]/2

    par_A <- as.character(r_B); par_B <- as.character(r_high)

  }

  if(change_par_x == "rK"){
    K_high <- (inc_set[j]*K_B + K_B)
    K_stepi <- rev(c(rep(K_high,length(1:chng_pt2)),rep(K_B,length((chng_pt2+1):years))))
    K_gradi <- par_for_fun(chng_pt, chng_pt2, years, K_stepi)

    r_high <- (inc_set[j]*r_B + r_B)
    r_stepi <- rev(c(rep(r_high,length(1:chng_pt2)),rep(r_B,length((chng_pt2+1):years))))
    r_gradi <- par_for_fun(chng_pt, chng_pt2, years, r_stepi)

    K <- K_stepi; K_for2 <- K_stepi; K_for3 <- K_gradi
    Binit <- K[1]/2

    r <- r_stepi; r_for2 <- r_stepi; r_for3 <- r_gradi
    Uinit <- r[1]/2

    par_A <- paste(as.character(r_B), as.character(K_B), sep = "_"); par_B <- paste(as.character(r_high), as.character(K_high), sep = "_")

  }

    # run the simulations for each management strategy
    for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # pre, post, and total periods
      tot_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = years)
      pre_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = chng_pt2 - 1)
      post_jmm <- results_fun(sim_jmm, pars, t_start = chng_pt2, t_end = years)

      all_jmm <- rbind(tot_jmm, pre_jmm, post_jmm)

      all_jmm$par_A <- rep(par_A, length(all_jmm[,1]))
      #all_jmm$par_B <- rep(par_B, length(all_jmm[,1]))
      all_jmm$per_change <- rep(inc_set[j]*100, length(all_jmm[,1]))


      # join all the results for the mm^th strategy:
      if(mm == 1){
        dfs_mm <- all_jmm
      } else{
        dfs_mm <- rbind(all_jmm, dfs_mm)
      }

    } # end of iterations for each management strategy

    if(j==1){
      dfs <- dfs_mm
    } else{
      dfs <- rbind(dfs_mm, dfs)
    }


  }

  step_inc_s <- dfs

  # grad decrease (true parameter has gradual decrease)
  change_type_x <- "grad"
  direction_x <- "decrease"

  for(j in 1:length(dec_set)){
  # get the parameters to use
  if(change_par_x == "K"){

    K_low <- (-dec_set[j]*K_A + K_A)
    K_stepd <- c(rep(K_A,length(1:chng_pt2)),rep(K_low,length((chng_pt2+1):years)))
    K_gradd <- par_for_fun(chng_pt, chng_pt2, years, K_stepd)

    K <- K_gradd; K_for2 <- K_stepd; K_for3 <- K_gradd
    Binit <- K[1]/2

    r <- r_fixed; r_for2 <- r_fixed; r_for3 <- r_fixed
    Uinit <- r[1]/2

    par_A <- as.character(K_A); par_B <- as.character(K_low)
  }

  if(change_par_x == "r"){
    r_low <- (-dec_set[j]*r_A + r_A)
    r_stepd <- c(rep(r_A,length(1:chng_pt2)),rep(r_low,length((chng_pt2+1):years)))
    r_gradd <- par_for_fun(chng_pt, chng_pt2, years, r_stepd)

    K <- K_fixed; K_for2 <- K_fixed; K_for3 <- K_fixed
    Binit <- K[1]/2

    r <- r_gradd; r_for2 <- r_stepd; r_for3 <- r_gradd
    Uinit <- r[1]/2

    par_A <- as.character(r_A); par_B <- as.character(r_low)

  }

  if(change_par_x == "rK"){
    K_low <- (-dec_set[j]*K_A + K_A)
    K_stepd <- c(rep(K_A,length(1:chng_pt2)),rep(K_low,length((chng_pt2+1):years)))
    K_gradd <- par_for_fun(chng_pt, chng_pt2, years, K_stepd)

    r_low <- (-dec_set[j]*r_A + r_A)
    r_stepd <- c(rep(r_A,length(1:chng_pt2)),rep(r_low,length((chng_pt2+1):years)))
    r_gradd <- par_for_fun(chng_pt, chng_pt2, years, r_stepd)

    K <- K_gradd; K_for2 <- K_stepd; K_for3 <- K_gradd
    Binit <- K[1]/2

    r <- r_gradd; r_for2 <- r_stepd; r_for3 <- r_gradd
    Uinit <- r[1]/2

    par_A <- paste(as.character(r_A), as.character(K_A), sep = "_"); par_B <- paste(as.character(r_low), as.character(K_low), sep = "_")

  }

    # run the simulations for each management strategy
    for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # pre, post, and total periods
      tot_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = years)
      pre_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = chng_pt2 - 1)
      post_jmm <- results_fun(sim_jmm, pars, t_start = chng_pt2, t_end = years)

      all_jmm <- rbind(tot_jmm, pre_jmm, post_jmm)

      all_jmm$par_A <- rep(par_A, length(all_jmm[,1]))
      #all_jmm$par_B <- rep(par_B, length(all_jmm[,1]))
      all_jmm$per_change <- rep(-dec_set[j]*100, length(all_jmm[,1]))


      # join all the results for the mm^th strategy:
      if(mm == 1){
        dfs_mm <- all_jmm
      } else{
        dfs_mm <- rbind(all_jmm, dfs_mm)
      }

    } # end of iterations for each management strategy

    if(j==1){
      dfs <- dfs_mm
    } else{
      dfs <- rbind(dfs_mm, dfs)
    }


  }

  grad_dec_s <- dfs

  # grad increase (true parameter has gradual increase)
  change_type_x <- "grad"
  direction_x <- "increase"

  for(j in 1:length(inc_set)){
  # get the parameters to use
  if(change_par_x == "K"){

    K_high <- (inc_set[j]*K_B + K_B)
    K_stepi <- rev(c(rep(K_high,length(1:chng_pt2)),rep(K_B,length((chng_pt2+1):years))))
    K_gradi <- par_for_fun(chng_pt, chng_pt2, years, K_stepi)

    K <- K_gradi; K_for2 <- K_stepi; K_for3 <- K_gradi
    Binit <- K[1]/2

    r <- r_fixed; r_for2 <- r_fixed; r_for3 <- r_fixed
    Uinit <- r[1]/2

    par_A <- as.character(K_B); par_B <- as.character(K_high)
  }

  if(change_par_x == "r"){
    r_high <- (inc_set[j]*r_B + r_B)
    r_stepi <- rev(c(rep(r_high,length(1:chng_pt2)),rep(r_B,length((chng_pt2+1):years))))
    r_gradi <- par_for_fun(chng_pt, chng_pt2, years, r_stepi)

    K <- K_fixed; K_for2 <- K_fixed; K_for3 <- K_fixed
    Binit <- K[1]/2

    r <- r_gradi; r_for2 <- r_stepi; r_for3 <- r_gradi
    Uinit <- r[1]/2

    par_A <- as.character(r_B); par_B <- as.character(r_high)

  }

  if(change_par_x == "rK"){
    K_high <- (inc_set[j]*K_B + K_B)
    K_stepi <- rev(c(rep(K_high,length(1:chng_pt2)),rep(K_B,length((chng_pt2+1):years))))
    K_gradi <- par_for_fun(chng_pt, chng_pt2, years, K_stepi)

    r_high <- (inc_set[j]*r_B + r_B)
    r_stepi <- rev(c(rep(r_high,length(1:chng_pt2)),rep(r_B,length((chng_pt2+1):years))))
    r_gradi <- par_for_fun(chng_pt, chng_pt2, years, r_stepi)

    K <- K_gradi; K_for2 <- K_stepi; K_for3 <- K_gradi
    Binit <- K[1]/2

    r <- r_gradi; r_for2 <- r_stepi; r_for3 <- r_gradi
    Uinit <- r[1]/2

    par_A <- paste(as.character(r_B), as.character(K_B), sep = "_"); par_B <- paste(as.character(r_high), as.character(K_high), sep = "_")

  }

    # run the simulations for each management strategy
    for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # pre, post, and total periods
      tot_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = years)
      pre_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = chng_pt2 - 1)
      post_jmm <- results_fun(sim_jmm, pars, t_start = chng_pt2, t_end = years)

      all_jmm <- rbind(tot_jmm, pre_jmm, post_jmm)

      all_jmm$par_A <- rep(par_A, length(all_jmm[,1]))
     # all_jmm$par_B <- rep(par_B, length(all_jmm[,1]))
      all_jmm$per_change <- rep(inc_set[j]*100, length(all_jmm[,1]))


      # join all the results for the mm^th strategy:
      if(mm == 1){
        dfs_mm <- all_jmm
      } else{
        dfs_mm <- rbind(all_jmm, dfs_mm)
      }

    } # end of iterations for each management strategy

    if(j==1){
      dfs <- dfs_mm
    } else{
      dfs <- rbind(dfs_mm, dfs)
    }


  }

  grad_inc_s <- dfs

  # periodic update decreases
  # (true parameter has gradual decrease, but gradual adaptive management scenario is periodic updates)

  for(uu in 1:length(update_ints)){
    change_type_x <- "grad" # true parameter is changing gradually
    direction_x <- "decrease"

    # new strategy names and change_refs: only consider the climate adaptive strategy when using periodic updates
    strat_names2 <- paste("update", update_ints[uu], sep = "_")
    change_refs2 <- 2

    for(j in 1:length(dec_set)){
      # get the parameters to use
      if(change_par_x == "K"){

        K_low <- (-dec_set[j]*K_A + K_A)
        K_stepd <- c(rep(K_A,length(1:chng_pt2)),rep(K_low,length((chng_pt2+1):years)))
        K_gradd <- par_for_fun(chng_pt, chng_pt2, years, K_stepd)
        K_gradd_up <- update_par_fun(chng_pt, chng_pt2, years, K_stepd, update_ints[uu])

        K <- K_gradd; K_for2 <- K_stepd; K_for3 <- K_gradd_up
        Binit <- K[1]/2

        r <- r_fixed; r_for2 <- r_fixed; r_for3 <- r_fixed
        Uinit <- r[1]/2

        par_A <- as.character(K_A); par_B <- as.character(K_low)
      }

      if(change_par_x == "r"){
        r_low <- (-dec_set[j]*r_A + r_A)
        r_stepd <- c(rep(r_A,length(1:chng_pt2)),rep(r_low,length((chng_pt2+1):years)))
        r_gradd <- par_for_fun(chng_pt, chng_pt2, years, r_stepd)
        r_gradd_up <- update_par_fun(chng_pt, chng_pt2, years, r_stepd, update_ints[uu])

        K <- K_fixed; K_for2 <- K_fixed; K_for3 <- K_fixed
        Binit <- K[1]/2

        r <- r_gradd; r_for2 <- r_stepd; r_for3 <- r_gradd_up
        Uinit <- r[1]/2

        par_A <- as.character(r_A); par_B <- as.character(r_low)

      }

      if(change_par_x == "rK"){
        K_low <- (-dec_set[j]*K_A + K_A)
        K_stepd <- c(rep(K_A,length(1:chng_pt2)),rep(K_low,length((chng_pt2+1):years)))
        K_gradd <- par_for_fun(chng_pt, chng_pt2, years, K_stepd)
        K_gradd_up <- update_par_fun(chng_pt, chng_pt2, years, K_stepd, update_ints[uu])

        r_low <- (-dec_set[j]*r_A + r_A)
        r_stepd <- c(rep(r_A,length(1:chng_pt2)),rep(r_low,length((chng_pt2+1):years)))
        r_gradd <- par_for_fun(chng_pt, chng_pt2, years, r_stepd)
        r_gradd_up <- update_par_fun(chng_pt, chng_pt2, years, r_stepd, update_ints[uu])

        K <- K_gradd; K_for2 <- K_stepd; K_for3 <- K_gradd_up
        Binit <- K[1]/2

        r <- r_gradd; r_for2 <- r_stepd; r_for3 <- r_gradd_up
        Uinit <- r[1]/2

        par_A <- paste(as.character(r_A), as.character(K_A), sep = "_"); par_B <- paste(as.character(r_low), as.character(K_low), sep = "_")

      }

      # run the simulations for each management strategy
     # for(mm in 1:length(change_refs2)){

        sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2,
                            K_for3, change_ref = change_refs2, delta_set, p, HCR_fun, HCR_fixed_pars,
                            use_HCR=TRUE)

        pars <- list(hcr_type = "slope", strategy = strat_names2, change_par = change_par_x,
                     change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                     years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                     error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

        # pre, post, and total periods
        tot_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = years)
        pre_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = chng_pt2 - 1)
        post_jmm <- results_fun(sim_jmm, pars, t_start = chng_pt2, t_end = years)

        all_jmm <- rbind(tot_jmm, pre_jmm, post_jmm)

        all_jmm$par_A <- rep(par_A, length(all_jmm[,1]))
        #all_jmm$par_B <- rep(par_B, length(all_jmm[,1]))
        all_jmm$per_change <- rep(-dec_set[j]*100, length(all_jmm[,1]))

        # join all the results for the mm^th strategy:
        # if(mm == 1){
        #   dfs_mm <- all_jmm
        # } else{
        #   dfs_mm <- rbind(all_jmm, dfs_mm)
        # }

        dfs_mm <- all_jmm

     # } # end of iterations for each management strategy

      if(j==1){
        dfs <- dfs_mm
      } else{
        dfs <- rbind(dfs_mm, dfs)
      }


    }

    if(uu==1){
      dfs2 <- dfs
    } else{
      dfs2 <- rbind(dfs, dfs2)
    }

  }

  update_dec_s <- dfs2

  # periodic update increases
  # (true parameter has gradual increase, but gradual adaptive management scenario is periodic updates)

  for(uu in 1:length(update_ints)){
    change_type_x <- "grad"
    direction_x <- "increase"

    # new strategy names and change_refs: only consider the climate adaptive strategy when using periodic updates
    strat_names2 <- paste("update", update_ints[uu], sep = "_")
    change_refs2 <- 2

    for(j in 1:length(inc_set)){
      # get the parameters to use
      if(change_par_x == "K"){

        K_high <- (inc_set[j]*K_B + K_B)
        K_stepi <- rev(c(rep(K_high,length(1:chng_pt2)),rep(K_B,length((chng_pt2+1):years))))
        K_gradi <- par_for_fun(chng_pt, chng_pt2, years, K_stepi)
        K_gradi_up <- update_par_fun(chng_pt, chng_pt2, years, K_stepi, update_ints[uu])

        K <- K_gradi; K_for2 <- K_stepi; K_for3 <- K_gradi_up
        Binit <- K[1]/2

        r <- r_fixed; r_for2 <- r_fixed; r_for3 <- r_fixed
        Uinit <- r[1]/2

        par_A <- as.character(K_B); par_B <- as.character(K_high)
      }

      if(change_par_x == "r"){
        r_high <- (inc_set[j]*r_B + r_B)
        r_stepi <- rev(c(rep(r_high,length(1:chng_pt2)),rep(r_B,length((chng_pt2+1):years))))
        r_gradi <- par_for_fun(chng_pt, chng_pt2, years, r_stepi)
        r_gradi_up <- update_par_fun(chng_pt, chng_pt2, years, r_stepi, update_ints[uu])

        K <- K_fixed; K_for2 <- K_fixed; K_for3 <- K_fixed
        Binit <- K[1]/2

        r <- r_gradi; r_for2 <- r_stepi; r_for3 <- r_gradi_up
        Uinit <- r[1]/2

        par_A <- as.character(r_B); par_B <- as.character(r_high)

      }

      if(change_par_x == "rK"){
        K_high <- (inc_set[j]*K_B + K_B)
        K_stepi <- rev(c(rep(K_high,length(1:chng_pt2)),rep(K_B,length((chng_pt2+1):years))))
        K_gradi <- par_for_fun(chng_pt, chng_pt2, years, K_stepi)
        K_gradi_up <- update_par_fun(chng_pt, chng_pt2, years, K_stepi, update_ints[uu])

        r_high <- (inc_set[j]*r_B + r_B)
        r_stepi <- rev(c(rep(r_high,length(1:chng_pt2)),rep(r_B,length((chng_pt2+1):years))))
        r_gradi <- par_for_fun(chng_pt, chng_pt2, years, r_stepi)
        r_gradi_up <- update_par_fun(chng_pt, chng_pt2, years, r_stepi, update_ints[uu])

        K <- K_gradi; K_for2 <- K_stepi; K_for3 <- K_gradi_up
        Binit <- K[1]/2

        r <- r_gradi; r_for2 <- r_stepi; r_for3 <- r_gradi_up
        Uinit <- r[1]/2

        par_A <- paste(as.character(r_B), as.character(K_B), sep = "_"); par_B <- paste(as.character(r_high), as.character(K_high), sep = "_")

      }

      # run the simulations for each management strategy
     # for(mm in 1:length(change_refs)){

        sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2,
                            K_for3, change_ref = change_refs2, delta_set, p, HCR_fun, HCR_fixed_pars,
                            use_HCR=TRUE)

        pars <- list(hcr_type = "slope", strategy = strat_names2, change_par = change_par_x,
                     change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                     years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                     error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

        # pre, post, and total periods
        tot_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = years)
        pre_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = chng_pt2 - 1)
        post_jmm <- results_fun(sim_jmm, pars, t_start = chng_pt2, t_end = years)

        all_jmm <- rbind(tot_jmm, pre_jmm, post_jmm)

        all_jmm$par_A <- rep(par_A, length(all_jmm[,1]))
        # all_jmm$par_B <- rep(par_B, length(all_jmm[,1]))
        all_jmm$per_change <- rep(inc_set[j]*100, length(all_jmm[,1]))


        # join all the results for the mm^th strategy:
        # if(mm == 1){
        #   dfs_mm <- all_jmm
        # } else{
        #   dfs_mm <- rbind(all_jmm, dfs_mm)
        # }

        dfs_mm <- all_jmm

     # } # end of iterations for each management strategy

      if(j==1){
        dfs <- dfs_mm
      } else{
        dfs <- rbind(dfs_mm, dfs)
      }

    }

    if(uu==1){
      dfs2 <- dfs
    } else{
      dfs2 <- rbind(dfs, dfs2)
    }

  }

  update_inc_s <- dfs2

  # combine results
  sens_df <- rbind(step_dec_s, step_inc_s, grad_dec_s, grad_inc_s, update_dec_s, update_inc_s)

  return(sens_df)

}


# function for running simulations for all the different parameter change/management strategy
# scenarios and saving the whole timeseries
tsruns_fun <- function(change_par_x, K_A, K_B, r_A, r_B, update_ints){
  # here K_A is high, K_B is low

  # sets of parameter values: 1 = step decrease, 2 = step increase, 3 = grad decrease, 4 = grad increase
  #K_set <- list(K_stepd, K_stepi, K_gradd, K_gradi)
  #K_for2_set <- list(K_stepd, K_stepi, K_stepd, K_stepi) # abrupt climate adaptive
  #K_for3_set <- list(K_gradd, K_gradi, K_gradd, K_gradi) # gradual climate adaptive

  # make the fixed parameter sets
  K_fixed <- rep(K_A, years)
  r_fixed <- rep(r_A, years)

  # step decrease (true parameter has step decrease)
  change_type_x <- "step"
  direction_x <- "decrease"

    # get the parameters to use
    if(change_par_x == "K"){

      K_low <- K_B
      K_stepd <- c(rep(K_A,length(1:chng_pt2)),rep(K_low,length((chng_pt2+1):years)))
      K_gradd <- par_for_fun(chng_pt, chng_pt2, years, K_stepd)

      K <- K_stepd; K_for2 <- K_stepd; K_for3 <- K_gradd
      #Binit <- 0.4*K[1]
      Binit <- K[1]/2

      r <- r_fixed; r_for2 <- r_fixed; r_for3 <- r_fixed
      Uinit <- r[1]/2

      par_A <- as.character(K_A); par_B <- as.character(K_low)
    }

    if(change_par_x == "r"){
      r_low <- r_B
      r_stepd <- c(rep(r_A,length(1:chng_pt2)),rep(r_low,length((chng_pt2+1):years)))
      r_gradd <- par_for_fun(chng_pt, chng_pt2, years, r_stepd)

      K <- K_fixed; K_for2 <- K_fixed; K_for3 <- K_fixed
      Binit <- K[1]/2

      r <- r_stepd; r_for2 <- r_stepd; r_for3 <- r_gradd
      Uinit <- r[1]/2

      par_A <- as.character(r_A); par_B <- as.character(r_low)

    }

    if(change_par_x == "rK"){
      K_low <- K_B
      K_stepd <- c(rep(K_A,length(1:chng_pt2)),rep(K_low,length((chng_pt2+1):years)))
      K_gradd <- par_for_fun(chng_pt, chng_pt2, years, K_stepd)

      r_low <- r_B
      r_stepd <- c(rep(r_A,length(1:chng_pt2)),rep(r_low,length((chng_pt2+1):years)))
      r_gradd <- par_for_fun(chng_pt, chng_pt2, years, r_stepd)

      K <- K_stepd; K_for2 <- K_stepd; K_for3 <- K_gradd
      Binit <- K[1]/2

      r <- r_stepd; r_for2 <- r_stepd; r_for3 <- r_gradd
      Uinit <- r[1]/2

      par_A <- paste(as.character(r_A), as.character(K_A), sep = "_"); par_B <- paste(as.character(r_low), as.character(K_low), sep = "_")

    }


    # run the simulations for each management strategy
    for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # turn results into data frame
       all_jmm <- ts_results_fun(sim_jmm, pars)

      all_jmm$par_A <- rep(par_A, length(all_jmm[,1]))
      all_jmm$par_B <- rep(par_B, length(all_jmm[,1]))

      # join all the results for the mm^th strategy:
      if(mm == 1){
        dfs_mm <- all_jmm
      } else{
        dfs_mm <- rbind(all_jmm, dfs_mm)
      }

    } # end of iterations for each management strategy

  step_dec_s <- dfs_mm


  # step increase (true parameter has step increase)
  change_type_x <- "step"
  direction_x <- "increase"

    # get the parameters to use
    if(change_par_x == "K"){

      K_high <- K_A
      K_stepi <- rev(c(rep(K_high,length(1:chng_pt2)),rep(K_B,length((chng_pt2+1):years))))
      K_gradi <- par_for_fun(chng_pt, chng_pt2, years, K_stepi)

      K <- K_stepi; K_for2 <- K_stepi; K_for3 <- K_gradi
      Binit <- K[1]/2

      r <- r_fixed; r_for2 <- r_fixed; r_for3 <- r_fixed
      Uinit <- r[1]/2

      par_A <- as.character(K_B); par_B <- as.character(K_high)
    }

    if(change_par_x == "r"){
      r_high <- r_A
      r_stepi <- rev(c(rep(r_high,length(1:chng_pt2)),rep(r_B,length((chng_pt2+1):years))))
      r_gradi <- par_for_fun(chng_pt, chng_pt2, years, r_stepi)

      K <- K_fixed; K_for2 <- K_fixed; K_for3 <- K_fixed
      Binit <- K[1]/2

      r <- r_stepi; r_for2 <- r_stepi; r_for3 <- r_gradi
      Uinit <- r[1]/2

      par_A <- as.character(r_B); par_B <- as.character(r_high)

    }

    if(change_par_x == "rK"){
      K_high <- K_A
      K_stepi <- rev(c(rep(K_high,length(1:chng_pt2)),rep(K_B,length((chng_pt2+1):years))))
      K_gradi <- par_for_fun(chng_pt, chng_pt2, years, K_stepi)

      r_high <- r_A
      r_stepi <- rev(c(rep(r_high,length(1:chng_pt2)),rep(r_B,length((chng_pt2+1):years))))
      r_gradi <- par_for_fun(chng_pt, chng_pt2, years, r_stepi)

      K <- K_stepi; K_for2 <- K_stepi; K_for3 <- K_gradi
      Binit <- K[1]/2

      r <- r_stepi; r_for2 <- r_stepi; r_for3 <- r_gradi
      Uinit <- r[1]/2

      par_A <- paste(as.character(r_B), as.character(K_B), sep = "_"); par_B <- paste(as.character(r_high), as.character(K_high), sep = "_")

    }

    # run the simulations for each management strategy
    for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # turn results into data frame
      all_jmm <- ts_results_fun(sim_jmm, pars)

      all_jmm$par_A <- rep(par_A, length(all_jmm[,1]))
      all_jmm$par_B <- rep(par_B, length(all_jmm[,1]))

      # join all the results for the mm^th strategy:
      if(mm == 1){
        dfs_mm <- all_jmm
      } else{
        dfs_mm <- rbind(all_jmm, dfs_mm)
      }

    } # end of iterations for each management strategy


  step_inc_s <- dfs_mm

  # grad decrease (true parameter has gradual decrease)
  change_type_x <- "grad"
  direction_x <- "decrease"

    # get the parameters to use
    if(change_par_x == "K"){

      K_low <- K_B
      K_stepd <- c(rep(K_A,length(1:chng_pt2)),rep(K_low,length((chng_pt2+1):years)))
      K_gradd <- par_for_fun(chng_pt, chng_pt2, years, K_stepd)

      K <- K_gradd; K_for2 <- K_stepd; K_for3 <- K_gradd
      Binit <- K[1]/2

      r <- r_fixed; r_for2 <- r_fixed; r_for3 <- r_fixed
      Uinit <- r[1]/2

      par_A <- as.character(K_A); par_B <- as.character(K_low)
    }

    if(change_par_x == "r"){
      r_low <- r_B
      r_stepd <- c(rep(r_A,length(1:chng_pt2)),rep(r_low,length((chng_pt2+1):years)))
      r_gradd <- par_for_fun(chng_pt, chng_pt2, years, r_stepd)

      K <- K_fixed; K_for2 <- K_fixed; K_for3 <- K_fixed
      Binit <- K[1]/2

      r <- r_gradd; r_for2 <- r_stepd; r_for3 <- r_gradd
      Uinit <- r[1]/2

      par_A <- as.character(r_A); par_B <- as.character(r_low)

    }

    if(change_par_x == "rK"){
      K_low <- K_B
      K_stepd <- c(rep(K_A,length(1:chng_pt2)),rep(K_low,length((chng_pt2+1):years)))
      K_gradd <- par_for_fun(chng_pt, chng_pt2, years, K_stepd)

      r_low <- r_B
      r_stepd <- c(rep(r_A,length(1:chng_pt2)),rep(r_low,length((chng_pt2+1):years)))
      r_gradd <- par_for_fun(chng_pt, chng_pt2, years, r_stepd)

      K <- K_gradd; K_for2 <- K_stepd; K_for3 <- K_gradd
      Binit <- K[1]/2

      r <- r_gradd; r_for2 <- r_stepd; r_for3 <- r_gradd
      Uinit <- r[1]/2

      par_A <- paste(as.character(r_A), as.character(K_A), sep = "_"); par_B <- paste(as.character(r_low), as.character(K_low), sep = "_")

    }

    # run the simulations for each management strategy
    for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # turn results into data frame
      all_jmm <- ts_results_fun(sim_jmm, pars)

      all_jmm$par_A <- rep(par_A, length(all_jmm[,1]))
      all_jmm$par_B <- rep(par_B, length(all_jmm[,1]))

      # join all the results for the mm^th strategy:
      if(mm == 1){
        dfs_mm <- all_jmm
      } else{
        dfs_mm <- rbind(all_jmm, dfs_mm)
      }

    } # end of iterations for each management strategy

  grad_dec_s <- dfs_mm

  # grad increase (true parameter has gradual increase)
  change_type_x <- "grad"
  direction_x <- "increase"

    # get the parameters to use
    if(change_par_x == "K"){

      K_high <- K_A
      K_stepi <- rev(c(rep(K_high,length(1:chng_pt2)),rep(K_B,length((chng_pt2+1):years))))
      K_gradi <- par_for_fun(chng_pt, chng_pt2, years, K_stepi)

      K <- K_gradi; K_for2 <- K_stepi; K_for3 <- K_gradi
      Binit <- K[1]/2

      r <- r_fixed; r_for2 <- r_fixed; r_for3 <- r_fixed
      Uinit <- r[1]/2

      par_A <- as.character(K_B); par_B <- as.character(K_high)
    }

    if(change_par_x == "r"){
      r_high <- r_A
      r_stepi <- rev(c(rep(r_high,length(1:chng_pt2)),rep(r_B,length((chng_pt2+1):years))))
      r_gradi <- par_for_fun(chng_pt, chng_pt2, years, r_stepi)

      K <- K_fixed; K_for2 <- K_fixed; K_for3 <- K_fixed
      Binit <- K[1]/2

      r <- r_gradi; r_for2 <- r_stepi; r_for3 <- r_gradi
      Uinit <- r[1]/2

      par_A <- as.character(r_B); par_B <- as.character(r_high)

    }

    if(change_par_x == "rK"){
      K_high <- K_A
      K_stepi <- rev(c(rep(K_high,length(1:chng_pt2)),rep(K_B,length((chng_pt2+1):years))))
      K_gradi <- par_for_fun(chng_pt, chng_pt2, years, K_stepi)

      r_high <- r_A
      r_stepi <- rev(c(rep(r_high,length(1:chng_pt2)),rep(r_B,length((chng_pt2+1):years))))
      r_gradi <- par_for_fun(chng_pt, chng_pt2, years, r_stepi)

      K <- K_gradi; K_for2 <- K_stepi; K_for3 <- K_gradi
      Binit <- K[1]/2

      r <- r_gradi; r_for2 <- r_stepi; r_for3 <- r_gradi
      Uinit <- r[1]/2

      par_A <- paste(as.character(r_B), as.character(K_B), sep = "_"); par_B <- paste(as.character(r_high), as.character(K_high), sep = "_")

    }

    # run the simulations for each management strategy
    for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # turn results into data frame
      all_jmm <- ts_results_fun(sim_jmm, pars)

      all_jmm$par_A <- rep(par_A, length(all_jmm[,1]))
      all_jmm$par_B <- rep(par_B, length(all_jmm[,1]))

      # join all the results for the mm^th strategy:
      if(mm == 1){
        dfs_mm <- all_jmm
      } else{
        dfs_mm <- rbind(all_jmm, dfs_mm)
      }

    } # end of iterations for each management strategy

  grad_inc_s <- dfs_mm

  # periodic update decreases
  # (true parameter has gradual decrease, but gradual adaptive management scenario is periodic updates)

  for(uu in 1:length(update_ints)){
    change_type_x <- "grad" # true parameter is changing gradually
    direction_x <- "decrease"

    # new strategy names and change_refs: only consider the climate adaptive strategy when using periodic updates
    strat_names2 <- paste("update", update_ints[uu], sep = "_")
    change_refs2 <- 2

      # get the parameters to use
      if(change_par_x == "K"){

        K_low <- K_B
        K_stepd <- c(rep(K_A,length(1:chng_pt2)),rep(K_low,length((chng_pt2+1):years)))
        K_gradd <- par_for_fun(chng_pt, chng_pt2, years, K_stepd)
        K_gradd_up <- update_par_fun(chng_pt, chng_pt2, years, K_stepd, update_ints[uu])

        K <- K_gradd; K_for2 <- K_stepd; K_for3 <- K_gradd_up
        Binit <- K[1]/2

        r <- r_fixed; r_for2 <- r_fixed; r_for3 <- r_fixed
        Uinit <- r[1]/2

        par_A <- as.character(K_A); par_B <- as.character(K_low)
      }

      if(change_par_x == "r"){
        r_low <- r_B
        r_stepd <- c(rep(r_A,length(1:chng_pt2)),rep(r_low,length((chng_pt2+1):years)))
        r_gradd <- par_for_fun(chng_pt, chng_pt2, years, r_stepd)
        r_gradd_up <- update_par_fun(chng_pt, chng_pt2, years, r_stepd, update_ints[uu])

        K <- K_fixed; K_for2 <- K_fixed; K_for3 <- K_fixed
        Binit <- K[1]/2

        r <- r_gradd; r_for2 <- r_stepd; r_for3 <- r_gradd_up
        Uinit <- r[1]/2

        par_A <- as.character(r_A); par_B <- as.character(r_low)

      }

      if(change_par_x == "rK"){
        K_low <- K_B
        K_stepd <- c(rep(K_A,length(1:chng_pt2)),rep(K_low,length((chng_pt2+1):years)))
        K_gradd <- par_for_fun(chng_pt, chng_pt2, years, K_stepd)
        K_gradd_up <- update_par_fun(chng_pt, chng_pt2, years, K_stepd, update_ints[uu])

        r_low <- r_B
        r_stepd <- c(rep(r_A,length(1:chng_pt2)),rep(r_low,length((chng_pt2+1):years)))
        r_gradd <- par_for_fun(chng_pt, chng_pt2, years, r_stepd)
        r_gradd_up <- update_par_fun(chng_pt, chng_pt2, years, r_stepd, update_ints[uu])

        K <- K_gradd; K_for2 <- K_stepd; K_for3 <- K_gradd_up
        Binit <- K[1]/2

        r <- r_gradd; r_for2 <- r_stepd; r_for3 <- r_gradd_up
        Uinit <- r[1]/2

        par_A <- paste(as.character(r_A), as.character(K_A), sep = "_"); par_B <- paste(as.character(r_low), as.character(K_low), sep = "_")

      }

      # run the simulations for each management strategy
      # for(mm in 1:length(change_refs2)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs2, delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names2, change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # turn results into data frame
      all_jmm <- ts_results_fun(sim_jmm, pars)

      all_jmm$par_A <- rep(par_A, length(all_jmm[,1]))
      all_jmm$par_B <- rep(par_B, length(all_jmm[,1]))

      # join all the results for the mm^th strategy:
      # if(mm == 1){
      #   dfs_mm <- all_jmm
      # } else{
      #   dfs_mm <- rbind(all_jmm, dfs_mm)
      # }

      dfs_mm <- all_jmm

      # } # end of iterations for each management strategy

    if(uu==1){
      dfs2 <- dfs_mm
    } else{
      dfs2 <- rbind(dfs_mm, dfs2)
    }

  }

  update_dec_s <- dfs2

  # periodic update increases
  # (true parameter has gradual increase, but gradual adaptive management scenario is periodic updates)

  for(uu in 1:length(update_ints)){
    change_type_x <- "grad"
    direction_x <- "increase"

    # new strategy names and change_refs: only consider the climate adaptive strategy when using periodic updates
    strat_names2 <- paste("update", update_ints[uu], sep = "_")
    change_refs2 <- 2

      # get the parameters to use
      if(change_par_x == "K"){

        K_high <- K_A
        K_stepi <- rev(c(rep(K_high,length(1:chng_pt2)),rep(K_B,length((chng_pt2+1):years))))
        K_gradi <- par_for_fun(chng_pt, chng_pt2, years, K_stepi)
        K_gradi_up <- update_par_fun(chng_pt, chng_pt2, years, K_stepi, update_ints[uu])

        K <- K_gradi; K_for2 <- K_stepi; K_for3 <- K_gradi_up
        Binit <- K[1]/2

        r <- r_fixed; r_for2 <- r_fixed; r_for3 <- r_fixed
        Uinit <- r[1]/2

        par_A <- as.character(K_B); par_B <- as.character(K_high)
      }

      if(change_par_x == "r"){
        r_high <- r_A
        r_stepi <- rev(c(rep(r_high,length(1:chng_pt2)),rep(r_B,length((chng_pt2+1):years))))
        r_gradi <- par_for_fun(chng_pt, chng_pt2, years, r_stepi)
        r_gradi_up <- update_par_fun(chng_pt, chng_pt2, years, r_stepi, update_ints[uu])

        K <- K_fixed; K_for2 <- K_fixed; K_for3 <- K_fixed
        Binit <- K[1]/2

        r <- r_gradi; r_for2 <- r_stepi; r_for3 <- r_gradi_up
        Uinit <- r[1]/2

        par_A <- as.character(r_B); par_B <- as.character(r_high)

      }

      if(change_par_x == "rK"){
        K_high <- K_A
        K_stepi <- rev(c(rep(K_high,length(1:chng_pt2)),rep(K_B,length((chng_pt2+1):years))))
        K_gradi <- par_for_fun(chng_pt, chng_pt2, years, K_stepi)
        K_gradi_up <- update_par_fun(chng_pt, chng_pt2, years, K_stepi, update_ints[uu])

        r_high <- r_A
        r_stepi <- rev(c(rep(r_high,length(1:chng_pt2)),rep(r_B,length((chng_pt2+1):years))))
        r_gradi <- par_for_fun(chng_pt, chng_pt2, years, r_stepi)
        r_gradi_up <- update_par_fun(chng_pt, chng_pt2, years, r_stepi, update_ints[uu])

        K <- K_gradi; K_for2 <- K_stepi; K_for3 <- K_gradi_up
        Binit <- K[1]/2

        r <- r_gradi; r_for2 <- r_stepi; r_for3 <- r_gradi_up
        Uinit <- r[1]/2

        par_A <- paste(as.character(r_B), as.character(K_B), sep = "_"); par_B <- paste(as.character(r_high), as.character(K_high), sep = "_")

      }

      # run the simulations for each management strategy
      # for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs2, delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names2, change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # turn results into data frame
      all_jmm <- ts_results_fun(sim_jmm, pars)

      all_jmm$par_A <- rep(par_A, length(all_jmm[,1]))
      all_jmm$par_B <- rep(par_B, length(all_jmm[,1]))

      # join all the results for the mm^th strategy:
      # if(mm == 1){
      #   dfs_mm <- all_jmm
      # } else{
      #   dfs_mm <- rbind(all_jmm, dfs_mm)
      # }

      dfs_mm <- all_jmm

      # } # end of iterations for each management strategy

    if(uu==1){
      dfs2 <- dfs_mm
    } else{
      dfs2 <- rbind(dfs_mm, dfs2)
    }

  }

  update_inc_s <- dfs2

  # combine results
  ts_df <- rbind(step_dec_s, step_inc_s, grad_dec_s, grad_inc_s, update_dec_s, update_inc_s)

  return(ts_df)

}


# function for running the simulations looking at sensitivity to initial biomass
# function of which parameter(s) to change (r, K, rK) and set of initial population sizes (as fractions of initial carrying capacity)
sensB0_fun <- function(change_par_x, Bfrac_set){

  # set up the parameter vectors (this function assumes all of these were defined outside the function)
  # have four change scenarios: step decrease, step increase, grad decrease, and grad increase
  if(change_par_x == "K"){

    # sets of parameter values: 1 = step decrease, 2 = step increase, 3 = grad decrease, 4 = grad increase
    K_set <- list(K_stepd, K_stepi, K_gradd, K_gradi)
    K_for2_set <- list(K_stepd, K_stepi, K_stepd, K_stepi) # abrupt climate adaptive
    K_for3_set <- list(K_gradd, K_gradi, K_gradd, K_gradi) # gradual climate adaptive

    # r values
    r_set <- list(r_fixedd, r_fixedd, r_fixedd, r_fixedd) # r stays at high value for all change scenarios
    # set values for r here since they won't change for the different K scenarios
    r_for2_set <- list(r_fixedd, r_fixedd, r_fixedd, r_fixedd) # no change for abrupt climate adaptive
    r_for3_set <- list(r_fixedd, r_fixedd, r_fixedd, r_fixedd) # no change for gradual climate adaptive

  }

  if(change_par_x == "r"){

    # K values
    K_set <- list(K_fixedd, K_fixedd, K_fixedd, K_fixedd) # K stays at high value
    # set values for K here since they won't change for the different r scenarios
    K_for2_set <- list(K_fixedd, K_fixedd, K_fixedd, K_fixedd) # no change for abrupt climate adaptive
    K_for3_set <- list(K_fixedd, K_fixedd, K_fixedd, K_fixedd) # no change for gradual climate adaptive

    # r values
    r_set <- list(r_stepd, r_stepi, r_gradd, r_gradi)
    r_for2_set <- list(r_stepd, r_stepi, r_stepd, r_stepi) # abrupt climate adaptive
    r_for3_set <- list(r_gradd, r_gradi, r_gradd, r_gradi) # gradual climate adaptive
  }

  if(change_par_x == "rK"){

    # K values
    K_set <- list(K_stepd, K_stepi, K_gradd, K_gradi)
    K_for2_set <- list(K_stepd, K_stepi, K_stepd, K_stepi) # abrupt climate adaptive
    K_for3_set <- list(K_gradd, K_gradi, K_gradd, K_gradi) # gradual climate adaptive

    # r values
    r_set <- list(r_stepd, r_stepi, r_gradd, r_gradi)
    r_for2_set <- list(r_stepd, r_stepi, r_stepd, r_stepi) # abrupt climate adaptive
    r_for3_set <- list(r_gradd, r_gradi, r_gradd, r_gradi) # gradual climate adaptive

  }


  # first scenario: step decrease
  change_type_x <- "step"
  direction_x <- "decrease"

  K <- K_set[[1]]
  K_for2 <- K_for2_set[[1]]
  K_for3 <- K_for3_set[[1]]
  r <- r_set[[1]]
  r_for2 <- r_for2_set[[1]]
  r_for3 <- r_for3_set[[1]]

  Uinit <- r[1]/2

  Binit_set<-K[1]*Bfrac_set # initial biomass -> represents different levels of initial exploitation

  for(j in 1:length(Binit_set)){

    Binit_j <- Binit_set[j] # Binit is the first column of parmat

    # run the simulations for each management strategy
    for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit = Binit_j, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit_j, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # pre, post, and total periods
      tot_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = years)
      pre_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = chng_pt2 - 1)
      post_jmm <- results_fun(sim_jmm, pars, t_start = chng_pt2, t_end = years)

      all_jmm <- rbind(tot_jmm, pre_jmm, post_jmm)

      # join all the results for the mm^th strategy:
      if(mm == 1){
        dfs_mm <- all_jmm
      } else{
        dfs_mm <- rbind(all_jmm, dfs_mm)
      }

    } # end of iterations for each management strategy

    if(j==1){
      dfs <- dfs_mm
    } else{
      dfs <- rbind(dfs_mm, dfs)
    }

  }


  step_dec_s <- dfs


  # second scenario: step increase
  change_type_x <- "step"
  direction_x <- "increase"

  K <- K_set[[2]]
  K_for2 <- K_for2_set[[2]]
  K_for3 <- K_for3_set[[2]]
  r <- r_set[[2]]
  r_for2 <- r_for2_set[[2]]
  r_for3 <- r_for3_set[[2]]

  Uinit <- r[1]/2

  Binit_set<-K[1]*Bfrac_set # initial biomass -> represents different levels of initial exploitation

  for(j in 1:length(Binit_set)){

    Binit_j <- Binit_set[j] # Binit is the first column of parmat

    # run the simulations for each management strategy
    for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit = Binit_j, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit_j, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # pre, post, and total periods
      tot_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = years)
      pre_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = chng_pt2 - 1)
      post_jmm <- results_fun(sim_jmm, pars, t_start = chng_pt2, t_end = years)

      all_jmm <- rbind(tot_jmm, pre_jmm, post_jmm)

      # join all the results for the mm^th strategy:
      if(mm == 1){
        dfs_mm <- all_jmm
      } else{
        dfs_mm <- rbind(all_jmm, dfs_mm)
      }

    } # end of iterations for each management strategy

    if(j==1){
      dfs <- dfs_mm
    } else{
      dfs <- rbind(dfs_mm, dfs)
    }

  }


  step_inc_s <- dfs

  # third scenario: grad decrease
  change_type_x <- "grad"
  direction_x <- "decrease"

  K <- K_set[[3]]
  K_for2 <- K_for2_set[[3]]
  K_for3 <- K_for3_set[[3]]
  r <- r_set[[3]]
  r_for2 <- r_for2_set[[3]]
  r_for3 <- r_for3_set[[3]]

  Uinit <- r[1]/2

  Binit_set<-K[1]*Bfrac_set # initial biomass -> represents different levels of initial exploitation

  for(j in 1:length(Binit_set)){

    Binit_j <- Binit_set[j] # Binit is the first column of parmat

    # run the simulations for each management strategy
    for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit = Binit_j, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit_j, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # pre, post, and total periods
      tot_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = years)
      pre_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = chng_pt2 - 1)
      post_jmm <- results_fun(sim_jmm, pars, t_start = chng_pt2, t_end = years)

      all_jmm <- rbind(tot_jmm, pre_jmm, post_jmm)

      # join all the results for the mm^th strategy:
      if(mm == 1){
        dfs_mm <- all_jmm
      } else{
        dfs_mm <- rbind(all_jmm, dfs_mm)
      }

    } # end of iterations for each management strategy

    if(j==1){
      dfs <- dfs_mm
    } else{
      dfs <- rbind(dfs_mm, dfs)
    }

  }


  grad_dec_s <- dfs


  # fourth scenario: grad increase
  change_type_x <- "grad"
  direction_x <- "increase"

  K <- K_set[[4]]
  K_for2 <- K_for2_set[[4]]
  K_for3 <- K_for3_set[[4]]
  r <- r_set[[4]]
  r_for2 <- r_for2_set[[4]]
  r_for3 <- r_for3_set[[4]]

  Uinit <- r[1]/2

  Binit_set<-K[1]*Bfrac_set# initial biomass -> represents different levels of initial exploitation

  for(j in 1:length(Binit_set)){

    Binit_j <- Binit_set[j] # Binit is the first column of parmat

    # run the simulations for each management strategy
    for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit = Binit_j, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit_j, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # pre, post, and total periods
      tot_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = years)
      pre_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = chng_pt2 - 1)
      post_jmm <- results_fun(sim_jmm, pars, t_start = chng_pt2, t_end = years)

      all_jmm <- rbind(tot_jmm, pre_jmm, post_jmm)

      # join all the results for the mm^th strategy:
      if(mm == 1){
        dfs_mm <- all_jmm
      } else{
        dfs_mm <- rbind(all_jmm, dfs_mm)
      }

    } # end of iterations for each management strategy

    if(j==1){
      dfs <- dfs_mm
    } else{
      dfs <- rbind(dfs_mm, dfs)
    }

  }


  grad_inc_s <- dfs

  # combine results
  sens_df <- rbind(step_dec_s, step_inc_s, grad_dec_s, grad_inc_s)

  return(sens_df)

}


# function for running sensitivity analyses to t_change
sensTchange_fun <- function(change_par_x, Tchange_set){

  # set up the parameter vectors (this function assumes all of these were defined outside the function)
  # have four change scenarios: step decrease, step increase, grad decrease, and grad increase
  if(change_par_x == "K"){

    # make a nested list where outermost is change scenario (step/grad, increase/decrease) and innermost
    # are the values for each t_change value
    K_stepds <- list()
    K_stepis <- list()
    K_gradds <- list()
    K_gradis <- list()
    r_fixedds <- list()

    for(j in 1:length(Tchange_set)){
      chng_pt2 <- Tchange_set[j]

      K_stepds[[j]] <- c(rep(K_A,length(1:chng_pt2)),rep(K_B,length((chng_pt2+1):years)))
      K_gradds[[j]] <- par_for_fun(chng_pt, chng_pt2, years, K_stepds[[j]])
      K_stepis[[j]] <- rev(K_stepds[[j]])
      K_gradis[[j]] <- par_for_fun(chng_pt, chng_pt2, years, K_stepis[[j]])

      r_fixedds[[j]] <- r_fixedd # r stays at high value for all change scenarios

    }

    # sets of parameter values: 1 = step decrease, 2 = step increase, 3 = grad decrease, 4 = grad increase
    K_set <- list(K_stepds, K_stepis, K_gradds, K_gradis)
    K_for2_set <- list(K_stepds, K_stepis, K_stepds, K_stepis) # abrupt climate adaptive
    K_for3_set <- list(K_gradds, K_gradis, K_gradds, K_gradis) # gradual climate adaptive

    r_set <- list(r_fixedds, r_fixedds, r_fixedds, r_fixedds)
    r_for2_set <- list(r_fixedds, r_fixedds, r_fixedds, r_fixedds)
    r_for3_set <- list(r_fixedds, r_fixedds, r_fixedds, r_fixedds)

  }

  if(change_par_x == "r"){

    r_stepds <- list()
    r_stepis <- list()
    r_gradds <- list()
    r_gradis <- list()
    K_fixedds <- list()

    for(j in 1:length(Tchange_set)){
      chng_pt2 <- Tchange_set[j]

      r_stepds[[j]] <- c(rep(r_A,length(1:chng_pt2)),rep(r_B,length((chng_pt2+1):years)))
      r_gradds[[j]] <- par_for_fun(chng_pt, chng_pt2, years, r_stepds[[j]])
      r_stepis[[j]] <- rev(r_stepds[[j]])
      r_gradis[[j]] <- par_for_fun(chng_pt, chng_pt2, years, r_stepis[[j]])

      K_fixedds[[j]] <- K_fixedd # r stays at high value for all change scenarios

    }


    # sets of parameter values: 1 = step decrease, 2 = step increase, 3 = grad decrease, 4 = grad increase
    r_set <- list(r_stepds, r_stepis, r_gradds, r_gradis)
    r_for2_set <- list(r_stepds, r_stepis, r_stepds, r_stepis) # abrupt climate adaptive
    r_for3_set <- list(r_gradds, r_gradis, r_gradds, r_gradis) # gradual climate adaptive

    K_set <- list(K_fixedds, K_fixedds, K_fixedds, K_fixedds)
    K_for2_set <- list(K_fixedds, K_fixedds, K_fixedds, K_fixedds)
    K_for3_set <- list(K_fixedds, K_fixedds, K_fixedds, K_fixedds)

  }

  if(change_par_x == "rK"){

    K_stepds <- list()
    K_stepis <- list()
    K_gradds <- list()
    K_gradis <- list()

    r_stepds <- list()
    r_stepis <- list()
    r_gradds <- list()
    r_gradis <- list()

    for(j in 1:length(Tchange_set)){
      chng_pt2 <- Tchange_set[j]

      K_stepds[[j]] <- c(rep(K_A,length(1:chng_pt2)),rep(K_B,length((chng_pt2+1):years)))
      K_gradds[[j]] <- par_for_fun(chng_pt, chng_pt2, years, K_stepds[[j]])
      K_stepis[[j]] <- rev(K_stepds[[j]])
      K_gradis[[j]] <- par_for_fun(chng_pt, chng_pt2, years, K_stepis[[j]])

      r_stepds[[j]] <- c(rep(r_A,length(1:chng_pt2)),rep(r_B,length((chng_pt2+1):years)))
      r_gradds[[j]] <- par_for_fun(chng_pt, chng_pt2, years, r_stepds[[j]])
      r_stepis[[j]] <- rev(r_stepds[[j]])
      r_gradis[[j]] <- par_for_fun(chng_pt, chng_pt2, years, r_stepis[[j]])

    }

    # K values
    K_set <- list(K_stepds, K_stepis, K_gradds, K_gradis)
    K_for2_set <- list(K_stepds, K_stepis, K_stepds, K_stepis) # abrupt climate adaptive
    K_for3_set <- list(K_gradds, K_gradis, K_gradds, K_gradis) # gradual climate adaptive

    # r values
    r_set <- list(r_stepds, r_stepis, r_gradds, r_gradis)
    r_for2_set <- list(r_stepds, r_stepis, r_stepds, r_stepis) # abrupt climate adaptive
    r_for3_set <- list(r_gradds, r_gradis, r_gradds, r_gradis) # gradual climate adaptive

  }


  # first scenario: step decrease
  change_type_x <- "step"
  direction_x <- "decrease"

  Ks <- K_set[[1]]
  K_for2s <- K_for2_set[[1]]
  K_for3s <- K_for3_set[[1]]
  rs <- r_set[[1]]
  r_for2s <- r_for2_set[[1]]
  r_for3s <- r_for3_set[[1]]

  for(j in 1:length(Tchange_set)){

    chng_pt2 <- Tchange_set[j]
    chng_time2 <- chng_pt2

    K <- Ks[[j]]
    K_for2 <- K_for2s[[j]]
    K_for3 <- K_for3s[[j]]
    r <- rs[[j]]
    r_for2 <- r_for2s[[j]]
    r_for3 <- r_for3s[[j]]

    Uinit <- r[1]/2
    Binit<-K[1]/2 # initial biomass -> represents different levels of initial exploitation

    # run the simulations for each management strategy
    for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # pre, post, and total periods
      tot_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = years)
      pre_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = chng_pt2 - 1)
      post_jmm <- results_fun(sim_jmm, pars, t_start = chng_pt2, t_end = years)

      all_jmm <- rbind(tot_jmm, pre_jmm, post_jmm)

      # join all the results for the mm^th strategy:
      if(mm == 1){
        dfs_mm <- all_jmm
      } else{
        dfs_mm <- rbind(all_jmm, dfs_mm)
      }

    } # end of iterations for each management strategy

    if(j==1){
      dfs <- dfs_mm
    } else{
      dfs <- rbind(dfs_mm, dfs)
    }

  }


  step_dec_s <- dfs


  # second scenario: step increase
  change_type_x <- "step"
  direction_x <- "increase"

  Ks <- K_set[[2]]
  K_for2s <- K_for2_set[[2]]
  K_for3s <- K_for3_set[[2]]
  rs <- r_set[[2]]
  r_for2s <- r_for2_set[[2]]
  r_for3s <- r_for3_set[[2]]

  for(j in 1:length(Tchange_set)){

    chng_pt2 <- Tchange_set[j]
    chng_time2 <- chng_pt2

    K <- Ks[[j]]
    K_for2 <- K_for2s[[j]]
    K_for3 <- K_for3s[[j]]
    r <- rs[[j]]
    r_for2 <- r_for2s[[j]]
    r_for3 <- r_for3s[[j]]

    Uinit <- r[1]/2
    Binit<-K[1]/2 # initial biomass -> represents different levels of initial exploitation


    # run the simulations for each management strategy
    for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # pre, post, and total periods
      tot_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = years)
      pre_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = chng_pt2 - 1)
      post_jmm <- results_fun(sim_jmm, pars, t_start = chng_pt2, t_end = years)

      all_jmm <- rbind(tot_jmm, pre_jmm, post_jmm)

      # join all the results for the mm^th strategy:
      if(mm == 1){
        dfs_mm <- all_jmm
      } else{
        dfs_mm <- rbind(all_jmm, dfs_mm)
      }

    } # end of iterations for each management strategy

    if(j==1){
      dfs <- dfs_mm
    } else{
      dfs <- rbind(dfs_mm, dfs)
    }

  }


  step_inc_s <- dfs

  # third scenario: grad decrease
  change_type_x <- "grad"
  direction_x <- "decrease"

  Ks <- K_set[[3]]
  K_for2s <- K_for2_set[[3]]
  K_for3s <- K_for3_set[[3]]
  rs <- r_set[[3]]
  r_for2s <- r_for2_set[[3]]
  r_for3s <- r_for3_set[[3]]

  for(j in 1:length(Tchange_set)){

    chng_pt2 <- Tchange_set[j]
    chng_time2 <- chng_pt2

    K <- Ks[[j]]
    K_for2 <- K_for2s[[j]]
    K_for3 <- K_for3s[[j]]
    r <- rs[[j]]
    r_for2 <- r_for2s[[j]]
    r_for3 <- r_for3s[[j]]

    Uinit <- r[1]/2
    Binit<-K[1]/2 # initial biomass -> represents different levels of initial exploitation


    # run the simulations for each management strategy
    for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # pre, post, and total periods
      tot_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = years)
      pre_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = chng_pt2 - 1)
      post_jmm <- results_fun(sim_jmm, pars, t_start = chng_pt2, t_end = years)

      all_jmm <- rbind(tot_jmm, pre_jmm, post_jmm)

      # join all the results for the mm^th strategy:
      if(mm == 1){
        dfs_mm <- all_jmm
      } else{
        dfs_mm <- rbind(all_jmm, dfs_mm)
      }

    } # end of iterations for each management strategy

    if(j==1){
      dfs <- dfs_mm
    } else{
      dfs <- rbind(dfs_mm, dfs)
    }

  }


  grad_dec_s <- dfs


  # fourth scenario: grad increase
  change_type_x <- "grad"
  direction_x <- "increase"

  Ks <- K_set[[4]]
  K_for2s <- K_for2_set[[4]]
  K_for3s <- K_for3_set[[4]]
  rs <- r_set[[4]]
  r_for2s <- r_for2_set[[4]]
  r_for3s <- r_for3_set[[4]]

  for(j in 1:length(Tchange_set)){

    chng_pt2 <- Tchange_set[j]
    chng_time2 <- chng_pt2

    K <- Ks[[j]]
    K_for2 <- K_for2s[[j]]
    K_for3 <- K_for3s[[j]]
    r <- rs[[j]]
    r_for2 <- r_for2s[[j]]
    r_for3 <- r_for3s[[j]]

    Uinit <- r[1]/2
    Binit<-K[1]/2 # initial biomass -> represents different levels of initial exploitation


    # run the simulations for each management strategy
    for(mm in 1:length(change_refs)){

      sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2,
                          K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars,
                          use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons)

      # pre, post, and total periods
      tot_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = years)
      pre_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = chng_pt2 - 1)
      post_jmm <- results_fun(sim_jmm, pars, t_start = chng_pt2, t_end = years)

      all_jmm <- rbind(tot_jmm, pre_jmm, post_jmm)

      # join all the results for the mm^th strategy:
      if(mm == 1){
        dfs_mm <- all_jmm
      } else{
        dfs_mm <- rbind(all_jmm, dfs_mm)
      }

    } # end of iterations for each management strategy

    if(j==1){
      dfs <- dfs_mm
    } else{
      dfs <- rbind(dfs_mm, dfs)
    }

  }


  grad_inc_s <- dfs

  # combine results
  sens_df <- rbind(step_dec_s, step_inc_s, grad_dec_s, grad_inc_s)

  return(sens_df)

}




# function for running all of the model extension simulations:
# arguments:
#change_par_x = which parameter is changing (r, K, or both)
#change_type_x = type of change (step or grad)
#direction_x  = direction of change (increase or decrease)
# K = true carrying capacity
# r = true growth rate
# r_for2 = "forecast" growth rate for abrupt climate adaptive
# r_for3 = "forecast" growth rate for gradual climate adaptive
# r_for3_up = "forecast" growth rate for gradual climate adaptive w/ periodic updates
# K_for2 = "forecast" carrying capacity for abrupt climate adaptive
# K_for3 = "forecast" carrying capacity for gradual climate adaptive
# K_for3_up = "forecast" carrying capacity for gradual climate adaptive w/ periodic updates
# Binit = initial biomass

ext_fun <- function(change_par_x, change_type_x, direction_x, K, r, r_for2, r_for3, r_for3_up, K_for2,
                    K_for3, K_for3_up, Binit){

  for(mm in 1:length(change_refs)){ # for each management strategy

    # deterministic

    sim_jmm <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2, K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars, use_HCR=TRUE)

    pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                 change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                 years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                 error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons, K_A = K[1], K_B = K[length(K)])

    # pre, post, and total periods
    tot_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = years)
    pre_jmm <- results_fun(sim_jmm, pars, t_start = 1, t_end = chng_pt2 - 1)
    post_jmm <- results_fun(sim_jmm, pars, t_start = chng_pt2, t_end = years)

    all_jmm <- rbind(tot_jmm, pre_jmm, post_jmm)

    # join all the results for the mm^th strategy:
    if(mm == 1){
      det_mm <- all_jmm
    } else{
      det_mm <- rbind(all_jmm, det_mm)
    }


    # stochastic
    rep_list <- list() # empty holding lists for all the replicates

    for(i in 1:nreps){

      rep_list[[i]] <- proj_pop(years, all_errors[[i]], r, r_for2, r_for3, Binit, Uinit, K, K_for2, K_for3, change_ref = change_refs[mm], delta_set, p, HCR_fun, HCR_fixed_pars, use_HCR=TRUE)

      pars <- list(hcr_type = "slope", strategy = strat_names[mm], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = rho, error_cv = cv, error_rep = i, B_cons = B_cons, K_A = K[1], K_B = K[length(K)])

      tot_i <- results_fun(rep_list[[i]], pars, t_start = 1, t_end = years)
      pre_i <- results_fun(rep_list[[i]], pars, t_start = 1, t_end = chng_pt2 - 1)
      post_i <- results_fun(rep_list[[i]], pars, t_start = chng_pt2, t_end = years)

      all_i <- rbind(tot_i, pre_i, post_i)

      if(i == 1){
        stoch_all <- all_i
      } else{
        stoch_all <- rbind(all_i, stoch_all)
      }

    } # end of iterations over different stochastic realizations

    # summarize across replicates: mean and 95% CIs
    stoch_summ <- stoch_all %>% group_by(period, hcr_type, strategy, change_par, change_type, direction, B_cons, Binit, chng_time2, discount_rate, error_rho, error_cv, metric) %>% summarize(mean = mean(value, na.rm = T), Q025 = quantile(value, 0.025, na.rm = T), Q975 = quantile(value, 0.975, na.rm = T))

    # join all the results for the mm^th strategy:
    if(mm == 1){
      stoch_mm <- stoch_summ
    } else{
      stoch_mm <- rbind(stoch_summ, stoch_mm)
    }

  } # end of iterations for each management strategy


  # process the deterministic and stochastic data frames and rbind them together
  det_dt <- det_mm %>% mutate(Q025 = value, Q975 = value) %>% mutate(errors = "N") %>% select(-error_rep)
  stoch_dt <- stoch_mm %>% mutate(errors = "Y") %>% rename(value = mean)

  ext_dt1 <- rbind(det_dt, stoch_dt)

  # now run simulations with the alternative HCRs and periodic update management scenario (note the latter is really more like
  # an alternative management strategy but I'm just including it under the HCR column for these summary figures)
  for(hh in 1:length(HCR_set)){

    if(HCR_set[hh] %in% c("constant_F", "constant_Esc")){ # for these two, use the normal forecast parameters

      # deterministic
      sim_h <- proj_pop(years, no_errors, r, r_for2, r_for3, Binit, Uinit, K, K_for2, K_for3, change_ref = change_refs[1], delta_set, p, HCR_fun = HCR_funs[[hh]], HCR_fixed_pars, use_HCR=TRUE)

      pars <- list(hcr_type = HCR_set[hh], strategy = strat_names[1], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons, K_A = K[1], K_B = K[length(K)])

      # pre, post, and total periods
      tot_h <- results_fun(sim_h, pars, t_start = 1, t_end = years)
      pre_h <- results_fun(sim_h, pars, t_start = 1, t_end = chng_pt2 - 1)
      post_h <- results_fun(sim_h, pars, t_start = chng_pt2, t_end = years)

      # stochastic
      rep_list <- list() # empty holding lists for all the replicates

      for(i in 1:nreps){

        rep_list[[i]] <- proj_pop(years, all_errors[[i]], r, r_for2, r_for3, Binit, Uinit, K, K_for2, K_for3, change_ref = change_refs[1], delta_set, p, HCR_fun = HCR_funs[[hh]], HCR_fixed_pars, use_HCR=TRUE)

        pars <- list(hcr_type = HCR_set[hh], strategy = strat_names[1], change_par = change_par_x,
                     change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                     years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                     error_rho = rho, error_cv = cv, error_rep = i, B_cons = B_cons, K_A = K[1], K_B = K[length(K)])

        tot_i <- results_fun(rep_list[[i]], pars, t_start = 1, t_end = years)
        pre_i <- results_fun(rep_list[[i]], pars, t_start = 1, t_end = chng_pt2 - 1)
        post_i <- results_fun(rep_list[[i]], pars, t_start = chng_pt2, t_end = years)

        all_i <- rbind(tot_i, pre_i, post_i)

        if(i == 1){
          stoch_all <- all_i
        } else{
          stoch_all <- rbind(all_i, stoch_all)
        }

      } # end of iterations over different stochastic realizations

    }

    if(HCR_set[hh]=="update"){ # need to use the forecast parameters with periodic updates

      # deterministic
      sim_h <- proj_pop(years, no_errors, r, r_for2, r_for3_up, Binit, Uinit, K, K_for2, K_for3_up, change_ref = change_refs[3], delta_set, p, HCR_fun = HCR_funs[[hh]], HCR_fixed_pars, use_HCR=TRUE)

      pars <- list(hcr_type = HCR_set[hh], strategy = strat_names[3], change_par = change_par_x,
                   change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                   years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                   error_rho = NA, error_cv = NA, error_rep = NA, B_cons = B_cons, K_A = K[1], K_B = K[length(K)])

      # pre, post, and total periods
      tot_h <- results_fun(sim_h, pars, t_start = 1, t_end = years)
      pre_h <- results_fun(sim_h, pars, t_start = 1, t_end = chng_pt2 - 1)
      post_h <- results_fun(sim_h, pars, t_start = chng_pt2, t_end = years)

      # stochastic
      rep_list <- list() # empty holding lists for all the replicates

      for(i in 1:nreps){

        rep_list[[i]] <- proj_pop(years, all_errors[[i]], r, r_for2, r_for3_up, Binit, Uinit, K, K_for2, K_for3_up, change_ref = change_refs[3], delta_set, p, HCR_fun = HCR_funs[[hh]], HCR_fixed_pars, use_HCR=TRUE)

        pars <- list(hcr_type = HCR_set[hh], strategy = strat_names[3], change_par = change_par_x,
                     change_type = change_type_x, direction = direction_x, chng_pt2 = chng_pt2,
                     years = years, Binit = Binit, chng_time2 = chng_time2, discount_rates = delta_set,
                     error_rho = rho, error_cv = cv, error_rep = i, B_cons = B_cons, K_A = K[1], K_B = K[length(K)])

        tot_i <- results_fun(rep_list[[i]], pars, t_start = 1, t_end = years)
        pre_i <- results_fun(rep_list[[i]], pars, t_start = 1, t_end = chng_pt2 - 1)
        post_i <- results_fun(rep_list[[i]], pars, t_start = chng_pt2, t_end = years)

        all_i <- rbind(tot_i, pre_i, post_i)

        if(i == 1){
          stoch_all <- all_i
        } else{
          stoch_all <- rbind(all_i, stoch_all)
        }

      } # end of iterations over different stochastic realizations


    }

    # join all the determinisitc results
    all_h <- rbind(tot_h, pre_h, post_h)

    # for stochastic results, summarize across replicates: mean and 95% CIs
    stoch_summ <- stoch_all %>% group_by(period, hcr_type, strategy, change_par, change_type, direction, B_cons, Binit, chng_time2, discount_rate, error_rho, error_cv, metric) %>% summarize(mean = mean(value, na.rm = T), Q025 = quantile(value, 0.025, na.rm = T), Q975 = quantile(value, 0.975, na.rm = T))



    # join all the results for the h^th harvest control rule
    if(hh == 1){
      det_h <- all_h
    } else{
      det_h <- rbind(all_h, det_h)
    }

    # join all the results for the mm^th strategy:
    if(hh == 1){
      stoch_h <- stoch_summ
    } else{
      stoch_h <- rbind(stoch_summ, stoch_h)
    }

  } # end of iterations for each HCR


  det_dt <- det_h %>% mutate(Q025 = value, Q975 = value) %>% mutate(errors = "N") %>% select(-error_rep)
  stoch_dt <- stoch_h %>% mutate(errors = "Y") %>% rename(value = mean)

  ext_dt2 <- rbind(det_dt, stoch_dt)


  all_dt <- rbind(ext_dt1, ext_dt2)

  return(all_dt)

}
