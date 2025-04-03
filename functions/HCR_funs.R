# functions for each of the harvest control rules that can be input into the function for simulating the model

# sliding/sloped HCR (original)

# function of current biomass B, carrying capacity K (either true or effective depending on strategy),
# and max harvest rate U_max (e.g., U_MSY, which could either be based on true or effective r based on
# strategy), and the alpha parameter alpha_HCR controlling the steepness of the HCR
# all of these arguments are combined in a list called HCR_pars, which is created within the function for simulating the model

slope_HCR <- function(HCR_pars){

  B <- HCR_pars$B
  K <- HCR_pars$K
  U_max <- HCR_pars$U_max
  alpha_HCR <- HCR_pars$alpha_HCR

  BMSY<-K/2

  # calculate actual harvest rate as a function of target rate (U_max) and B/B_MSY
  if(B/BMSY < 0.2){
    U_t <- 0
  }
  if(B/BMSY >= 0.20 & B/BMSY<1){
    U_t <- (U_max*(B/BMSY - alpha_HCR))/(1-alpha_HCR)
  }

  if(B/BMSY >=1){
    U_t <- U_max
  }


  C_t <- U_t*B

  return(C_t) # return harvest rate at time t x biomass at time t

}


# constant F
# function of current biomass B and chosen harvest rate U_max (e.g., U_MSY)

constF_HCR <- function(HCR_pars){

  B <- HCR_pars$B
  U_max <- HCR_pars$U_max

  U_t <- U_max

  C_t <- U_t*B

  return(C_t) # return catch at time t (=harvest rate at time t x biomass at time t)

}


# constant escapement
# function of current biomass B and target stock size B_target (e.g., B_MSY, or slightly less than
# B_MSY if more precautionary)

constEsc_HCR <- function(HCR_pars){

  B <- HCR_pars$B
  B_target <- HCR_pars$B_target

  C_t <- max(0, B-B_target)

  return(C_t) # return catch at time t

}



# catch scalars: haven't worked this out yet
# function of the current biomass, carrying capacity (=unfished biomass), catch summary statistic C_hat. and the scalars for each of the 3 exploitation levels (theta1, theta2, theta3)

# catch_scalar_HCR <- function(HCR_pars){
#
#   B <- HCR_pars$B
#   K <- HCR_pars$K
#   C_hat <- HCR_pars$C_hat
#   theta1 <- HCR_pars$theta1
#   theta2 <- HCR_pars$theta2
#   theta3 <- HCR_pars$theta3
#
#   if(B/K < 0.19){
#       C_t <- theta1*C_hat
#     }
#     if(B/K >= 0.19 & B/K<0.65){
#       C_t <- theta2*C_hat
#     }
#
#     if(B/K >=0.65){
#       C_t <- theta3*C_hat
#     }
#
#     C_t <- min(B, C_t) # make sure total catch isn't greater than current biomass
#
#       return(C_t) # return catch at time t
# }






