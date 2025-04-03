Function directory:

# model_funs.R

## proj_pop
- function for simulating the model

## error_fun
- function for generating errors for stochastic model simulations

## par_for_fun
- function for getting the vectors of r and/or K values in the gradual change scenarios

## update_par_fun
- function for getting the vectors of r and/or K values in the gradual change scenarios with periodic updates

# HCR_funs.R

## slope_HCR
- function for the sloped HCR (default)

## constF_HCR
- function for the constant harvest rate HCR

## constEsc_HCR
- function for the constant escapement HCR

# results_funs.R

## results_fun
- function for calculating the summary statistics (mean biomass, cumulative harvest, etc.) from the simulations and returning them as dataframe for ggplot plotting

## summ_dfs

- function for calculating percent differences in summary statistics (mean biomass, cumulative harvest, etc.) across the different management strategies

## tdiff_fun
- function for calculating the years during which each of the management strategies differ from one another in terms of biomass and harvest

# sim_funs.R

## sensB0_fun
- function for running simulations with different initial biomass values to test sensitivity of results to this parameter

## sensABCD_fun
- function for running simulations with different percent changes in K, r, or both to test sensitivity to the magnitude of the change in these parameters

## ext_fun
- function for running all of the extension simulations (stochastic and deterministic, different HCRs, adding the discount factor for revenue, adding the biomass conservation limit)

# plot_abcd_main_fun.R
- a script for working on changes to the function gg_ABCD_sens_plot_gcsq()
## gg_ABCD_sens_plot_gcsq
- makes plots showing differences in mean biomass and cumulative harvest for different percent changes in K, r, or both for all grad decrease/increase scenarios pre

# plot_funs.R

## gg_sens_plot
- function to plot the results of the original sensitivity analyses (mean biomass, cumulative harvest, etc. for different initial biomasses and gradual course correction timing) for a specified scenario (e.g., step decrease in K)

## gg_sens_plot2 
- same as gg_sens_plot function but just plotting effect of initial biomass, not course correction timing

## gg_ABCD_sens_plot
- makes plots showing differences in mean biomass and cumulative harvest for different percent changes in K, r, or both for all step/grad decrease/increase scenarios

## gg_ABCD_sens_plot_gcsq
- makes plots showing differences in mean biomass and cumulative harvest for different percent changes in K, r, or both for all grad decrease/increase scenarios pre

## gg_ABCD_pre_byscenario
- combines plots made with gg_ABCD_sens_plot_gcsq() into multipanel figure by scenario (change in K, r, rK)

## gg_ABCD_sens_Bmn_Hcm_plot_gcsq
- makes plots showing relationships between mean biomass (x axis) and cumulative harvest (y axis) for different percent changes in K, r, or both for all grad decrease/increase scenarios pre

## gg_ABCD_pre_Bmn_Hcm_byscenario
- combines plots made with gg_ABCD_sens_Bmn_Hcm_plot_gcsq() into multipanel figure by scenario (change in K, r, rK)

## gg_ext_plot
- function to plot the results of the extension analyses (mean biomass, cumulative harvest, etc. for deterministic and stochastic simulations, with different HCRs, etc.)

## helper_ts
- function for plotting "helper" timeseries of biomass, harvest rate, and harvest

## tdiff_plot
- function for making plots showing the time window over which each of the management strategies differ from one another in terms of biomass and harvest
