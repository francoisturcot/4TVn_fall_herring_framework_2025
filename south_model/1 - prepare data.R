
library(wham) 
library(tidyverse)

#predators
#pred = read.csv("../../../predators/predators_input.csv")
#pred = read.csv("../../../../predators/pred_for_wham_nov15.csv")



input1 <- prepare_wham_input(asap) 

basic_info <- list(
    n_stocks = 1L,
    n_seasons = 1L,
    n_fleets = 1L,
    ages = 1:input1$data$n_ages,
    n_fleets = input1$data$n_fleets,
    fracyr_SSB = cbind(input1$data$fracyr_SSB), #(n_years x n_stocks)
    maturity = input1$data$mature, #(n_stocks x n_years x n_ages)
    years = as.integer(input1$years), #(n_years)
    waa = input1$data$waa, #(any no. x n_years x n_ages)
    waa_pointer_ssb = input1$data$waa_pointer_ssb, #(n_stocks)
    spawn_regions = 1, #(n_stocks)
    spawn_seasons = 1 #(n_stocks)
)

catch_info <- list(
    n_fleets = NCOL(input1$data$agg_catch), #(n_fleets)
    agg_catch = cbind(input1$data$agg_catch), #(n_years x n_fleets)
    agg_catch_cv = cbind(sqrt(exp(input1$data$agg_catch_sigma^2) - 1)), #(n_years x n_fleets)
    catch_paa = input1$data$catch_paa, #(n_fleets x n_years x n_ages)
    use_catch_paa = cbind(input1$data$use_catch_paa), #(n_years x n_fleets), 0: don't use, 1: use
    catch_Neff = cbind(input1$data$catch_Neff), #(n_years x n_fleets)
    selblock_pointer_fleets = cbind(input1$data$selblock_pointer_fleets), #(n_years x n_fleets)
    waa_pointer_fleets = input1$data$waa_pointer_fleets, #(n_fleets)
    fleet_regions = rep(1,input1$data$n_fleets) #(n_fleets)
)

index_info <- list(
    n_indices = NCOL(input1$data$agg_indices),
    agg_indices = cbind(input1$data$agg_indices), #(n_years x n_indices)
    units_indices = input1$data$units_indices, #(n_indices) 1: biomass 2: numbers
    units_index_paa = input1$data$units_index_paa, #(n_indices) 1: biomass 2: numbers
    agg_index_cv = cbind(sqrt(exp(input1$data$agg_index_sigma^2) - 1)), #(n_years x n_indices)
    index_Neff = cbind(input1$data$index_Neff), #(n_years x n_indices)
    fracyr_indices = cbind(input1$data$fracyr_indices), #(n_years x n_indices)
    use_indices = cbind(input1$data$use_indices), #(n_years x n_indices)
    use_index_paa = cbind(input1$data$use_index_paa),  #(n_years x n_indices)
    index_paa = input1$data$index_paa,  #(n_indices x n_years x n_ages)
    selblock_pointer_indices = cbind(input1$data$selblock_pointer_indices), #(n_years x n_indices)
    waa_pointer_indices = input1$data$waa_pointer_indices, #(n_indices)
    index_regions = rep(1,input1$data$n_indices)) #(n_indices)
