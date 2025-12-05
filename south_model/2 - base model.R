#south 

library(scales)
library(tidyverse)
library(wham)

#read data file
asap <- read_asap3_dat("DATA_RV.DAT")

#prepare data
source("1 - prepare data.R")

#scale indices and init q
index_info$agg_indices
#SG
#RV
index_info$agg_indices[,1] = index_info$agg_indices[,1]*10

catchability = list(re = c("none", "none"), 
                    initial_q = c(0.2, 0.2)
                   )

#replace zeroes by small values to avoid log(0)
catch_info$agg_catch    <- pmax(catch_info$agg_catch, 1e-6)

#initial values for annual fully-selected fishing mortality
F_info <- list(F = matrix(0.3, length(basic_info$years), catch_info$n_fleets)) #(n_years x n_fleets)

#NAA
NAA_info = list(
  N1_model = "age-specific-fe",
  #sigma = "rec+1",
  sigma = "rec",
  #cor = "iid",
  bias_correct_process = F,
  recruit_model = 1) 

#age_comp 
age_comp = list(fleets = "dirichlet-miss0", indices = c("logistic-normal-miss0", "logistic-normal-miss0"))

#M
M_info = list(mean_model = "fixed-M",
              re_model    = matrix(c("iid_y"), nrow = 1, ncol = 1),
              initial_means = array(c(0.3), dim = c(1,1,10)),
              re_map = array(c(NA,NA,NA,NA,NA,1,1,1,1,1), dim = c(1, 1, 10))
)

#selectivity
catch_info$selblock_pointer_fleets
index_info$selblock_pointer_indices

#SG - maturity schedule fixed
#rv - logistic estimated 

sel_info <- list(model = c("age-specific", "age-specific", "logistic"), 
                 n_selblocks = 3,
                 re = c("ar1_y", "none", "none"),
                 fix_pars = list(
                                 c(8:10),
                                 c(1:10),
                                 NULL),
                 initial_pars = list(
                     c(0.001, 0.004, 0.057, 0.234, 0.536, 0.797, 0.928, 0.999, 0.999, 0.999),
                     
                     c(0.03,0.48,0.96,1,1,1,1,1,1,1),
                     c(3,0.2)
                 )
)

#set reference points years
basic_info$XSPR_input_average_years = c(1:5)

# agg cv
index_info$agg_index_cv[,1] = 0.2 #sg
index_info$agg_index_cv[,2] = 0.3 #rv
catch_info$agg_catch_cv[,1] = 0.2 #fixed+mobile catch

#prepare input
input <- prepare_wham_input(
  basic_info = basic_info,
  catch_info = catch_info, 
  index_info = index_info,
  selectivity =sel_info,
  NAA_re = NAA_info,
  M = M_info,
  F = F_info, 
  catchability = catchability,
  age_comp = age_comp,
  ecov = NULL
  
)

input$fleet_names =  c("Fixed + Mobile")
input$index_names = c("SG", "RV")

#fit
base <- fit_wham(input, 
                 do.fit = T,
                 do.retro = F,
                 do.sdrep = T,
                 n.peels = 4,
                 do.brps = F, 
                 do.osa = F, 
                 MakeADFun.silent = F, 
                 
                 #integer, number of additional Newton steps after optimization. Passed to fit_tmb. Default = 3.
                 n.newton = 6,
                 #Default is list(use.optim = FALSE, opt.control = list(iter.max = 1000, eval.max = 1000)
                 fit.tmb.control = list(opt.control = list(iter.max = 2000, eval.max = 2000))) 

check_convergence(base)
est = check_estimability(base)
estt = est$BadParams
estt

plot_wham_output(mod=base, out.type='html', plot.opts = list(ages.lab = c("2","3","4","5","6","7","8","9","10","11+" )))



therep = base$report()

sapply(grep("nll",names(therep),value=T), function(x) sum(therep[[x]]))

