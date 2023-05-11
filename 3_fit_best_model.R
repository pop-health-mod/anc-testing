####################################################################################################
## Author:      Adrien Allorant
##
## Description: Fit best model identified through the CSV file including the values 
#               of the 3 criteria for the 9 models
##              save the model with the highest performance across these criteria.
##              If criteria points to different models as best models, we use a majority rule.
##              
## Requires:    a CSV file indicating the value
##              of each of the 3 criteria for the 9 models.
##
## Outputs:     a .RDATA file including the model specification, fit, and data used.
####################################################################################################

rm(list=ls())
library(data.table)
library(dplyr)
library(tidyverse)
library(INLA)


data_model <- readRDS("TestPM20Model/df_gr_testpm20_cov.rds")

lookup_cnt <- unique(df[c("sector_num", "country_num")])[, "sector_num"]
lookup_svy <- unique(df[c("country_num", "survey_num")])[, "country_num"]

df <- data_model

dat_stan <- list(N = nrow(df), 
                 C = length(unique(df$country_num)), 
                 R = length(unique(df$sector_num)),
                 S = length(unique(df$survey_num)),
                 num = df$num,
                 den = df$den,
                 survey_idx = df$survey_num,
                 country_idx = df$country_num,
                 region_idx = df$sector_num,
                 time = as.numeric(df$survyear-min(df$survyear)),
                 year_idx = as.numeric(df$survyear-min(df$survyear))+1,
                 Y = length(unique(min(df$survyear)):unique(max(df$survyear))),
                 covariates = covariates,
                 n_c = ncol(covariates),
                 lookup_cnt = lookup_cnt,
                 lookup_svy = lookup_svy)

#######################################################
# SAMPLE FROM POSTERIOR FOR FULL MODEL
#######################################################

save.fit <- T
niter     = 4000
warmup    = 1000
chains = 4

stan_model <- rstan::stan_model(paste0('TestPM20Model/stan_models/',model,'.stan'))
options(mc.cores = parallel::detectCores())

fit <- rstan::sampling(stan_model,
                       data    = dat_stan,
                       control = list(max_treedepth=15,adapt_delta=0.95),
                       iter    = niter, 
                       warmup  = warmup,
                       refresh = 50,
                       thin    = 1, 
                       chains  = nchains)


###############################
# -- save the model output -- #
###############################
if (save.fit) save(fit, dat_stan, df, file="TestPM20Model/ModelFits.RDATA")

