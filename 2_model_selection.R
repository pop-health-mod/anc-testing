####################################################################################################
## Author:      Adrien Allorant
##
## Description: Read processed survey data and
##              fit 9 different mixed models for recent HIV testing.
##              The 9 models include different combinations
##              of random effects for time, space, survey instrument and covariates. 
##              Several goodness of fit criteria including DIC, LCPO
##              and WAIC are calculated.
##
## Requires:    inla_models.R a script including all 9 models to be run in INLA
##              fit_inla_models.R a script that runs the 9 models
##
## Outputs:     a CSV file indicating the value
##              of each of the 3 criteria for the 9 models.
##              
####################################################################################################

rm(list=ls())

library(data.table)
library(dplyr)
library(tidyverse)
library(INLA)

# read in the data
data_model <- readRDS("TestPM20Model/df_gr_testpm20_cov.rds")

All<-setDT(arrange(data_model,survyear))
All[, time := scale(as.numeric(survyear))]
All[, year := as.numeric(survyear)]
All[, year_num := as.numeric(year)- min(as.numeric(year))+1]
mod_dat<-All

# create extra terms for random effects and interaction terms
mod_dat$country_num7<-mod_dat$country_num6<-mod_dat$country_num5<-mod_dat$country_num4<-mod_dat$country_num3<-mod_dat$country_num2<-mod_dat$country_num
mod_dat$year_num3<-mod_dat$year_num2<-mod_dat$year_num
mod_dat$agegr3<-mod_dat$agegr2<-mod_dat$agegr

##################################
#### -- Set some priors --- ######
##################################

# Set prior on precision
prec.prior <- list(prec = list(param = c(0.001, 0.001)))

# creating the survey RE sum-to-one constraint
A <- matrix(1, ncol = length(unique(All$survey_type[!is.na(All$survey_type)])), nrow = 1)
e <- matrix(0, ncol = 1)

#######################################
# -- fit the model to HIV outcomes-- #
#######################################

source("TestPM20Model/inla_models.R")
if(no_wealth){
  source("TestPM20Model/inla_models_no_wealth.R")
}
source("TestPM20Model/fit_inla_models.R")

#####################################
# --- look at model performance --- #
#####################################
# looking for low DIC and WAIC and high sum(log(cpo))
res<-rbind(cbind(mod1$dic$dic,mod1$dic$p.eff,-sum(log(mod1$cpo$cpo),na.rm=T),mod1$waic$waic),
           cbind(mod2$dic$dic,mod2$dic$p.eff,-sum(log(mod2$cpo$cpo),na.rm=T),mod2$waic$waic),
           cbind(mod3$dic$dic,mod3$dic$p.eff,-sum(log(mod3$cpo$cpo),na.rm=T),mod3$waic$waic),
           cbind(mod4$dic$dic,mod4$dic$p.eff,-sum(log(mod4$cpo$cpo),na.rm=T),mod4$waic$waic),
           cbind(mod5$dic$dic,mod5$dic$p.eff,-sum(log(mod5$cpo$cpo),na.rm=T),mod5$waic$waic), 
           cbind(mod6$dic$dic,mod6$dic$p.eff,-sum(log(mod6$cpo$cpo),na.rm=T),mod6$waic$waic),
           cbind(mod7$dic$dic,mod7$dic$p.eff,-sum(log(mod7$cpo$cpo),na.rm=T),mod7$waic$waic), 
           cbind(mod8$dic$dic,mod8$dic$p.eff,-sum(log(mod8$cpo$cpo),na.rm=T),mod8$waic$waic),
           cbind(mod9$dic$dic,mod9$dic$p.eff,-sum(log(mod9$cpo$cpo),na.rm=T),mod9$waic$waic))


res<-as.data.frame(res)
names(res)<-c("DIC","p_eff","sum_log_cpo","WAIC")
res$model<-1:9

write_csv(res,"TestPM20Model/ModelSelection.csv")

