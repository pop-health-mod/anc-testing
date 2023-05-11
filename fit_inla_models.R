##########################################
####
#### fit the INLA models
print("Fitting Model 1")
mod1 <- inla(model_1,
             data = mod_dat, family = "binomial",
             Ntrials = mod_dat$N,
             control.predictor=list(link = 1),#compute=TRUE),
             control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T))
print("Fitting Model 2")
mod2 <- inla(model_2,
             data = mod_dat, family = "binomial",
             Ntrials = mod_dat$N,
             control.predictor=list(link = 1),#compute=TRUE),
             control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T))
print("Fitting Model 3")
mod3 <- inla(model_3,
             data = mod_dat, family = "binomial",
             Ntrials = mod_dat$N,
             control.predictor=list(link = 1),#compute=TRUE),
             control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T))
print("Fitting Model 4")
mod4 <- inla(model_4,
             data = mod_dat, family = "binomial",
             Ntrials = mod_dat$N,
             control.predictor=list(link = 1),#compute=TRUE),
             control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T))
print("Fitting Model 5")
mod5 <- inla(model_5,
             data = mod_dat, family = "binomial",
             Ntrials = mod_dat$N,
             control.predictor=list(link = 1),#compute=TRUE),
             control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T))
print("Fitting Model 6")
mod6 <- inla(model_6,
             data = mod_dat, family = "binomial",
             Ntrials = mod_dat$N,
             control.predictor=list(link = 1),#compute=TRUE),
             control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T))

print("Fitting Model 7")
mod7 <- inla(model_7,
             data = mod_dat, family = "binomial",
             Ntrials = mod_dat$N,
             control.predictor=list(link = 1),#compute=TRUE),
             control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T))

print("Fitting Model 8")
mod8 <- inla(model_8,
             data = mod_dat, family = "binomial",
             Ntrials = mod_dat$N,
             control.predictor=list(link = 1),#compute=TRUE),
             control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T))

print("Fitting Model 9")
mod9 <- inla(model_9,
             data = mod_dat, family = "binomial",
             Ntrials = mod_dat$N,
             control.predictor=list(link = 1),#compute=TRUE),
             control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T))