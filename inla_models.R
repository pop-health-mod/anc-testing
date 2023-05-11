############################
# -- set up some models -- #

#################################################################################
### Models to consider for recent HIV testing ##
#################################################################################

model_1 <- Y ~ 1 + f(country_num, model = 'iid', hyper = prec.prior) + restype + factor(agegr) + 
  factor(birthgr) +
  factor(wealth) + factor(edu) + f(country_num2, time, model = "iid", hyper = prec.prior) 

model_2 <- Y ~ 1 +  f(country_num, model = 'iid', hyper = prec.prior)  + restype +  f(agegr, model = 'iid', hyper = prec.prior) +  
  factor(wealth) + factor(edu)+ 
  factor(birthgr) +
  f(year_num, model="iid", hyper = prec.prior) +
  f(year_num2, model = 'rw1', scale.model = T)

model_3 <- Y ~ 1 +  f(country_num, model = 'iid', hyper = prec.prior)  + restype + f(agegr, model = 'iid', hyper = prec.prior) + 
  factor(wealth) + factor(edu)+ time + 
  factor(birthgr) + f(country_num2, time, model = "iid", hyper = prec.prior) +
  f(year_num, model="iid", hyper = prec.prior) +
  f(year_num2, model = 'rw1', scale.model = T)

model_4 <-  Y ~ 1 +  f(country_num, model = 'iid', hyper = prec.prior)  + restype + f(agegr, model = 'iid', hyper = prec.prior) + 
  factor(wealth) + factor(edu)+ factor(birthgr) +
  f(year_num, model="iid", hyper = prec.prior) +
  f(year_num2, model = 'rw1', scale.model = T) +
  f(survey_type, model="iid", hyper = prec.prior, extraconstr = list(A = A, e = e))

model_5 <-  Y ~ 1 +  f(country_num, model = 'iid', hyper = prec.prior)  + restype + f(agegr, model = 'iid', hyper = prec.prior) + 
  factor(wealth) + factor(edu)+ factor(birthgr) +
  f(country_num2, time, model = "iid", hyper = prec.prior) +
  f(year_num, model="iid", hyper = prec.prior) +
  f(year_num2, model = 'rw1', scale.model = T) +
  f(survey_type, model="iid", hyper = prec.prior, extraconstr = list(A = A, e = e))

model_6 <-  Y ~ 1 +  f(country_num, model = 'iid', hyper = prec.prior)  + restype + f(agegr, model = 'iid', hyper = prec.prior) + 
  factor(wealth) + factor(edu)+ factor(birthgr) +
  f(year_num, model="iid", hyper = prec.prior) +
  f(year_num2, model = 'rw1', scale.model = T) +
  f(year_num3, model = 'rw1', scale.model = T, replicate=country_num)+
  f(survey_type, model="iid", hyper = prec.prior, extraconstr = list(A = A, e = e))

model_7 <-  Y ~ 1 +  f(country_num, model = 'iid', hyper = prec.prior)  + restype + f(agegr, model = 'iid', hyper = prec.prior) + 
  factor(wealth) + factor(edu)+ factor(birthgr) +
  f(country_num2, time, model = "iid", hyper = prec.prior) +
  f(year_num, model="iid", hyper = prec.prior) +
  f(year_num2, model = 'rw1', scale.model = T) +
  f(year_num3, model = 'rw1', scale.model = T, replicate=country_num) +
  f(survey_type, model="iid", hyper = prec.prior, extraconstr = list(A = A, e = e))

model_8 <-  Y ~ 1 +  f(country_num, model = 'iid', hyper = prec.prior)  + restype + 
  f(agegr, model = 'iid', hyper = prec.prior) + 
  f(agegr2, model = 'rw1', replicate=country_num) + 
  factor(wealth) + factor(edu)+ factor(birthgr) +
  f(year_num, model="iid", hyper = prec.prior) +
  f(year_num2, model = 'rw1', scale.model = T) +
  f(year_num3, model = 'rw1', scale.model = T, replicate=country_num)+
  f(survey_type, model="iid", hyper = prec.prior, extraconstr = list(A = A, e = e))

model_9 <-  Y ~ 1 + factor(sector_num) + f(country_num, model = 'iid', hyper = prec.prior)  + restype + 
  f(agegr, model = 'iid', hyper = prec.prior) + 
  f(agegr2, model = 'rw1', replicate=country_num) +
  factor(wealth) + factor(edu)+ factor(birthgr) +
  f(country_num2, time, model = "iid", hyper = prec.prior) +
  f(year_num, model="iid", hyper = prec.prior) +
  f(year_num2, model = 'rw1', scale.model = T) +
  f(year_num3, model = 'rw1', scale.model = T, replicate=country_num)+
  f(survey_type, model="iid", hyper = prec.prior, extraconstr = list(A = A, e = e))
