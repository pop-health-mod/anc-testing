###############################################################################################################################
## Author: Adrien Allorant
## Description: reformatting the data to have aggregated counts of outcome and denominator by country, survey, year, and
##              socio-demographic characteristics.
## 
## Input: root (where the Github repo is saved locally)
##        combined survey data from DHS, PHIA, MICS, AIS, MIS, and country-specific (SABSSM, KAIS, BAIS)
## Output: A dataset with aggregated counts of women who were tested for HIV in the last 24 months aggregated by country, survey, year
##        age, sex, birth groups, location of residence, education, and wealth.
##############################################################################################################################

rm(list=ls())

library(data.table)
library(dplyr)
library(stringr)

# read in the data
dat <- readRDS("TestPM20Model/combined-test-data.rds")

# re-categorize birth groups

dat$birthgr <- cut(dat$births, 
                   breaks = c(0, 2, 4, 6, Inf),
                   include.lowest = TRUE,
                   labels = c("<2", "2-3", "4-5", "6+"))

#reorganize the data
dat_testpm20 <- dat %>%
  filter(age < 50) %>% # only keep women aged 15-49
  filter(!is.na(test_pm20)) %>% # with information on the outcome
  filter(!is.na(wealth)) %>% # and each of the covariate included in the model
  filter(!is.na(edu)) %>%
  filter(!is.na(restype)) %>% 
  filter(!is.na(agegr)) %>% 
  filter(!is.na(birthgr)) %>%
  mutate(caseid = factor(as.numeric(factor(caseid))),
         restype = factor(as.numeric(factor(restype))),
         hivstatus = factor(ifelse(is.na(hivstatus), 3, hivstatus)),
         wealth = factor(wealth),
         edu = factor(as.numeric(edu)),
         agegr = factor(as.numeric(factor(agegr))),
         birthgr = factor(as.numeric(factor(birthgr))),
         sector = factor(sector),
         country = factor(country),
         survyear = as.numeric(survyear),
         survyear_c = survyear - mean(survyear))

# ---- Recode new variables ----# 
# it is good practice to convert to numeric before
dat_testpm20$sector_num <- as.numeric(dat_testpm20$sector)
dat_testpm20$country_num <- as.numeric(dat_testpm20$country)
dat_testpm20$psu_num <- as.numeric(dat_testpm20$caseid)

dat_testpm20 <- setDT(dat_testpm20)
dat_testpm20[,survey_type := sapply(str_split(surveyid,"[0-9]+"), "[[", 2)]

## Calculating the household relative wealth quintiles by taking out the mean wealth score by stratum (region and urban/rural)

mean_wealth_area <- dat_testpm20[, list(mean_wealth = mean(wealths, na.rm = T)),
                                     by= 'country,surveyid,region,restype']
dat_testpm20 <- merge(dat_testpm20, mean_wealth_area, by = c('country','surveyid','region','restype'))
dat_testpm20[,wealths_adj := wealths - mean_wealth]
dat_testpm20[,wealthq_adj := ntile(wealths_adj, 5), by = c('country','surveyid','region','restype')]

## Re-calculating weights to account for unequal probability of sampling using Kish's effective sample size formula
weights <- dat_testpm20[, list(M =sum(!is.na(indweight)),
                                   Ms = sum(indweight, na.rm = T)^2/sum(indweight^2, na.rm = T),
                                   w = sum(indweight, na.rm = T),
                                   m_w = mean(indweight, na.rm = T)),
                            by = 'country,surveyid,region,restype']

dat_testpm20 <- merge(dat_testpm20, weights,by = c('country','surveyid','region','restype'))
dat_testpm20[,indweight_k:= (indweight/m_w)*(M/Ms)]

# Only keep the variables needed
keep_vars <- c("survyear", "country",'country_num','sector','sector_num',"survey_type", "surveyid",
               "restype", "agegr", "edu", "wealthq_adj","birthgr",
               'indweight_k','test_pm20')

dat_testpm20 <- dat_testpm20[, keep_vars, with=F]
setnames(dat_testpm20,'wealthq_adj','wealth')

###############################
## Aggregate indicator value ##
###############################

## Aggregate data by country, survey, year and socio-demographic groups

data_aggregated <- dat_testpm20[, list(Y = round(sum(test_pm20*indweight_k, na.rm = T)),
                                      N = round(sum(as.numeric(!is.na(test_pm20))*indweight_k,na.rm = T))),
                               by = 'sector,sector_num,country,country_num,surveyid,survey_type,survyear,agegr,restype,edu,wealth,birthgr']

saveRDS(data_aggregated, file = "TestPM20Model/df_gr_testpm20_cov.rds")
