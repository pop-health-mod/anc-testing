####################################################################################################
## Author: Adrien Allorant
## Last updated: February 2023
## Description: Look at variations in structure of population in each country by survey-year.
##              We approximate the relative weight of each 'cell' (residence x edu x age x wealth x birth)
##              in each country, by looking at its survey weight relative to the total weights in a given
##              survey or in a census.
##
## Inputs:      Data from all available surveys and censuses in a given country
##              function to perform interpolation and raking (for census data as they do not include info on wealth)
##
## Outputs:     interpolated_population.rds a dataset that contains interpolated populations for
##              all countries with at least two year of data (surveys or censuses, or combined) from 2005 to 2022
####################################################################################################

rm(list=ls())

dhs <- readRDS("Surveydata/DHS_populationinfo.rds")
all_non_dhs <- readRDS("TestPM20Model/combined-test-data.rds")
census_pop <- readRDS("census_pop.rds")

library(zoo)
library(magrittr)
library(data.table)
library(tidyverse)
library(tweenr)

# function to interpolate populations
source('interp_pop_with_raking.R')
source('raking_function.R')

pop <- dhs  %>%
  filter(!duplicated(id_full))%>%
  mutate(
    surveytool = gsub("[^A-Z ]","",str_sub(surveyid, -4, -1)),
    birthgr = case_when(
      birthgr == "0" ~ 1,
      birthgr == "1-2" ~ 1,
      birthgr == "3-5" ~ 2,
      birthgr == "6-9" ~ 3,
      birthgr == "10-19" ~ 4,
    ),
    edu = case_when(
      edu == 0 ~ 1,
      edu == 1 ~ 2,
      edu == 2 ~ 3,
      edu == 3 ~ 4
    ),
    agegr = case_when(
      agegr == "15-19" ~ 1,
      agegr == "20-24" ~ 2,
      agegr == "25-29" ~ 3,
      agegr == "30-34" ~ 4,
      agegr == "35-39" ~ 5,
      agegr == "40-44" ~ 6,
      agegr == "45-49" ~ 7,
    ),
    restype = case_when(
      restype == "urban" ~ 1,
      restype == "rural" ~ 2
    )
  ) %>%
  bind_rows(all_non_dhs  %>%
              mutate(
                surveytool = gsub("[^A-Z ]","",str_sub(surveyid, -4, -1)),
                birthgr = case_when(
                  birthgr == "0" ~ 1,
                  birthgr == "1-2" ~ 1,
                  birthgr == "3-5" ~ 2,
                  birthgr == "6-9" ~ 3,
                  birthgr == "10-19" ~ 4,
                ),
                edu = case_when(
                  edu == 0 ~ 1,
                  edu == 1 ~ 2,
                  edu == 2 ~ 3,
                  edu == 3 ~ 4
                ),
                agegr = case_when(
                  agegr == "15-19" ~ 1,
                  agegr == "20-24" ~ 2,
                  agegr == "25-29" ~ 3,
                  agegr == "30-34" ~ 4,
                  agegr == "35-39" ~ 5,
                  agegr == "40-44" ~ 6,
                  agegr == "45-49" ~ 7,
                ),
                restype = case_when(
                  restype == "urban" ~ 1,
                  restype == "rural" ~ 2
                )
              ) %>%
              filter(!(surveytool %in% c("DHS","BSSM")))) %>%
  filter(!is.na(agegr) & !is.na(birthgr) & !is.na(edu) & !is.na(restype))

pop<-setDT(pop)
pop[, year := as.numeric(survyear)]
surveypop <- pop[,list(n = sum(indweight)),by='surveyid,surveytool,sector,country,year']
surveypop[, datatype:= 'Survey']
census <- census_pop %>%
  dplyr::select(country, year, pop_tot) %>%
  distinct() %>%
  left_join(surveypop %>% dplyr::select(country,sector) %>% distinct()) %>%
  filter(!is.na(sector))
plotDF <- census[, list(n = sum(pop_tot)), by = 'sector,country,year']
plotDF[, surveyid:= NA]; plotDF[, surveytool := 'Census']
plotDF[, datatype := 'Census']

# Normalize weights 
pop <- pop %>%
  group_by(country, surveyid) %>%
  mutate(
    prop = indweight/sum(indweight),
    year = as.numeric(survyear)) %>%
  data.table() 

## Recalculate wealth quintiles by region and urban/rural
mean_wealth_area <- pop[, list(mean_wealth = mean(wealths, na.rm = T)),
                        by= 'surveyid,region,restype']
pop <- merge(pop, mean_wealth_area, by = c('surveyid','region','restype'))
pop[,wealths_adj := wealths - mean_wealth]
pop[,wealthq_adj := ntile(wealths_adj, 5), by = c('surveyid','region','restype')]

pop_survey_no_wealthq_adj <- pop[,list(prop = sum(prop)), by = 'sector,country,surveyid,year,restype,agegr,edu,birthgr']
pop_survey_age_res <- pop[,list(prop = sum(prop)), by = 'sector,country,surveyid,year,restype,agegr']
pop_survey <- pop[,list(prop = sum(prop)), by = 'sector,country,surveyid,year,restype,agegr,edu,birthgr,wealthq_adj']
pop_wealthq_adj <- pop[,list(prop = sum(prop)), by = 'sector,country,surveyid,year,wealthq_adj']
pop_wealthq_adj_res <- pop[,list(prop = sum(prop)), by = 'sector,country,surveyid,year,wealthq_adj,restype']
pop_wealthq_adj_edu <- pop[,list(prop = sum(prop)), by = 'sector,country,surveyid,year,wealthq_adj,edu']

saveRDS(pop_survey, file = 'TestPM20Model/all_svy_pop.RDS')


popTOT <- pop_survey %>%
  dplyr::select(sector,country, year, restype, agegr, edu, birthgr, prop, wealthq_adj) %>%
  mutate(data = "survey") %>%
  rbind(
    census_pop %>%
      dplyr::select(country, year, restype, agegr, edu, birthgr, prop) %>%
      mutate(data = "census",
             wealthq_adj = NA) %>%
      left_join(pop_survey %>%
                  dplyr::select(sector,country) %>%
                  distinct())
  ) 

popTOT_no_wealthq_adj <- pop_survey_no_wealthq_adj %>%
  dplyr::select(sector, country, year, restype, agegr, edu, birthgr, prop) %>%
  mutate(data = "survey") %>%
  rbind(
    census_pop %>%
      dplyr::select(country, year, restype, agegr, edu, birthgr, prop) %>%
      mutate(data = "census")%>%
      left_join(pop_survey_no_wealthq_adj %>%
                  dplyr::select(sector,country) %>%
                  distinct())
  ) 

n_cntry <- length(unique(popTOT$country))
country <- unique(as.character(popTOT$country))
final <- NULL

#-------------- Population interpolation --------#

popTOT<-setDT(popTOT)
setnames(popTOT, 'wealthq_adj','wealth')
for (i in unique(popTOT$country)) {
  tmp <- popTOT[country == i,]
  if(tmp %>%
     filter(!is.na(wealth)) %>%
     dplyr::select(year) %>%
     distinct() %>%
     nrow() < 2) {cat('\n', i);next}
  else
    final <- rbind(final, interpolate_missing_years(tmp) %>%
                     mutate(country = i))
}



saveRDS(final, file = 'interpolated_population_dhs_census.rds')
