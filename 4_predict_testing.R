####################################################################################################
## Author:      Adrien Allorant
##
## Description: Read model fit and predict recent HIV testing on interpolated populations
##              to perform post-stratification.
##
## Requires:    model fit
##              population frame to perform post-stratification
##
## Outputs:     three RDS file with yearly estimates by 1) country, 2) sub-regions
##              3) all of sub-Saharan Africa.
##              
####################################################################################################

rm(list=ls())
id_folder <- 'aa' # Adrien

if (id_folder == 'clh') {setwd("C:/Users/Caroline/Documents/HIV Testing Project/hiv-testing-modalities")}
if (id_folder == 'mnicomputer') { setwd("E:/HIV testing modalities")}
if (id_folder == 'aa') {setwd("/Users/adrienallorant/Documents/McGill/Research/hiv-testing-modalities")}
if (id_folder == 'pm') { setwd("~/Files/ANC Testing")}
if (id_folder == 'mmg') { setwd("~/Google Drive/McGill/Research/HTS-modalities/hiv-testing-modalities")}

library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)

load("TestPM20Model/ModelFits.RDATA")
  
draws <- rstan::extract(fit)
rm(fit)

# reading in interpolated population of women by country, year, and socio-demographic groups (location of residence, age, wealth, education, births)

weighted_pop <- readRDS(file = 'interpolated_population_dhs_census.rds')

  # for cnt with just one survey (i.e., Gabon and Guinea-Bissau) we repeat structure every year
  all_svy <- setDT(readRDS('TestPM20Model/all_svy_pop.RDS'))[country %in% c('Gabon','Guinea-Bissau')] 
  all_svy <- all_svy[complete.cases(all_svy),]
  setnames(all_svy, 'wealthq_adj','wealth')
  
  gab <- all_svy[country == 'Gabon', lapply(.SD, function(x) approx(x=year, y = x, rule = 2, method = "constant", xout = max(all_svy$year[all_svy$country == 'Gabon']):2021)$y),
                   by = c('agegr','birthgr','restype','edu','wealth'), .SDcols = 'prop']
  gab <- gab[, year := rep(max(all_svy$year[all_svy$country == 'Gabon']):2021, times= 627)]
  gab_pre <- all_svy[country == 'Gabon', lapply(.SD, function(x) approx(x=year, y = x, rule = 2, method = "constant", 
                                                   xout = 2005:min(all_svy$year[all_svy$country == 'Gabon']))$y),
                  by = c('agegr','birthgr','restype','edu','wealth'), .SDcols = 'prop']
  gab_pre[, year := rep(2005:min(all_svy$year[all_svy$country == 'Gabon']), times= 627)]
  gab_pre[, country := 'Gabon']
  gab[, country := 'Gabon']
  gnb <- all_svy[country == 'Guinea-Bissau', lapply(.SD, function(x) approx(x=year, y = x, rule = 2, method = "constant", xout = max(all_svy$year[all_svy$country == 'Guinea-Bissau']):2021)$y),
                 by = c('agegr','birthgr','restype','edu','wealth'), .SDcols = 'prop']
  gnb <- gnb[, year := rep(max(all_svy$year[all_svy$country == 'Guinea-Bissau']):2021, times= 568)]
  gnb_pre <- all_svy[country == 'Guinea-Bissau', lapply(.SD, function(x) approx(x=year, y = x, rule = 2, method = "constant", 
                                                                        xout = 2005:min(all_svy$year[all_svy$country == 'Guinea-Bissau']))$y),
                     by = c('agegr','birthgr','restype','edu','wealth'), .SDcols = 'prop']
  gnb_pre[, year := rep(2005:min(all_svy$year[all_svy$country == 'Guinea-Bissau']), times= 568)]
  gnb_pre[, country := 'Guinea-Bissau']
  gnb[, country := 'Guinea-Bissau']
  gab_gnb <- bind_rows(gab_pre,gab,gnb_pre,gnb)
  setnames(gab_gnb, 'prop', 'Prop_interp')
  
  # we append these two countries to all others
  weighted_pop <- rbind(weighted_pop, gab_gnb)

# Get country population totals from UN WPP
dat_year <- unique(weighted_pop[, c("country", "year")])

# reading in population projections from WPP
pops <- fread("WPP2022_PopulationByAge5GroupSex_Medium.csv")

# Need to match some names
pops[Location == "Democratic Republic of the Congo", ]$Location <- "Congo Democratic Republic"
pops[Location == "Côte d'Ivoire", ]$Location <- "Cote d'Ivoire"
pops[Location == "United Republic of Tanzania", ]$Location <- "Tanzania"

pops_cntry <- pops %>%
  filter(Location %in% dat_year[["country"]] & Time %in% dat_year[["year"]]) %>%
  filter(AgeGrpStart %in% c(5*3:9)) %>%
  dplyr::rename(country = Location,
         year = Time) %>%
  right_join(dat_year) %>%
  dplyr::group_by(country, year) %>%
  dplyr::summarise(population = sum(PopFemale))

country_match <- unique(mod_dat[,c('country','country_num')])
country_sector_match <- unique(mod_dat[,c('country_num','sector','sector_num')])

# Merge populations and calculate weight
df_totals <- weighted_pop %>%
  filter(country %in% unique(mod_dat$country)) %>%
  left_join(pops_cntry) %>%
  dplyr::group_by(country, year) %>%
  mutate(weight = Prop_interp * population)

# Reformatting the data to create matrices of 0s and 1s
df_totals <- df_totals %>%
  filter(year %in% 2005:2021) %>%
  left_join(country_match) %>%
  left_join(country_sector_match) %>%
  mutate(
    r2 = ifelse(restype==2, 1, 0),
    w2 = ifelse(wealth==2, 1, 0),
    w3 = ifelse(wealth==3, 1, 0),
    w4 = ifelse(wealth==4, 1, 0),
    w5 = ifelse(wealth==5, 1, 0),
    e2 = ifelse(edu==2, 1, 0),
    e3 = ifelse(edu==3, 1, 0),
    e4 = ifelse(edu==4, 1, 0),
    a1 = ifelse(agegr==1, 1, 0),
    a2 = ifelse(agegr==2, 1, 0),
    a3 = ifelse(agegr==3, 1, 0),
    a4 = ifelse(agegr==4, 1, 0),
    a5 = ifelse(agegr==5, 1, 0),
    a6 = ifelse(agegr==6, 1, 0),
    a7 = ifelse(agegr==7, 1, 0),
    b2 = ifelse(birthgr==2, 1, 0),
    b3 = ifelse(birthgr==3, 1, 0),
    b4 = ifelse(birthgr==4, 1, 0),
    C1 = ifelse(country_num==1, 1, 0),
    C2 = ifelse(country_num==2, 1, 0),
    C3 = ifelse(country_num==3, 1, 0),
    C4 = ifelse(country_num==4, 1, 0),
    C5 = ifelse(country_num==5, 1, 0),
    C6 = ifelse(country_num==6, 1, 0),
    C7 = ifelse(country_num==7, 1, 0),
    C8 = ifelse(country_num==8, 1, 0),
    C9 = ifelse(country_num==9, 1, 0),
    C10 = ifelse(country_num==10, 1, 0),
    C11 = ifelse(country_num==11, 1, 0),
    C12 = ifelse(country_num==12, 1, 0),
    C13 = ifelse(country_num==13, 1, 0),
    C14 = ifelse(country_num==14, 1, 0),
    C15 = ifelse(country_num==15, 1, 0),
    C16 = ifelse(country_num==16, 1, 0),
    C17 = ifelse(country_num==17, 1, 0),
    C18 = ifelse(country_num==18, 1, 0),
    C19 = ifelse(country_num==19, 1, 0),
    C20 = ifelse(country_num==20, 1, 0),
    C21 = ifelse(country_num==21, 1, 0),
    C22 = ifelse(country_num==22, 1, 0),
    C23 = ifelse(country_num==23, 1, 0),
    C24 = ifelse(country_num==24, 1, 0),
    C25 = ifelse(country_num==25, 1, 0),
    C26 = ifelse(country_num==26, 1, 0),
    C27 = ifelse(country_num==27, 1, 0),
    C28 = ifelse(country_num==28, 1, 0),
    C29 = ifelse(country_num==29, 1, 0),
    C30 = ifelse(country_num==30, 1, 0),
    C31 = ifelse(country_num==31, 1, 0),
    C32 = ifelse(country_num==32, 1, 0),
    C33 = ifelse(country_num==33, 1, 0),
    C34 = ifelse(country_num==34, 1, 0),
    C35 = ifelse(country_num==35, 1, 0),
    C36 = ifelse(country_num==36, 1, 0),
    C37 = ifelse(country_num==37, 1, 0),
    C38 = ifelse(country_num==38, 1, 0),
    C39 = ifelse(country_num==39, 1, 0),
    C40 = ifelse(country_num==40, 1, 0),
    C41 = ifelse(country_num==41, 1, 0),
    R2 = ifelse(sector_num==2, 1, 0),
    R3 = ifelse(sector_num==3, 1, 0),
    R4 = ifelse(sector_num==4, 1, 0))
  
rm(weighted_pop, pops, pops_cntry, dat_year)

  df_betas <- dplyr::select(df_totals,
                            r2, 
                            w2, w3, w4, w5, 
                            e2, e3, e4, 
                            b2, b3, b4,
                            a2, a3, a4, a5, a6, a7)

df_alphas <- dplyr::select(df_totals, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14, C15, C16, C17, C18, C19, C20,
                           C21, C22, C23, C24, C25, C26, C27, C28, C29, C30, C31, C32, C33, C34, C35, C36, C37, C38, C39, C40,C41)


alpha_s <- data.matrix(df_alphas)[,-(1:2)]
beta_s <- data.matrix(df_betas)[,-(1:2)]

alpha_n <- alpha_s %*% t(draws$a_c)
beta_n <- beta_s %*% t(draws$betas)
kappa_n <- alpha_s %*% t(draws$delta_cnt)
a_t <- draws$a_t
time <- unique(dat_stan$time)

  cst_n <- alpha_n + beta_n
  rm(alpha_n, beta_n)

# creating empty object
year <- c(2005:2021)
df_prd_cnt <- NULL
df_prd_reg <- NULL
df_prd_all <- NULL
cnt_id <- unique(df_totals$country_num)
reg_id <- unique(df_totals$sector)
draws_cnt <- NULL
all_prob <- NULL

for (t in 1:length(year)) {
    year_c <- year[t] - min(year)

      prob <- plogis(a_t[,t] +
                       cst_n[which(df_totals$year == year[t]),] + 
                       rep(year_c,sum(df_totals$year == year[t])) * kappa_n[which(df_totals$year == year[t]),])# inverse logit to get our probabilities
      
    
    wgt_probs <- colSums((prob * df_totals$weight[which(df_totals$year == year[t])]) / sum(df_totals$weight[which(df_totals$year == year[t])]))
    qtl <- quantile(wgt_probs, probs = c(0.5, 0.025, .1, .9, 0.975))
    
    df_prd_all_i <- data.frame(strata = "all", year = year[t], median = qtl[1], lci = qtl[2],
                               `10%` = qtl[3], `90%` = qtl[4], uci = qtl[5])
    df_prd_all <- rbind(df_prd_all, df_prd_all_i)
    
    for (j in 1:length(cnt_id)) {
      df_cnt_j <- df_totals[which(df_totals$country_num == cnt_id[j] & df_totals$year == year[t]),]

        prob_j <-plogis(a_t[,t] +
                          cst_n[which(df_totals$country_num == cnt_id[j] & df_totals$year == year[t]), ] +
                          rep(year_c,sum(df_totals$country_num == cnt_id[j] & df_totals$year == year[t])) * kappa_n[which(df_totals$country_num == cnt_id[j] & df_totals$year == year[t]), ])
        
      wgt_probs <- colSums((prob_j * df_cnt_j$weight) / sum(df_cnt_j$weight))
      qtl <- quantile(wgt_probs, probs = c(0.5, 0.025, .1, .9, 0.975))
      df_prd_cnt_i <- data.frame(country = df_cnt_j$country[1], country_num = cnt_id[j], year = year[t], median = qtl[1], lci = qtl[2],
                                 `10%` = qtl[3], `90%` = qtl[4], uci = qtl[5])
      df_prd_cnt <- rbind(df_prd_cnt, df_prd_cnt_i)
    }
    for (z in levels(reg_id)) {
      df_reg_z <- df_totals[which(df_totals$sector == z & df_totals$year == year[t]),]

        prob_z <-plogis(a_t[,t] + 
                          cst_n[which(df_totals$sector == z & df_totals$year == year[t]), ] +
                          rep(year_c,sum(df_totals$sector == z & df_totals$year == year[t])) * kappa_n[which(df_totals$sector == z & df_totals$year == year[t]), ])

      wgt_probs <- colSums((prob_z * df_reg_z$weight) / sum(df_reg_z$weight)) # just relative prop
      qtl <- quantile(wgt_probs, probs = c(0.5, 0.025, .1, .9, 0.975))
      df_prd_reg_i <- data.frame(region = df_reg_z$sector[1], year = year[t], median = qtl[1], lci = qtl[2],
                                 `10%` = qtl[3], `90%` = qtl[4], uci = qtl[5])
      df_prd_reg <- rbind(df_prd_reg, df_prd_reg_i)
    }
    print(year[t])
  }

  saveRDS(df_prd_cnt, file = paste0('Pred_cnt.RDS'))
  saveRDS(df_prd_reg, file = paste0('Pred_reg.RDS'))
  saveRDS(df_prd_all, file = paste0('Pred_all.RDS'))


