interpolate_missing_years <- function(df){
  
  # re-initialize Q
  dat <- NULL
  dat2 <- NULL
  dat3 <- NULL
  dat4 <- NULL
  pre_dat <- NULL
  post_dat <- NULL
  
  # 
  n_census <- 0
  if("census" %in% unique(df$data)) {
  n_census <- df %>%
        filter(data == "census") %>%
        dplyr::select(year) %>%
       distinct() %>%
       nrow()

     if(unique(df$year[df$data=="census"]) %in% unique(df$year[df$data=="survey"])){
      overlap_year <- unique(df$year[df$data=="census"])[unique(df$year[df$data=="census"]) %in% unique(df$year[df$data=="survey"])]
       df <- df %>%
        filter(data != "survey" | (data == "survey" & year != overlap_year))
     }
   }
  dat<-df %>%
      filter(data == "survey") %>%
      filter(!is.na(wealth)) %>%
      group_by(agegr, restype,edu,birthgr,wealth) %>% 
      # filter(year %in% c(min(year),max(year))) %>%
      complete(year = seq(min(year),max(year))) %>%
      tween_fill("linear") %>%
      rename(Prop_interp = prop) %>%
      left_join(df) %>%
      data.table()
  
  
  if(min(dat$year) > 2005){
      dat2<-data.table(dat)
    
    # can only keep combination with several observations
    dat2 <- dat2 %>%
      mutate(id = paste(restype, edu, agegr, birthgr, wealth))
    
    dat2 <- dat2 %>%
      filter(!is.na(data)) %>%
      dplyr::select(year,id) %>%
      distinct() %>%
      group_by(id) %>%
      summarize(
        n = n()
      ) %>%
      filter(n > 1) %>%
      left_join(dat2) %>%
      dplyr::select(-c(n, id)) %>%
      ungroup() %>%
      data.table()
    
    n_rep <- dat2 %>%
      mutate(id = paste(restype, edu, agegr, birthgr, wealth)) %>%
      dplyr::select(restype, edu, agegr, birthgr, wealth) %>%
      distinct() %>%
      nrow()
    
    pre_dat <- dat2[, lapply(.SD, function(x) approx(x=year, y = x, rule = 2, method = "linear", 
                                                     xout = 2005:min(dat2$year))$y),
                    by = c('agegr','birthgr','restype','edu','wealth'), .SDcols = 'Prop_interp']
    pre_dat[, year := rep(2005:min(dat2$year), times= n_rep)]
    pre_dat <- pre_dat %>%
      filter(year != min(dat2$year)) %>%
      data.table()
  } else
    pre_dat <- NULL
  
    dat4 <- data.table(dat %>%
                         mutate(id = paste(restype, edu, agegr, birthgr, wealth)) %>%
                         group_by(id) %>%
                         summarize(n = n()) %>%
                         filter(n > 1) %>%
                         left_join(dat %>%
                                     mutate(id = paste(restype, edu, agegr, birthgr, wealth))
                         ))
  
  n_rep <- dat4 %>%
    mutate(id = paste(restype, edu, agegr, birthgr, wealth)) %>%
    dplyr::select(restype, edu, agegr, birthgr, wealth) %>%
    distinct() %>%
    nrow()
  
  post_dat <- dat4[, lapply(.SD, function(x) approx(x=year, y = x, rule = 2, method = "constant", xout = max(df$year):2022)$y),
                   by = c('agegr','birthgr','restype','edu','wealth'), .SDcols = 'Prop_interp']
  
  post_dat[, year := rep(max(df$year):2022, times= n_rep)]
  post_dat <- post_dat %>%
    filter(year != max(df$year)) %>%
    data.table()
  
  if(!is.null(pre_dat)){
    survey_interp <- rbind(pre_dat, dat %>%
                   dplyr::select(-c(sector,country,prop,data)), post_dat) %>%
             filter(year %in% 2005:2022)
  } else
    survey_interp <- rbind(dat %>%
                   dplyr::select(-c(sector,country,prop,data)), post_dat)%>%
             filter(year %in% 2005:2022)
  
  if(n_census > 0){
    dat <- df %>%
      filter(data == "census") %>%
      rename(Prop_interp = prop)
    if(n_census > 1){
      dat<-dat %>%
        filter(!is.na(restype)) %>%
        group_by(agegr,restype,edu,birthgr) %>%
        complete(year = seq(min(year),max(year))) %>%
        tween_fill("linear") %>%
        left_join(dat) %>%
        data.table()
    }

    overlap_years <- unique(dat$year)[unique(dat$year) %in% unique(survey_interp$year)]

    if(length(overlap_years > 0)){
    dat_to_rake <- survey_interp %>%
      filter(year %in% overlap_years) %>%
      rename(prop1 = Prop_interp) %>%
      left_join(dat %>%
                  dplyr::select(-c(wealth,data)) %>%
                  rename(pjc=Prop_interp)) %>%
      filter(!is.na(pjc)) %>%
      mutate(begin_prop = prop1,
             change = .1) %>%
      mutate(id = paste0(agegr, restype, edu, birthgr, wealth, year)) %>%
      data.table()

    iter <- 1
    tol <- .00001
    raking_pop <- dat_to_rake
    while (max(dat_to_rake$change, na.rm = T) > tol) {

      cat(paste("\n\nRaking, iteration", iter))

      raking_pop <- rake(data = raking_pop)
      raking_pop[, tot_change := abs(begin_prop - prop1)]
      cat(paste(".......max difference:", max(raking_pop$change, na.rm = T)))

      # update the iteration number; stop after 500 (we can reasonably assume that it's not going to work at that point)
      iter <- iter + 1
      if (iter > 100) {
        stop(paste0("Raking has iterated 100 times. \nCheck to ensure your file is convergence compliant. \nExecution halted."))
      }
    }
    survey_interp <- raking_pop %>%
      rename(Prop_interp = prop1) %>%
      dplyr::select(-c(sector,country,pjc,id, begin_prop, change,sum_prop3, tot_change)) %>%
      rbind(survey_interp %>%
              mutate(id = paste0(agegr, restype, edu, birthgr, wealth, year)) %>%
              filter(!(id %in% raking_pop$id)) %>%
              dplyr::select(-id))
    }
  }

  
  return(survey_interp)
}
