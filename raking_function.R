rake <- function(data) {
  
  # 1. sum survey prop over wealth groups
  data[, sum_prop1 := sum(prop1), by = c('country,year,restype,agegr,edu,birthgr')]
  
  # 2. compare group size in survey and census
  data[, rjsc := pjc / sum_prop1 ]
  
  data[is.na(rjsc), rjsc := 0]
  
  data[, prop2 := prop1*rjsc]
  
  data[, sum_prop2 := sum(prop2), by = c('country,year,wealth')]
  
  data[, sum_prop3 := sum(begin_prop), by = c('country,year,wealth')]
  
  data[, rws := sum_prop3 / sum_prop2]
  
  data[is.na(rws), rws := 0]
  
  data[, prop3 := prop2*rws]
  
  data[, change := abs(prop3 - prop1)]
  
  data[, prop1 := prop3]
  
  data[, c("rjsc", "rws", "prop2", "prop3", "sum_prop1", "sum_prop2") := NULL]
  
  return(data)
}
rake_restype <- function(data) {
  
  # 1. sum survey prop over restype groups
  data[, sum_prop1 := sum(prop1), by = c('country,year,agegr,edu,birthgr')]
  
  # 2. compare group size in survey and census
  data[, rjsc := pjc / sum_prop1 ]
  
  data[is.na(rjsc), rjsc := 0]
  
  data[, prop2 := prop1*rjsc]
  
  data[, sum_prop2 := sum(prop2), by = c('country,year,restype')]
  
  data[, sum_prop3 := sum(begin_prop), by = c('country,year,restype')]
  
  data[, rws := sum_prop3 / sum_prop2]
  
  data[is.na(rws), rws := 0]
  
  data[, prop3 := prop2*rws]
  
  data[, change := abs(prop3 - prop1)]
  
  data[, prop1 := prop3]
  
  data[, c("rjsc", "rws", "prop2", "prop3", "sum_prop1", "sum_prop2") := NULL]
  
  return(data)
}
