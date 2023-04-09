
#Util functions

get_metric_from_string <- function(s){
  #'Extract metric from selected metric in UI:
  #'
  #'@return The correct metric name usable in later functions.
  
  for(metric_name in c("CSMA", "SMI", "SMRA", "SMG")){
  
    if(grepl(metric_name, s, fixed=T)){
      return(metric_name)
    }}
}

get_unit_for_measurement <- function(metric = c("CSMA", "SMI", "SMRA", "SMG")){
  #' Get correct unit for a given metric.
  #' 
  #' @param metric character The metric.
  #' 
  #' @return The corresponding unit.
  
  units <- list(
    CSMA='cm2',
    SMI = 'cm2/m2',
    SMRA = 'HU',
    SMG = 'HU*cm2/m2'
  )
  
  if(metric %in% names(units)){
    return(units[[metric]])
  }else{
    return('')
  }
  
  
}

