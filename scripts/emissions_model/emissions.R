# Emissions function
# Apply the emissions factors to every fuel for the ck, wh, sp, and lt

  emissions <- function(df, fuel, cols, service) {
  # find all columns associated wtih specific fuel
  # this part of function will be looped when function is called
  cols <- str_subset(cols, pattern = fuel) %>% as.list() 
  
  if(service == "ck" | service == "wh") {
    if(fuel == "kerosene") {fuel <- "kero"} # change name to match atts
    if(fuel == "coalLigChar") {fuel <- "coal"}
    fuel <-
      paste0("ck_", fuel)
  }
 
  if(service == "sh") {
    fuel <- paste0("sh_", fuel)
  }
  
  if(service == "lt") {
    fuel <-
      paste0("lt_", fuel, "_kero")
  }
  
  # get emissions factors for specific fuel
  ef_factor <- fuel_atts %>% 
    filter(Fname3 == fuel) %>% 
    select(c(ef_pm25, ef_bc, ef_co2))
  
  # loop this part so multiplies by each column
  out  <- lapply(cols, function(x) {
    
    # each month corresponds to a col, make into list
    ene <- df %>% 
      as.data.frame() %>% 
      select(x) 
    
    # create new columns of emissions amounts
    ene %>% 
      transmute(!!paste(ef_names[1], x, sep = "_") := ef_factor[,1] * ene[,],
                !!paste(ef_names[2], x, sep = "_") := ef_factor[,2] * ene[,],
                !!paste(ef_names[3], x, sep = "_") := ef_factor[,3] * ene[,])
  })
  
  # bind together  
  out <- do.call(cbind, out)
  
  return(out)
  }
  