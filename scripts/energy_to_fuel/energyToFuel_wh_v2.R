
energyToFuel_wh_v2 <- function(df, fuelFracVar, monthNum, reqEne = 4.87) {
  # list fuel name
  fuel <- gsub(".*_", "", fuelFracVar)
  
  #find water column number
  wHeatingDays_index <- paste0("water_", monthNum)
 
  # dplyr programming syntax
  fuelFracVar <- sym(fuelFracVar)
  wHeatingDays_index <- sym(wHeatingDays_index)
  
  # lookuplhv value from table
  lhv <- fuel_atts %>% 
    filter(Fname2 == fuel) %>% 
    select(LHV) %>% 
    pull()

  # lookup fuel efficiency from table
  fuel_effic <- fuel_atts %>% 
    filter(Fname2 == fuel) %>% 
    select(eff_fuelLevel) %>% 
    pull() 
  
  # note the !!as.name is required for dplyr to evaluate dynamic column headings
  newCol <- df %>% 
    transmute(!!paste("wh", fuel, "kg", monthNum, sep = "_") := reqEne *
                (!!fuelFracVar * nHouseholds) *
                (!! wHeatingDays_index / fuel_effic / lhv))
  


  
  return(newCol)
}
