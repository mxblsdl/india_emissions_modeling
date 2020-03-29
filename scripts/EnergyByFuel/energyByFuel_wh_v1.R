# Function to calculate delivered energy for cooking

## From original model: 6.16 = increase MJ/hh-day if water heating which includes 4.87 from water and 1.29 on average for cooking; only the WH component is considered below

delEnergyByFuel_wh <- function(df, fuelFracVar, monthNum, reqEne = 4.87) {
  # list fuel name
  fuel <- gsub(".*_", "", fuelFracVar)
  
  #find water column number
  wHeatingDays_index <- paste0("water_", monthNum)
  
  # # lookuplhv value from table
  # lhv <- fuel_atts %>% 
  #   filter(Fname2 == fuel) %>% 
  #   select(LHV) %>% 
  #   unlist()
  
  # lookup fuel efficiency from table
  fuel_effic <- fuel_atts %>% 
    filter(Fname2 == fuel) %>% 
    select(eff_fuelLevel) %>% 
    unlist() 
  
  # note the !!as.name is required for dplyr to evaluate dynamic column headings
  newCol <- df %>% 
    transmute(!!paste("wh", fuel, "mj_del", monthNum, sep = "_") := reqEne *
                ((!!as.name(fuelFracVar)) * nHouseholds) *
                (!!as.name(wHeatingDays_index)) / fuel_effic)
  #  st_geometry(newCol) <- NULL
  return(newCol)
}


reqEnergyByFuel_wh <- function(df, fuelFracVar, monthNum, reqEne = 4.87) {
  # list fuel name
  fuel <- gsub(".*_", "", fuelFracVar)
  
  #find water column number
  wHeatingDays_index <- paste0("water_", monthNum)
  
  # lookup fuel efficiency from table
  fuel_effic <- fuel_atts %>% 
    filter(Fname2 == fuel) %>% 
    select(eff_fuelLevel) %>% 
    unlist() 
  
  # note the !!as.name is required for dplyr to evaluate dynamic column headings
  newCol <- df %>% 
    transmute(!!paste("wh", fuel, "mj_req", monthNum, sep = "_") := reqEne *
                ((!!as.name(fuelFracVar)) * nHouseholds) *
                (!!as.name(wHeatingDays_index)))
  #  st_geometry(newCol) <- NULL
  return(newCol)
}








# energyToFuel_wh_v2 <- function(df, fuelFracVar, monthNum, reqEne = 4.87) {
#   # list fuel name
#   fuel <- gsub(".*_", "", fuelFracVar)
#   
#   #find water column number
#   wHeatingDays_index <- paste0("water_", monthNum)
#   
#   # lookuplhv value from table
#   lhv <- fuel_atts %>% 
#     filter(Fname2 == fuel) %>% 
#     select(LHV) %>% 
#     unlist()
#   
#   # lookup fuel efficiency from table
#   fuel_effic <- fuel_atts %>% 
#     filter(Fname2 == fuel) %>% 
#     select(eff_fuelLevel) %>% 
#     unlist() 
#   
#   # note the !!as.name is required for dplyr to evaluate dynamic column headings
#   newCol <- df %>% 
#     transmute(!!paste("wh", fuel, "kg", monthNum, sep = "_") := reqEne *
#                 ((!!as.name(fuelFracVar)) * nHouseholds) *
#                 (!!as.name(wHeatingDays_index)) / fuel_effic / lhv)
# #  st_geometry(newCol) <- NULL
#   return(newCol)
# }
