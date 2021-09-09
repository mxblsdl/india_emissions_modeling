

# Function to calculate delivered *energy* (not required!) by fuel type
delEnergyByFuel_ck <- function(df, fuelFracVar, monthNum, heat_bonus = 0.10) {
  fuel <- gsub(".*_", "", fuelFracVar)
  
  heatingDays_index <- paste0("space_", monthNum)
  # get fuel values
  # lhv <- fuel_atts %>% 
  #   filter(Fname2 == fuel) %>% 
  #   select(LHV) %>% 
  #   unlist()
  
  fuel_effic <- fuel_atts %>%
    filter(Fname2 == fuel) %>% 
    select(eff_fuelLevel) %>%
    unlist()
  
  newCol <- df %>% 
    transmute(!!paste("ck", fuel, "mj_del", monthNum, sep = "_") := 
                ck_ene * (!!as.name(fuelFracVar)) * nHouseholds * 
                (365/12 + (1+heat_bonus) * (!!as.name(heatingDays_index))) / fuel_effic)
  # add line to remove geometry from new column
  # will be bound to df with geometry in main code
  # st_geometry(newCol) <- NULL
  return(newCol)
}


reqEnergyByFuel_ck <- function(df, fuelFracVar, monthNum, heat_bonus = 0.10) {
  fuel <- gsub(".*_", "", fuelFracVar)
  
  heatingDays_index <- paste0("space_", monthNum)
  
  newCol <- df %>% 
    transmute(!!paste("ck", fuel, "mj_req", monthNum, sep = "_") := 
                ck_ene * (!!as.name(fuelFracVar)) * nHouseholds * 
                (365/12 + (1+heat_bonus) * (!!as.name(heatingDays_index))))
  # add line to remove geometry from new column
  # will be bound to df with geometry in main code
  # st_geometry(newCol) <- NULL
  return(newCol)
}





# function to calculate emissions from cooking (delivered fuel)

# energyToFuel_ck_v2 <- function(df, fuelFracVar, monthNum, heat_bonus = 0.10) {
#     fuel <- gsub(".*_", "", fuelFracVar)
#   
#     heatingDays_index <- paste0("space_", monthNum)
#   # get fuel values
#     lhv <- fuel_atts %>% 
#       filter(Fname2 == fuel) %>% 
#       select(LHV) %>% 
#       unlist()
#   
#     fuel_effic <- fuel_atts %>%
#       filter(Fname2 == fuel) %>% 
#       select(eff_fuelLevel) %>%
#       unlist()
#     
#     newCol <- df %>% 
#       transmute(!!paste("ck", fuel, "kg", monthNum, sep = "_") := 
#                   ck_ene * (!!as.name(fuelFracVar)) * nHouseholds * 
#                   (365/12 + (1+heat_bonus) * (!!as.name(heatingDays_index))) / fuel_effic / lhv)
#     # add line to remove geometry from new column
#     # will be bound to df with geometry in main code
#     # st_geometry(newCol) <- NULL
#     return(newCol)
# }




