# function to calculate emissions from cooking

#' @param df main dataset
#' @param fuelFracVar vector of fuel names
#' @param monthNum integer indicating month
#' 
#' @note ck_ene is a column with in df

energyToFuel_ck_v2 <- function(df, fuelFracVar, monthNum, heat_bonus = 0.10) {
    fuel <- gsub(".*_", "", fuelFracVar)
  
    heatingDays_index <- paste0("space_", monthNum)
  
    # dplyr syntax
    heatingDays_index <- sym(heatingDays_index)
    fuelFracVar <- sym(fuelFracVar)
    
    # get fuel values
    lhv <- fuel_atts %>% 
      filter(Fname2 == fuel) %>% 
      select(LHV) %>% 
      pull()
  
    # get efficiency
    fuel_effic <- fuel_atts %>%
      filter(Fname2 == fuel) %>% 
      select(eff_fuelLevel) %>%
      pull()
    
    newCol <- df %>% 
      transmute(!!paste("ck", fuel, "kg", monthNum, sep = "_") := 
                  ck_ene * (!! fuelFracVar) * nHouseholds * 
                  (365/12 + (1 + heat_bonus) * (!! heatingDays_index)) / fuel_effic / lhv)
  
    # return new column
    return(newCol)
}


