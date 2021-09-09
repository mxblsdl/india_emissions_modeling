

delEnergyByFuel_sh <- function(df, monthNum) {
  
  # get lower heating value for wood (MJ/kg)
  lhv <- fuel_atts %>%
    filter(Fname3 == "sh_wood") %>%
    pull(LHV)
  
  sheatingDays_index <- paste0("space_", monthNum) 
  
  newCol <- df %>% 
    # correct columns for space heating
    #select(TRU, nHouseholds, sheatingDays_index, maincook_coalLigChar, maincook_lpg) %>% 
    ## Why is the function below not with wood!!
    mutate(sum_col = maincook_wood + maincook_cropres + maincook_dung + maincook_coalLigChar) %>%   
    transmute(
      delivEne_mj_day = case_when(
        TRU == "Rural" ~ nHouseholds * sum_col * 0.875 * 4 * 0.75 * !!as.name(sheatingDays_index) * lhv,
        TRU == "Urban" ~ nHouseholds * sum_col * 0.875 * 4 * 0.75 * !!as.name(sheatingDays_index)) * lhv,
      delivEne_mj_night = case_when(
        TRU == "Rural" ~ nHouseholds * 0.15 * 7 * 0.75 * !!as.name(sheatingDays_index) * lhv,
        TRU == "Urban" ~ nHouseholds * 0.05 * 9 * 0.75 * !!as.name(sheatingDays_index) * lhv
      ),
      !!paste("sh_wood", "mj_del", monthNum, sep="_") := delivEne_mj_night + delivEne_mj_day
    ) %>% 
    select(!!paste("sh_wood", "mj_del", monthNum, sep="_"))
  
  # st_geometry(newCol) <- NULL
  return(newCol)
}


reqEnergyByFuel_sh <- function(df, monthNum) {
  
  # get lower heating value for wood (MJ/kg)
  lhv <- fuel_atts %>%
    filter(Fname3 == "sh_wood") %>%
    pull(LHV)
  
  # lookup fuel efficiency from table (should this be Fname3 = "sh_wood" , need to check if efficiencies are same)
  fuel_effic <- fuel_atts %>% 
    filter(Fname3 == "sh_wood") %>% 
    pull(eff_fuelLevel)
  
  sheatingDays_index <- paste0("space_", monthNum) 
  
  newCol <- df %>% 
    # correct columns for space heating
    #select(TRU, nHouseholds, sheatingDays_index, maincook_coalLigChar, maincook_lpg) %>% 
    ## Why is the function below not with wood!!
    mutate(sum_col = maincook_wood + maincook_cropres + maincook_dung + maincook_coalLigChar) %>%   
    transmute(
      delivEne_mj_day = case_when(
        TRU == "Rural" ~ nHouseholds * sum_col * 0.875 * 4 * 0.75 * !!as.name(sheatingDays_index) * lhv / fuel_effic,
        TRU == "Urban" ~ nHouseholds * sum_col * 0.875 * 4 * 0.75 * !!as.name(sheatingDays_index)) * lhv / fuel_effic,
      delivEne_mj_night = case_when(
        TRU == "Rural" ~ nHouseholds * 0.15 * 7 * 0.75 * !!as.name(sheatingDays_index) * lhv / fuel_effic,
        TRU == "Urban" ~ nHouseholds * 0.05 * 9 * 0.75 * !!as.name(sheatingDays_index) * lhv / fuel_effic
      ),
      !!paste("sh_wood", "mj_req", monthNum, sep="_") := delivEne_mj_night + delivEne_mj_day
    ) %>% 
    select(!!paste("sh_wood", "mj_req", monthNum, sep="_"))
  
  # st_geometry(newCol) <- NULL
  return(newCol)
}

  
  
  

# all space heating assumed to be with wood
# sum maincook columns beforehand, this will remove need to use select

# Notes: 
  # hours of space heating per day
  #   hours = {'Rural': 4, 'Urban': 4, 'Rural_night': 7, 'Urban_night': 9}
  # Fraction of househoulds that use space heating:
  #   frH = {'Rural': .875, 'Urban': .875, 'Rural_night': .15, 'Urban_night': .05}
  # fuel burn rate(kg/hr) = 0.75  
  # All space heating assumed to be with wood

energyToFuel_sh_v2 <- function(df, monthNum) {
  
sheatingDays_index <- paste0("space_", monthNum) 

newCol <- df %>% 
  # correct columns for space heating
    #select(TRU, nHouseholds, sheatingDays_index, maincook_coalLigChar, maincook_lpg) %>% 
  mutate(sum_col = maincook_coalLigChar + maincook_lpg) %>%   
  transmute(
      mass_day = case_when(
        TRU == "Rural" ~ nHouseholds * sum_col * 0.875 * 4 * 0.75 * !!as.name(sheatingDays_index),
        TRU == "Urban" ~ nHouseholds * sum_col * 0.875 * 4 * 0.75 * !!as.name(sheatingDays_index)),
      mass_night = case_when(
        TRU == "Rural" ~ nHouseholds * 0.15 * 7 * 0.75 * !!as.name(sheatingDays_index),
        TRU == "Urban" ~ nHouseholds * 0.05 * 9 * 0.75 * !!as.name(sheatingDays_index)
      ),
      !!paste("sh_wood", "kg", monthNum, sep="_") := mass_night + mass_day
    ) %>% 
    select(!!paste("sh_wood", "kg", monthNum, sep="_"))
  
  # st_geometry(newCol) <- NULL
  return(newCol)
}

## think through rewriting this with case_when
# census <- dat
# monthNum <- 1
# sheatingDays_index <- paste0("space_", monthNum)
# 
# census %>% 
#   select(TRU, nHouseholds, sheatingDays_index, maincook_coalLigChar, maincook_lpg) %>% 
#   transmute(!!paste("sh_wood", "kg", monthNum, sep = "_") := case_when( 
#     TRU == "Rural" ~ # adding together the day and night columns
#       nHouseholds * rowSums(.[ , 4:5, drop=T], na.rm = T) * 0.875 * 4 * 0.75 * !!as.name(sheatingDays_index) + 
#       nHouseholds * 0.15 * 7 * 0.75 * !!as.name(sheatingDays_index),
#     TRU == "Urban" ~ nHouseholds * rowSums(.[ , 4:5, drop=T], na.rm = T) * 0.875 * 4 * 0.75 * !!as.name(sheatingDays_index) +
#       nHouseholds * 0.05 * 9 * 0.75 * !!as.name(sheatingDays_index)
#       ))
# 
# # in the interest of more readable code I'm going to make this 
# census %>% 
#   select(TRU, nHouseholds, sheatingDays_index, maincook_coalLigChar, maincook_lpg) %>% 
#   transmute(
#     mass_day = case_when(
#         TRU == "Rural" ~ nHouseholds * rowSums(.[ , 4:5, drop=T], na.rm = T) * 0.875 * 4 * 0.75 * !!as.name(sheatingDays_index),
#         TRU == "Urban" ~ nHouseholds * rowSums(.[ , 4:5, drop=T], na.rm = T) * 0.875 * 4 * 0.75 * !!as.name(sheatingDays_index)),
#     mass_night = case_when(
#        TRU == "Rural" ~ nHouseholds * 0.15 * 7 * 0.75 * !!as.name(sheatingDays_index),
#        TRU == "Urban" ~ nHouseholds * 0.05 * 9 * 0.75 * !!as.name(sheatingDays_index)
#     ),
#     !!paste("sh_wood", "kg", monthNum, sep="_") := mass_night + mass_day
#   )


