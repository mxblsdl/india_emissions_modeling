#######################################################################

# functions to shift fuel amounts based on modeling scenarios

#######################################################################

# Establish all fuels to change. Same fuels for cooking and water heating
moveFuels <- function(census, buf_dist) {
  
  # dplyr syntax
  buf_dist <- sym(buf_dist)
  
  fuelFracVar <- c("maincook_wood", 
                   "maincook_cropres", 
                   "maincook_dung",
                   "maincook_coalLigChar",
                   "maincook_kerosene",
                   "maincook_biogas",
                   "maincook_other") 
  
  # implements moving of values
  return(census %<>% 
           mutate( maincook_lpg = maincook_lpg + (rowSums(select(., fuelFracVar)) * eff * !! buf_dist),
                   maincook_wood = maincook_wood  - (maincook_wood * eff * !! buf_dist),
                   maincook_cropres = maincook_cropres - (maincook_cropres * eff * !! buf_dist),
                   maincook_dung = maincook_dung - (maincook_dung * eff * !! buf_dist),
                   maincook_kerosene = maincook_kerosene - (maincook_kerosene * eff * !! buf_dist),
                   maincook_biogas = maincook_biogas - (maincook_biogas * eff * !! buf_dist),
                   maincook_coalLigChar = maincook_coalLigChar - (maincook_coalLigChar * eff * !! buf_dist),
                   maincook_other = maincook_other - (maincook_other * eff *!! buf_dist))
  )
}

# Very similar function to above, but only affects urban areas that meet the constrain factor
moveUrban <- function(census, buf_dist, constrain_factor, constrain_value) {
  fuelFracVar <- c("maincook_wood",
                   "maincook_cropres",
                   "maincook_dung",
                   "maincook_coalLigChar",
                   "maincook_kerosene",
                   "maincook_biogas",
                   "maincook_other")
  
  census %<>%
           mutate(eff = case_when(!!as.name(constrain_factor) >= constrain_value ~ 1,
                                  TRU != "Urban" ~ 0, # sets eff to 0 for anything that is not "Urban"
                                  T ~ 0),
                  maincook_lpg = maincook_lpg + (rowSums(select(., fuelFracVar)) * eff * `buf_0km`),
                  maincook_wood = maincook_wood  - (maincook_wood * eff * `buf_0km`),
                  maincook_cropres = maincook_cropres - (maincook_cropres * eff * `buf_0km`),
                  maincook_dung = maincook_dung - (maincook_dung * eff * `buf_0km`),
                  maincook_kerosene = maincook_kerosene - (maincook_kerosene * eff * `buf_0km`),
                  maincook_biogas = maincook_biogas - (maincook_biogas * eff * `buf_0km`),
                  maincook_coalLigChar = maincook_coalLigChar - (maincook_coalLigChar * eff * `buf_0km`),
                  maincook_other = maincook_other - (maincook_other * eff * `buf_0km`)
  )
  return(census)
}
## Testing
# moveUrban(dat, buf_dist = 'buf_0km', constrain_factor = "maincook_lpg", constrain_value = 0.05)

# Function to move fuel mass from wood to lpg for space heating
# Only affects urban areas because of the buf_0km factor
urbanSpace <- function(census) {
  wood <- fuel_atts %>% 
    filter(Fname3 == "ck_wood")
  lpg <- fuel_atts %>% 
    filter(Fname3 == "ck_lpg")
  sh_cols <- census %>% 
    select(., contains("sh_wood")) %>% 
    colnames()
  
  return(census %<>% 
           mutate(sh_lpg_kg_1 = ((!!as.name(sh_cols[1]) * eff * buf_0km) * wood$LHV * wood$eff_fuelLevel) /
                    (lpg$LHV * lpg$eff_fuelLevel),
                  sh_lpg_kg_2 = ((!!as.name(sh_cols[2]) * eff * buf_0km) * wood$LHV * wood$eff_fuelLevel) /
                    (lpg$LHV * lpg$eff_fuelLevel),
                  sh_lpg_kg_3 = ((!!as.name(sh_cols[3]) * eff * buf_0km) * wood$LHV * wood$eff_fuelLevel) /
                    (lpg$LHV * lpg$eff_fuelLevel),
                  sh_lpg_kg_4 = ((!!as.name(sh_cols[4]) * eff * buf_0km) * wood$LHV * wood$eff_fuelLevel) /
                    (lpg$LHV * lpg$eff_fuelLevel),
                  sh_lpg_kg_5 = ((!!as.name(sh_cols[5]) * eff * buf_0km) * wood$LHV * wood$eff_fuelLevel) / 
                    (lpg$LHV * lpg$eff_fuelLevel),
                  sh_lpg_kg_6 = ((!!as.name(sh_cols[6]) * eff * buf_0km) * wood$LHV * wood$eff_fuelLevel) / 
                    (lpg$LHV * lpg$eff_fuelLevel),
                  sh_lpg_kg_7 = ((!!as.name(sh_cols[7]) * eff * buf_0km) * wood$LHV * wood$eff_fuelLevel) / 
                    (lpg$LHV * lpg$eff_fuelLevel),
                  sh_lpg_kg_8 = ((!!as.name(sh_cols[8]) * eff * buf_0km) * wood$LHV * wood$eff_fuelLevel) /
                    (lpg$LHV * lpg$eff_fuelLevel),
                  sh_lpg_kg_9 = ((!!as.name(sh_cols[9]) * eff * buf_0km) * wood$LHV * wood$eff_fuelLevel) / 
                    (lpg$LHV * lpg$eff_fuelLevel),
                  sh_lpg_kg_10 = ((!!as.name(sh_cols[10]) * eff * buf_0km) * wood$LHV * wood$eff_fuelLevel) /
                    (lpg$LHV * lpg$eff_fuelLevel),
                  sh_lpg_kg_11 = ((!!as.name(sh_cols[11]) * eff * buf_0km) * wood$LHV * wood$eff_fuelLevel) /
                    (lpg$LHV * lpg$eff_fuelLevel),
                  sh_lpg_kg_12 = ((!!as.name(sh_cols[12]) * eff * buf_0km) * wood$LHV * wood$eff_fuelLevel) /
                    (lpg$LHV * lpg$eff_fuelLevel),
                  
                  sh_wood_kg_1 = sh_wood_kg_1 - (!!as.name(sh_cols[1]) * eff * buf_0km),
                  sh_wood_kg_2 = sh_wood_kg_2 - (!!as.name(sh_cols[2]) * eff * buf_0km),
                  sh_wood_kg_3 = sh_wood_kg_3 - (!!as.name(sh_cols[3]) * eff * buf_0km),
                  sh_wood_kg_4 = sh_wood_kg_4 - (!!as.name(sh_cols[4]) * eff * buf_0km),
                  sh_wood_kg_5 = sh_wood_kg_5 - (!!as.name(sh_cols[5]) * eff * buf_0km),
                  sh_wood_kg_6 = sh_wood_kg_6 - (!!as.name(sh_cols[6]) * eff * buf_0km),
                  sh_wood_kg_7 = sh_wood_kg_7 - (!!as.name(sh_cols[7]) * eff * buf_0km),
                  sh_wood_kg_8 = sh_wood_kg_8 - (!!as.name(sh_cols[8]) * eff * buf_0km),
                  sh_wood_kg_9 = sh_wood_kg_9 - (!!as.name(sh_cols[9]) * eff * buf_0km),
                  sh_wood_kg_10 = sh_wood_kg_10 - (!!as.name(sh_cols[10]) * eff * buf_0km),
                  sh_wood_kg_11 = sh_wood_kg_11 - (!!as.name(sh_cols[11]) * eff * buf_0km),
                  sh_wood_kg_12 = sh_wood_kg_12 - (!!as.name(sh_cols[12]) * eff * buf_0km))
  )
}
