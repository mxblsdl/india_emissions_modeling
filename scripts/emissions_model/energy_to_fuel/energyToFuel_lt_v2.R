# lighting function

lightToFuelProp <- function(df, kero_prop= c(.45, .55) ) {
  # this will be calculated on a yearly basis, different from other functions
  # out <- df %>% 
  #   as.data.frame() %>% 
  #   transmute("lt_kero_swl_kg" = allLighting * kero_prop[1] * nHouseholds * 0.78 * 9,
  #             "lt_kero_hur_kg" = allLighting * kero_prop[2] * nHouseholds * 0.78 * 9)
  # check on nHouseholds vs. nPeople
  # 12/18/2019 
  # Changing nHouseholds to nPeople based on conversations with Nick
  out <- df %>% 
    as.data.frame() %>% 
    transmute("lt_kero_swl_kg" = allLighting * kero_prop[1] * nPeople * 0.78 * 9,
              "lt_kero_hur_kg" = allLighting * kero_prop[2] * nPeople * 0.78 * 9)
  
  return(out)
}