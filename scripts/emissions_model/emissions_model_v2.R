
# Author Notes ------------------------------------------------------------

# developed my Max Blasdel maxblasdel@gmail.com
# I apologize for programming with dplyr and the !!as.name() notation
# There have been API changes to dplyr which make this somewhat cleaner, but this syntax still works

# Purpose -----------------------------------------------------------------

# This function gets looped in `run_emissions_scenarios_V2.R` and `run_emissions_scenarios.R` to produce all model results

emissionsModel <- function(census,
                           buf_dist, 
                           region, 
                           constrain_factor, 
                           constrain_value){
  
    # only filter if region is not india
    if(region != "india") { 
      census %<>%
        filter(zone == as.name(region))  
    }

  # calculate an effectiveness value based on the constrain factor that is being used
  census <-
    census %>% 
      mutate(eff = case_when(!!as.name(constrain_factor) >= constrain_value ~ 1, 
                             TRUE ~ 0))
  
  # move fuels for urban areas
  # urban areas should have the same output as a zero buffer, so I think this is uneeded
    # if (baseline != 1) {
    #   census <- moveUrban(census, buf_dist = buf_dist, constrain_factor, constrain_value)
    # }
    
  # ########### Lighting ############
  # fuelFracVar <- str_subset(colnames(census), pattern = "mainlight")
  # 
  # # drop unneeded lighting columns
  # fuelFracVar <- str_subset(fuelFracVar, pattern = c("solar", "nolight"), negate = T)
  # fuelFracVar
  # 
  lighting_fuel_cols <- c("mainlight_electricity", "mainlight_kerosene", "mainlight_otheroil", "mainlight_other")
  # lighting_fuel_cols
  # adding in lighting scenario
  # Move lighting fuels from emissions sources to electricity
  if(lighting_scenario == 1) {
    # dplyr syntax
    buf_val <- sym(buf_dist)
    
    census <-
      census %>%
      mutate(
        mainlight_electricity = mainlight_electricity + (rowSums(select(., lighting_fuel_cols)) * eff * !! buf_val),
        mainlight_kerosene = mainlight_kerosene  - (mainlight_kerosene * eff * !! buf_val),
        mainlight_otheroil = mainlight_otheroil - (mainlight_otheroil * eff * !! buf_val),
        mainlight_other = mainlight_other - (mainlight_other * eff * !! buf_val))
  }
  
    # Drop electricity from emissions calculations
    lighting_fuel_cols <- str_subset(lighting_fuel_cols, "electricity", negate = T)
    
    # Sum all lighting
    census %<>% 
      mutate(allLighting = rowSums(select(., lighting_fuel_cols)))
    
    # Run lighting and cbind to original dataset
    census <- cbind(census, lightToFuelProp(census))
    
    ############# Space heating #################
    for (month in months) {
      # Changing function version to V3 to reflect greater fuels which are counted in the change 1/22/2020
      # Produces the total fuel in kilograms used for space heating
      # All fuel is assumed wood
      heating_index <- paste0("space_", month)
      # heating_index <- quo(heating_index)
      
      census <- cbind(census, energyToFuel_sh_v3(df = census, monthNum = month, heating_index = heating_index))
    }
    
    # establish fuels to move
    cooking_fuel_cols <- c("maincook_wood", 
                           "maincook_cropres", 
                           "maincook_dung",
                           "maincook_coalLigChar",
                           "maincook_kerosene",
                           "maincook_lpg", 
                           "maincook_biogas",
                           "maincook_other")
    
    # Note: water heating uses the same fuels as cooking
    water_fuel_cols <- cooking_fuel_cols
    
    #################### Cooking and Water Heating ######################
    if (cooking_scenario == 1 & water_scenario == 1) {
      
      ## Move fuel proportions
      census <- moveFuels(census, buf_dist)
      
      ## Water Heating 
      for (month in months){
        census <- cbind(census, lapply(water_fuel_cols, function(x) {
          energyToFuel_wh_v2(df = census, fuelFracVar = x, monthNum = month)
        }))}
      
      ## Cooking
      for (month in months){
        census <- cbind(census, lapply(cooking_fuel_cols, function(x) {
          energyToFuel_ck_v2(df = census, fuelFracVar = x, monthNum = month)
        }))}
    }
    
    # water and no cooking scenario
    if (water_scenario == 1 & cooking_scenario == 0){
    
      ## Cooking
      for (month in months){
        census <- cbind(census, lapply(cooking_fuel_cols, function(x) {
           energyToFuel_ck_v2(df = census, fuelFracVar = x, monthNum = month)
        }))}
      
      ## Move fuel proportions
      census <- moveFuels(census, buf_dist)
      
      ## Water Heating 
      for (month in months){
        census <- cbind(census, lapply(water_fuel_cols, function(x) {
          energyToFuel_wh_v2(df = census, fuelFracVar = x, monthNum = month)
        }))}
    }
    
    # cooking and no water scenario
    if (water_scenario == 0 & cooking_scenario == 1) {
      
      ## Water Heating 
      for (month in months){
        census <- cbind(census, lapply(water_fuel_cols, function(x) {
          energyToFuel_wh_v2(df = census, fuelFracVar = x, monthNum = month)
        }))}
      
      ## Move fuel proportions
      census <- moveFuels(census, buf_dist)
      
      ## Cooking
      for (month in months){
        census <- cbind(census, lapply(cooking_fuel_cols, function(x) {
          energyToFuel_ck_v2(df = census, fuelFracVar = x, monthNum = month)
        }))}
    }
    
    # move fuel movement scenario
    if (water_scenario == 0 & cooking_scenario == 0) {
      
      ## Water Heating 
      for (month in months){
        census <- cbind(census, lapply(water_fuel_cols, function(x) {
          energyToFuel_wh_v2(df = census, fuelFracVar = x, monthNum = month)
        }))}
      
      ## Cooking
      for (month in months){
        census <- cbind(census, lapply(cooking_fuel_cols, function(x) {
          energyToFuel_ck_v2(df = census, fuelFracVar = x, monthNum = month)
        }))}
    }
    
    ######### Move Spacing heating fuels ###################
    ## This is done separately from calculating the emissions associated with the activity ##
    if (space_scenario == 1) {
      
      # get wood attributes
      wood <- fuel_atts %>% 
        filter(Fname3 == "sh_wood")
      
      # get fuel attributes
      lpg <- fuel_atts %>% 
        filter(Fname3 == "ck_lpg")
      
      # get columns of interest, sh_wood
      sh_cols <- grep("sh_wood", colnames(census), value = T)

      buf_val <- sym(buf_dist)
      # calc new columns
      census %<>% 
        mutate(
          sh_lpg_kg_1 = (sh_wood_kg_1 * eff * !! buf_val * wood$LHV * wood$eff_fuelLevel) / (lpg$LHV * lpg$eff_fuelLevel),
          sh_lpg_kg_2 = (sh_wood_kg_2 * eff * !! buf_val * wood$LHV * wood$eff_fuelLevel) / (lpg$LHV * lpg$eff_fuelLevel),
          sh_lpg_kg_3 = (sh_wood_kg_3 * eff * !! buf_val * wood$LHV * wood$eff_fuelLevel) / (lpg$LHV * lpg$eff_fuelLevel),
          sh_lpg_kg_4 = (sh_wood_kg_4 * eff * !! buf_val * wood$LHV * wood$eff_fuelLevel) / (lpg$LHV * lpg$eff_fuelLevel),
          sh_lpg_kg_5 = (sh_wood_kg_5 * eff * !! buf_val * wood$LHV * wood$eff_fuelLevel) / (lpg$LHV * lpg$eff_fuelLevel),
          sh_lpg_kg_6 = (sh_wood_kg_6 * eff * !! buf_val * wood$LHV * wood$eff_fuelLevel) / (lpg$LHV * lpg$eff_fuelLevel),
          sh_lpg_kg_7 = (sh_wood_kg_7 * eff * !! buf_val * wood$LHV * wood$eff_fuelLevel) / (lpg$LHV * lpg$eff_fuelLevel),
          sh_lpg_kg_8 = (sh_wood_kg_8 * eff * !! buf_val * wood$LHV * wood$eff_fuelLevel) / (lpg$LHV * lpg$eff_fuelLevel),
          sh_lpg_kg_9 = (sh_wood_kg_9 * eff * !! buf_val * wood$LHV * wood$eff_fuelLevel) / (lpg$LHV * lpg$eff_fuelLevel),
          sh_lpg_kg_10 = (sh_wood_kg_10 * eff * !! buf_val * wood$LHV * wood$eff_fuelLevel) / (lpg$LHV * lpg$eff_fuelLevel),
          sh_lpg_kg_11 = (sh_wood_kg_11 * eff * !! buf_val * wood$LHV * wood$eff_fuelLevel) / (lpg$LHV * lpg$eff_fuelLevel),
          sh_lpg_kg_12 = (sh_wood_kg_12 * eff * !! buf_val * wood$LHV * wood$eff_fuelLevel) / (lpg$LHV * lpg$eff_fuelLevel),
          
          sh_wood_kg_1 = sh_wood_kg_1 - (sh_wood_kg_1 * eff * !! buf_val),
          sh_wood_kg_2 = sh_wood_kg_2 - (sh_wood_kg_2 * eff * !! buf_val),
          sh_wood_kg_3 = sh_wood_kg_3 - (sh_wood_kg_3 * eff * !! buf_val),
          sh_wood_kg_4 = sh_wood_kg_4 - (sh_wood_kg_4 * eff * !! buf_val),
          sh_wood_kg_5 = sh_wood_kg_5 - (sh_wood_kg_5 * eff * !! buf_val),
          sh_wood_kg_6 = sh_wood_kg_6 - (sh_wood_kg_6 * eff * !! buf_val),
          sh_wood_kg_7 = sh_wood_kg_7 - (sh_wood_kg_7 * eff * !! buf_val),
          sh_wood_kg_8 = sh_wood_kg_8 - (sh_wood_kg_8 * eff * !! buf_val),
          sh_wood_kg_9 = sh_wood_kg_9 - (sh_wood_kg_9 * eff * !! buf_val),
          sh_wood_kg_10 = sh_wood_kg_10 - (sh_wood_kg_10 * eff * !! buf_val),
          sh_wood_kg_11 = sh_wood_kg_11 - (sh_wood_kg_11 * eff * !! buf_val),
          sh_wood_kg_12 = sh_wood_kg_12 - (sh_wood_kg_12 * eff * !! buf_val))
    }
    
    # Calculate Emissions Totals ----------------------------------------------
  
    # establish fuels
    fuelFracVar <- str_subset(colnames(census), pattern = "maincook")
    
    # drop electricity and nocook
    fuelFracVar <- str_subset(fuelFracVar, pattern = c("electricity","nocook"), negate = T)
    
    # remove everything before underscore '_'
    fuel <- gsub(".*_", "", fuelFracVar)
    
    # paste appropriate prefix, will change for other energy types
    fuel <- paste("ck", fuel, sep = "_")
    fuel <- as.list(fuel)
    
    # Establish all cooking columns by fuel type
    ck_columns <- lapply(fuel, function(x) {
      str_subset(names(census), pattern = x)
    })
    
    ## Cooking totals
    if (annual_totals == 1) {
      # run loop to add new column of cooking totals
      # depends on fuel and _columns to be of same length
      for (i in 1:length(fuel)) {
        census <- census %>% 
          mutate(!!paste(fuel[[i]], "tot", sep = "_") := rowSums(select(., ck_columns[[i]]))
          )
      }
      
      # find totals columns
      ck_totals <- lapply(fuel, function(x) {
        str_subset(names(census), pattern = x) %>% 
          str_subset(pattern = "_tot")
      })  
    } # end if statement
    
    # change fuels from cooking to water heating
    fuel %<>% 
      unlist() %>% 
      gsub(pattern = "ck", replacement =  "wh", x = .) %>% 
      as.list()
    
    # find all columns associated with water heating
    wh_columns <- lapply(fuel, function(x) {
      str_subset(names(census), pattern = x)
    })
    
    ## Water heating totals
    if (annual_totals == 1) {
      for (i in 1:length(fuel)) {
        census <- census %>% 
          mutate(!!paste(fuel[[i]], "tot", sep = "_") := rowSums(select(., wh_columns[[i]])))
      }
      
      # Find totals columns
      wh_totals <- lapply(fuel, function(x) {
        str_subset(names(census), pattern = x) %>% 
          str_subset(pattern = "_tot")
      }) 
    }
    
    # space heating is either wood or lpg
    if (space_scenario == 1) {
      fuel <- c("sh_wood", "sh_lpg") %>% as.list()
    } else {
      fuel <- c("sh_wood") %>% as.list()
    }
    
    # find all columns for space heating 
    sh_columns <-  lapply(fuel, function(x) {
      str_subset(names(census), pattern = x)
    })
    
    # sum all space heating columns
    if (annual_totals == 1) {
      for (i in 1:length(fuel)) {
        census <- census %>% 
          mutate(!!paste(fuel[[i]], "tot", sep = "_") := rowSums(select(., sh_columns[[i]])))  
      }
      
      # find all space heating totals
      sh_totals <- lapply(fuel, function(x) {
        str_subset(names(census), pattern = x) %>% 
          str_subset(pattern = "tot")
      })
    } 
    
    # lighting columns
    lt_columns <- str_subset(names(census), pattern = "lt_")
    
    #unlist other columns
    ck_columns %<>% unlist()
    wh_columns %<>% unlist()
    sh_columns %<>% unlist()
    
    # establish fuel totals or individual columns
    if (annual_totals == 1) {
      cols <- ck_totals
    } else {
      cols <- ck_columns
    }
    
    # define fuel variable
    fuel <- gsub(".*_", "", fuelFracVar) %>% as.list()
    # 2/19/20 explicit about the fuel to use

    # Calculate emissions
    newCols <- lapply(fuel, function(x) {
      emissions(df = census, fuel = x, cols = cols, service = "ck")
    })
 
    census <- cbind(newCols, census)
  
    # repeat for water columns
    # column totals
    if (annual_totals == 1) {
      cols <- wh_totals
    } else {
      cols <- wh_columns
    }
    
    # uses same fuel as cooking
    newCols <- lapply(fuel, function(x) {
      emissions(df = census, fuel = x, cols = cols, service = "wh")
    })

    census <- cbind(newCols, census)
    
    # create columns for space heating scenarios
    if (space_scenario == 1) {
      fuel <- c("wood", "lpg") %>% as.list()
    } else {
      fuel <- c("wood") %>% as.list()
    }
    
    # use totals or monthly numbers
    if (annual_totals == 1) {
      cols <- sh_totals
    } else {
      cols <- sh_columns
    }

    # calculate emissions from space heating, only comes from wood for baseline
    newCols <- lapply(fuel, function(x) {
      emissions(df = census, fuel = x, cols = cols, service = "sh")
    })
    census <- cbind(newCols, census)
    
    # lighting columns
    fuel <- c("swl", "hur") %>% as.list()
    
    newCols <- lapply(fuel, function(x) {
      emissions(df = census, fuel = x, cols = lt_columns, service = "lt")
    })
    census <- cbind(newCols, census)
   
# Non-Annual Emissions (untested) -----------------------------------------
     
    if (annual_totals == 0) {
      
      pm25 <- str_subset(names(census), pattern = "pm25")
      bc <- str_subset(names(census), pattern = "bc")
      co2 <- str_subset(names(census), pattern = "co2")
      
      lt_pm <- str_subset(names(census), pattern = "lt_") %>% 
        str_subset(pattern = "pm25")
      lt_co2 <- str_subset(names(census), pattern = "lt_") %>% 
        str_subset(pattern = "co2")
      lt_bc <- str_subset(names(census), pattern = "lt_") %>% 
        str_subset(pattern = "bc")
    }
    if (annual_totals == 0) {
      sums <- census %>% 
        transmute(
          # create monthly totals plus 1/12 of the lighting totals
          pm_1_tots = rowSums(select(., str_subset(pm25, pattern = "_1$"))) + rowSums(select(., lt_pm))/12,
          pm_2_tots = rowSums(select(., str_subset(pm25, pattern = "_2$"))) + rowSums(select(., lt_pm))/12,
          pm_3_tots = rowSums(select(., str_subset(pm25, pattern = "_3$"))) + rowSums(select(., lt_pm))/12,
          pm_4_tots = rowSums(select(., str_subset(pm25, pattern = "_4$"))) + rowSums(select(., lt_pm))/12,
          pm_5_tots = rowSums(select(., str_subset(pm25, pattern = "_5$"))) + rowSums(select(., lt_pm))/12,
          pm_6_tots = rowSums(select(., str_subset(pm25, pattern = "_6$"))) + rowSums(select(., lt_pm))/12,
          pm_7_tots = rowSums(select(., str_subset(pm25, pattern = "_7$"))) + rowSums(select(., lt_pm))/12,
          pm_8_tots = rowSums(select(., str_subset(pm25, pattern = "_8$"))) + rowSums(select(., lt_pm))/12,
          pm_9_tots = rowSums(select(., str_subset(pm25, pattern = "_9$"))) + rowSums(select(., lt_pm))/12,
          pm_10_tots = rowSums(select(., str_subset(pm25, pattern = "_10$"))) + rowSums(select(., lt_pm))/12,
          pm_11_tots = rowSums(select(., str_subset(pm25, pattern = "_11$"))) + rowSums(select(., lt_pm))/12,
          pm_12_tots = rowSums(select(., str_subset(pm25, pattern = "_12$"))) + rowSums(select(., lt_pm))/12,
          
          bc_1_tot = rowSums(select(., str_subset(bc, pattern = "_1$"))) + rowSums(select(., lt_bc))/12,
          bc_2_tot = rowSums(select(., str_subset(bc, pattern = "_2$"))) + rowSums(select(., lt_bc))/12,
          bc_3_tot = rowSums(select(., str_subset(bc, pattern = "_3$"))) + rowSums(select(., lt_bc))/12,
          bc_4_tot = rowSums(select(., str_subset(bc, pattern = "_4$"))) + rowSums(select(., lt_bc))/12,
          bc_5_tot = rowSums(select(., str_subset(bc, pattern = "_5$"))) + rowSums(select(., lt_bc))/12,
          bc_6_tot = rowSums(select(., str_subset(bc, pattern = "_6$"))) + rowSums(select(., lt_bc))/12,
          bc_7_tot = rowSums(select(., str_subset(bc, pattern = "_7$"))) + rowSums(select(., lt_bc))/12,
          bc_8_tot = rowSums(select(., str_subset(bc, pattern = "_8$"))) + rowSums(select(., lt_bc))/12,
          bc_9_tot = rowSums(select(., str_subset(bc, pattern = "_9$"))) + rowSums(select(., lt_bc))/12,
          bc_10_tot = rowSums(select(., str_subset(bc, pattern = "_10$"))) + rowSums(select(., lt_bc))/12,
          bc_11_tot = rowSums(select(., str_subset(bc, pattern = "_11$"))) + rowSums(select(., lt_bc))/12,
          bc_12_tot = rowSums(select(., str_subset(bc, pattern = "_12$"))) + rowSums(select(., lt_bc))/12,
          
          co2_1_tot =  rowSums(select(., str_subset(co2, pattern = "_1$"))) + rowSums(select(., lt_co2))/12,
          co2_2_tot = rowSums(select(., str_subset(co2, pattern = "_2$"))) + rowSums(select(., lt_co2))/12,
          co2_3_tot = rowSums(select(., str_subset(co2, pattern = "_3$"))) + rowSums(select(., lt_co2))/12,
          co2_4_tot = rowSums(select(., str_subset(co2, pattern = "_4$"))) + rowSums(select(., lt_co2))/12,
          co2_5_tot = rowSums(select(., str_subset(co2, pattern = "_5$"))) + rowSums(select(., lt_co2))/12,
          co2_6_tot = rowSums(select(., str_subset(co2, pattern = "_6$"))) + rowSums(select(., lt_co2))/12,
          co2_7_tot = rowSums(select(., str_subset(co2, pattern = "_7$"))) + rowSums(select(., lt_co2))/12,
          co2_8_tot = rowSums(select(., str_subset(co2, pattern = "_8$"))) + rowSums(select(., lt_co2))/12,
          co2_9_tot = rowSums(select(., str_subset(co2, pattern = "_9$"))) + rowSums(select(., lt_co2))/12,
          co2_10_tot = rowSums(select(., str_subset(co2, pattern = "_10$"))) + rowSums(select(., lt_co2))/12,
          co2_11_tot = rowSums(select(., str_subset(co2, pattern = "_11$"))) + rowSums(select(., lt_co2))/12,
          co2_12_tot = rowSums(select(., str_subset(co2, pattern = "_12$"))) + rowSums(select(., lt_co2))/12,
          
          geometry = geometry # geometry column allows for conversion to sf object
        )
    }
    if (annual_totals == 0) {
      month_sums <- sums %>% 
        summarize_if(is.numeric, sum, na.rm = T)
      
      # Attach scenario choices into output df
      temp <- scenarioChoices(vals = "yes", buf_dist, region) 
      temp <- do.call(cbind, temp) %>% 
        as.data.frame()
      colnames(temp) <- c("cook", "water", "space", "lighting", "annual", "urban", "unconstrained", "baseline", "region")
      month_sums <- cbind(month_sums, temp)
      
      # add zone to annual sum data
      # month_sums <- cbind(month_sums, filter_zone)
      
      # append to totals list
      totals <- data.frame(month_sums, row.names = buf_dist)
      totals %<>% 
        mutate(constrain_factor = constrain_factor,
               constrain_value = constrain_value,
               dist = buf_dist)
      return(totals)
    }
    

# Sum emissions data ------------------------------------------------------

    if (annual_totals == 1) {
      census %<>% 
        mutate(pm_tot = rowSums(select(., str_subset(names(census), pattern = "pm25"))), 
               bc_tot = rowSums(select(., str_subset(names(census), pattern = "bc"))),
               co2_tot = rowSums(select(., str_subset(names(census), pattern = "co2"))))
    }
    
    #### Finalize data for return -----------------------------------------------------
    
    # Subset and export poly with selected columns
    if (mapping_option == 1) {
      
      # Calc Delivered and Required Fuels ---------------------------------------
      
      # These values are only calculated when the full object is returned
      
      # Delivered and required energy is calculated for each month for cooking, water heating, and space heating
      #### Delivered Energy
      
      # get fuels associated with cooking
      fuelFracVar <- str_subset(colnames(census), pattern = "maincook")
      
      # drop electricity and nocook
      fuelFracVar <- str_subset(fuelFracVar, pattern = c("electricity","nocook"), negate = T)
      
      
      # for each month, run each fuelFracVar for cooking
      for (month in months){
        census <- cbind(census, lapply(fuelFracVar, function(x) {
          delEnergyByFuel_ck(df = census, fuelFracVar = x, monthNum = month)
        }))}
      
      # for each month, run each fuelFracVar for water heating, same fuels as cooking
      for (month in months){
        census <- cbind(census, lapply(fuelFracVar, function(x) {
          delEnergyByFuel_wh(df = census, fuelFracVar = x, monthNum = month)
        }))}
      
      # Run each month for space heating
      for (month in months) {
        census <- cbind(census, delEnergyByFuel_sh(df = census, monthNum = month))
      }
      
      #### Required Energy
      
      # for each month, run each fuelFracVar for cooking
      for (month in months){
        census <- cbind(census, lapply(fuelFracVar, function(x) {
          reqEnergyByFuel_ck(df = census, fuelFracVar = x, monthNum = month)
        }))}
      
      # for each month, run each fuelFracVar for water heating, same fuels as cooking
      for (month in months){
        census <- cbind(census, lapply(fuelFracVar, function(x) {
          reqEnergyByFuel_wh(df = census, fuelFracVar = x, monthNum = month)
        }))}
      
      # Run each month for space heating
      for (month in months) {
        census <- cbind(census, reqEnergyByFuel_sh(df = census, monthNum = month))
      }
      # only write out shp file for desired buffer distance
      # TODO tweak this, awkward
      # Select emissions columns, population, area, region, state 
      # if(buf_dist == desired_dist) {
        map <- census %>% 
          # select(map_cols) %>% # I dont think `map_cols` is defined
          st_as_sf()
        return(map)
      # }
    }
    
    if (annual_totals == 1) {
      annual_sums <- census %>% 
        summarise_at(c("pm_tot", "co2_tot", "bc_tot"), sum, na.rm=T)
      
      # Attach scenario choices into output df
      temp <- scenarioChoices(vals = "yes", buf_dist, region) 
      temp <- do.call(cbind, temp) %>% 
        as.data.frame()
      
      # create column headers
      colnames(temp) <- c("cook", "water", "space", "lighting", "annual", "urban", "unconstrained", "baseline", "region")
    
      # bind emissions data with scenario headers
      annual_sums <- cbind(annual_sums, temp)
      
      # add zone to annual sum data
      #  annual_sums <- cbind(annual_sums, filter_zone)
      
      # append to totals list
      totals <- data.frame(annual_sums, row.names = buf_dist)
      
      # set identifying values
      totals %<>% 
        mutate(constrain_factor = constrain_factor,
               constrain_value = constrain_value,
               dist = buf_dist)

      return(totals)
    }
    
    # Check if baseline requirements are met -or-
    # If fuel scenarios are not being modeled. Covers urban scenario.
    if (cooking_scenario == 0 & water_scenario == 0 & space_scenario == 0) {
      return(totals)
    }
    if (baseline == 1) {
      return(totals)
    }
 # } # end of for loop
  return(totals)
} # end of function

