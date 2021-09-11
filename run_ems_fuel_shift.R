
# Developed in collaboration with Nick Lam and in support of High Resolution India Modeling Project
#
# Scenario Modeling of India Emissions
#
# Inputs: Polygon file of India with census information merged and spatial buffers around urban areas
#
# Outputs: Sum of CO2, PM, and Black Carbon for each energy technology shift
#
# Purpose:  Run various scenarios changing fuel types for cooking, water heating, and space heating
#           Emissions from lighting are also calculated, but there are no fuel shifts associated with lighting

# Fuel Shift --------------------------------------------------------------

source("scripts/emissions_model/load_all.R")


##################### baseline model #####################
# change urban wealth indicies to 5 as they were NA values
# dat <- dat %>%
#   mutate(quintiles = case_when(TRU == "Urban" ~ 5, # 5 is the highest value for the wealth index
#                                is.na(quintiles) ~ 0, # set NA values to zero
#                                T ~ quintiles))


scenarioChoices(vals = T,
                buf_dist = "Don't worry")

# run loop function over each region
# Outputs a file with all buffer distances for each region
for (region in region_list) {
 
    # time tracking
    start <- Sys.time()
  
    output <- lapply(buf_list, function(x) {
   
    # set constrain value
    constrain_value = 0.05
    
    # Unconstrained scenario
    if (unconstrain_scenario == 1) {
      constrain_value <- -1
      }
    
    emissionsModel(census = dat, 
                     buf_dist = x,
                     region = region, 
                     constrain_factor = "maincook_lpg",
                     constrain_value = constrain_value)
    } 
  ) # end lapply function
  
  output <- bind_rows(output) # combine into a df
  
  key <- c(cooking_scenario,
           water_scenario,
           space_scenario,
           lighting_scenario,
           unconstrain_scenario) %>% 
    paste(collapse = "_")

  # output data for graphing
  fwrite(output, paste0("output/tabular/emissions/fuel_shift/", region_list[i], "_", key ,".csv"))

  print(Sys.time() - start)
} # end region for loop function
