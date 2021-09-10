# Max Blasdel
# max.blasdel@humboldt.edu

# Developed in collaboration with Nick Lam and in support of High Resolution India Modeling Project
#
# Scenario Modeling of India Emissions
#
# Inputs: Polygon file of India with census information merged and spatial buffers around urban areas
#
# Outputs: Sum of CO2, PM, and Black Carbon for each energy technology shift
#
##################################################################################################
#
# Purpose:  Run various scenarios changing fuel types for cooking, water heating, and space heating
#           Emissions from lighting are also calculated, but there are no fuel shifts associated with lighting

# Fuel Shift --------------------------------------------------------------


# Load Libraries ----------------------------------------------------------

library(dplyr) # data manipulation
library(stringr) # string subsetting
library(magrittr) # essentially only for use of '%<>%' operator
library(sf) # for manipulating spatial objects
library(data.table) # write out baseline emissions

# Relevent Functions ------------------------------------------------------


## loop script function 
source("scripts/emissions_model_v2.R")

## Calculate the mass of fuels based on required energy for different uses
source("scripts/energyToFuel_ck_v2.R")
source("scripts/energyToFuel_wh_v2.R")
source("scripts/energyToFuel_sh_v3.R") # updated to V3; uses different fuels for calculation
source("scripts/energyToFuel_lt_v2.R")

## Helper function to inform user of scenario choices
source("scripts/scenarioChoices.R")

## Calculate emissions based on fuel type
## Returns CO2, PM, and BC
source("scripts/emissions.R")

## Move fuel functions for scenario modeling
source("scripts/moveFuels.R")

## Energy by fuel type
source("scripts/EnergyByFuel/energyByFuel_ck_v1.R")
source("scripts/EnergyByFuel/energyByFuel_sh_v1.R")
source("scripts/EnergyByFuel/energyByFuel_wh_v1.R")


################# Options #####################
# set penalty for scientific notation
options(scipen = 10)

########### Load Emissions Data #################
fuel_atts <- read.csv("sup_info/fuelAtts_v5.csv")
ef_names <- gsub(".*_", "", colnames(fuel_atts[8:10]))

######################## Load census data ##################################
dat <- readRDS("output/spatial/india_with_indicies.rds") %>% 
  as.data.frame()

# load zones for filtering data
# zones <- read.csv("sup_info/alt_zones.csv", stringsAsFactors = F)

##################### constants #######################
# This most likely will not change but there is flexibility here
months <- 1:12

# For water heating
reqEne <-  4.87

#############################################################################
# Choose which zonal council to filter on 
#   - or -
# Choose a state to filter on 
#TODO add this
#############################################################################
# For looping through
region_list <- c("north",
                 "north_west",
                 "gangetic_plains", 
                 "north_east",
                 "central",
                 "west",
                 "south",
                 "india")

################ Set loop variables ###################################
## Buf distances to loop through
buf_list <- c("buf_0km", # add this as a check on the outputs, moving Urban only
              "buf_1km", 
              #"buf_2km", 
              "buf_3km", # adding a few more buffer distances to aid in the gradient effect
              #"buf_4km", 
              "buf_5km", 
              "buf_10km", 
              "buf_15km", 
              "buf_20km")

##################### baseline model #####################
# change urban wealth indicies to 5 as they were NA values
# dat <- dat %>%
#   mutate(quintiles = case_when(TRU == "Urban" ~ 5, # 5 is the highest value for the wealth index
#                                is.na(quintiles) ~ 0, # set NA values to zero
#                                T ~ quintiles))

################### Scenario variations ##################
# for outputting an sf object
mapping_option <- 1
mapping_option <- 0

# deprecated but var is still needed
urban_scenario <- 0

# annual_totals <- 0 # monthly WARNING: HAS NOT BEEN EXTENSIVELY TESTED
annual_totals <- 1 # Annual totals

## unconstrained scenario; this value does not get used in the model; need to change constrain value beforehand
unconstrain_scenario <- 0 # Applies the constraint factor when moving fuels
unconstrain_scenario <- 1 # Does not use a constraint factor

## Cooking scenario
cooking_scenario <- 0 # off 
cooking_scenario <- 1 # on

## Water heating scenario
water_scenario <- 0 # off
water_scenario <- 1 # on

## Space Heating Variable
space_scenario <- 0 # off
space_scenario <- 1 # on

## Lighting Scenario
lighting_scenario <- 0
lighting_scenario <- 1

## Urban should be the same as the 0km buffer
# urban_scenario <- 0 # off
# urban_scenario <- 1 # if 1 overrides other scenario choices

## No fuel movement occurs. This is considered the estimated emissions for all of India
baseline <- 0 # off
baseline <- 1 # if 1 overrides other scenario choices

scenarioChoices(vals = T,
                buf_dist = "Don't worry")

# run loop function
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
  )
  
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
}


#setGlobals <- function() {
# incomplete but could be used to second loop the above
  scens <- 
    data.frame('unconstrain_sc' = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
               'cooking_scenar' = c(0, 1, 0, 0, 0, 1, 1, 0, 0, 0),
               'water_scenario' = c(0, 0, 1, 0, 0, 1, 0, 1, 0, 0),
               'space_scenario' = c(0, 0, 0, 1, 0, 1, 0, 0, 1, 0),
               'lighting_scena' = c(0, 0, 0, 0, 1, 1, 0, 0, 0, 1))
  
  unconstrain_scenario <<- scens[i, 1]
  cooking_scenario <<- scens[i, 2]
  water_scenario <<- scens[i, 3]
  spcace_scenario <<- scens[i, 4]
  lighting_scenario <<- scens[i, 5]
#}

# Mapping output -----------------------------------------------------------

mapping_option = 1

region <- region_list[8] # select india
dist <- buf_list[1] # select any values
constrain_value = 1.5 # manually set to greater than one, forces baseline scenario

# create sf object to represent baseline values
#map <-
source("scripts/emissions_model_v2.R")

  emissionsModel(census = dat, 
               buf_dist = dist,
               region = region, 
               constrain_factor = "maincook_lpg",
               constrain_value = constrain_value)

# subset to columns of interest
map %>%
  names()

# first 60 columns have emissions totals by category
map <-
  map %>%
  select(grep("bc", colnames(.)),
         grep("pm25", colnames(.)),
         grep("co2", colnames(.)),
         # co2_tot, # totals
         # pm_tot,
         # bc_tot,
         # grep("req", colnames(.)), # required and delivered energies
         # grep("del", colnames(.)),
         STATE, # identifying values
         area, 
         NAME_1,
         TRU,
         nPeople,
         nHouseholds,
         zone,
         literacy_rate, # indicator variables
         per_house_weighted,
         wquint_pca, # wealth indices
         wquint_pca_no_lpg,
         buf_1km, # buffer distances
         buf_2km,
         buf_3km, 
         buf_4km, 
         buf_5km,
         buf_10km,
         buf_15km,
         buf_20km)

# addressing issue #1 in draft report
# map %>%
#   select(STATE, area, NAME_1, TRU, nPeople, nHouseholds, zone, contains("pm25_ck"))

# round decimal places
# space saving
map %<>%
  mutate_if(is.numeric, .funs = ~round(., digits = 2))

# alternative base solution to rounding numeric only columns
# nums <- vapply(map, is.numeric, FUN.VALUE = logical(1))
# 
# map[,nums] <- round(map[, nums], digits = 3)


# Write out spatial object
# writing as geopackage since takes long time to write otherwise
st_write(map, "output/spatial/all_em/all_emissions_sf.gpkg", delete_layer = T)

# create dt with no geometry
d_map <- map %>%
  st_set_geometry(NULL) %>%
  as.data.table()

# check each column
sapply(d_map, class, simplify = F)

# write out for analysis
fwrite(d_map, "output/spatial/all_baseline.csv")
# warnings associated with large numbers being written out, consider rounding
