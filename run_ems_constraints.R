
# Scenario Modeling of India Emissions -----------------------------------------------------

# Author: Max Blasdel ---------------------
# email: max.blasdel@humboldt.edu --------------------



#! Constraint Change Script ------------------------------------------------


# Developed in collaboration with Nick Lam and in support of High Resolution India Modeling Project


# Inputs and Outputs ------------------------------------------------------


# Inputs: Polygon file of India with census information merged and spatial buffers around urban areas

# Outputs: Sum of CO2, PM, and Black Carbon for each buffer and scenario choices for chosen region

# Purpose -----------------------------------------------------------------
# Run various scenarios changing fuel types for cooking, water heating, and space heating
# Emissions from lighting are also calculated, but there are no fuel shifts associated with lighting

# Load Libraries ----------------------------------------------------------

library(dplyr) # data manipulation
library(stringr) # string subsetting
library(magrittr) # essentially only for use of '%<>%' operator
library(sf) # for manipulating spatial objects
library(readr) # write_csv function

# Load all relavent functions ---------------------------

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

# Options ----------------------------------

# set penalty for scientific notation
options(scipen = 10)

# LOAD Data ----------------------------------------------

# Emissions Data

fuel_atts <- read.csv("sup_info/fuelAtts_v5.csv")
ef_names <- gsub(".*_", "", colnames(fuel_atts[8:10]))

# Main Data File 

dat <- readRDS("output/spatial/india_with_indicies.rds") %>% as.data.frame()

# Constants ---------------------------------------------

# This most likely will not change but there is flexibility here
months <- 1:12

# For water heating
reqEne <-  4.87

# Scenario variations ----------------------------------

## NOTE: This script is set up for running all fuel shifts with varying constraint factors. 
## A separate file (run_emissions_scenarios.R) is set up to run the constributions of each energy service

# annual or monthly totals
# annual_totals <- 0 # monthly WARNING: HAS NOT BEEN EXTENSIVELY TESTED
annual_totals <- 1 # Annual totals

## unconstrained scenario
unconstrain_scenario <- 0 # Applies the constraint factor when moving fuels
#unconstrain_scenario <- 1 # Does not use a constraint factor

## Cooking scenario
# cooking_scenario <- 0 # off 
cooking_scenario <- 1 # on

## Water heating scenario
# water_scenario <- 0 # off
water_scenario <- 1 # on

## Space Heating Variable
# space_scenario <- 0 # off
space_scenario <- 1 # on

## Lighting Scenario
# lighting_scenario <- 0
lighting_scenario <- 1


# Override Scenarios ----------------------------------------
# The following variables override previous scenario choices

## urban scenario: only change fuel for urban areas. Is not run with other scenarios
## Urban should be the same as the 0km buffer
urban_scenario <- 0 # off # deprecated but still needed for model runs
# urban_scenario <- 1 # if 1 overrides other scenario choices

## baseline function
## No fuel movement occurs. This is considered the estimated emissions for all of India
baseline <- 0 # off
# baseline <- 1 # if 1 overrides other scenario choices

# Run all of the fuel shift scenarios
# Runs cooking, spaceheating, and water scenarios
# all <- 0
all <- 1 # if 1 overrides previous scenario choices


# Add Model Vectors -------------------------------------------------------

# These are the model options to run through the selected fuel scenarios

#TODO add state filtering
# Zonal Councils
region_list <- c("north",
                 "north_west",
                 "gangetic_plains", 
                 "north_east",
                 "central",
                 "west",
                 "south",
                 "india")

# Buffer distances

buf_list <- c("buf_0km", # add this as a check on the outputs, moving Urban only
              "buf_1km", 
              #"buf_2km", 
              "buf_3km", # adding a few more buffer distances to aid in the gradient effect
              #"buf_4km", 
              "buf_5km", 
              "buf_10km", 
              "buf_15km", 
              "buf_20km")


# Mapping Option -----------------------------------------------

## IGNORE THESE PARAMETERS FOR NOW< CHOSE 0 FOR MAPPING OPTION
## THIS IS ADDRESSED IN V2

# mapping_option <- 1
mapping_option <- 0 # NEEDS VALUE TO RUN MODEL


# Provides some fixes for running constraint values
# change urban wealth indicies to 5 as they were NA values

# dat <- dat %>%
#   mutate(quintiles = case_when(TRU == "Urban" ~ 5, # 5 is the highest value for the wealth index
#                                is.na(quintiles) ~ 0, # set NA values to zero
#                                T ~ quintiles))


# fill in missing values of weighted house people
dat <- dat %>%
  mutate(per_house_weighted = case_when(is.na(per_house_weighted) ~ mean(dat$per_house_weighted, na.rm = T), # set NA values to zero
                               T ~ per_house_weighted))

# Baseline --------------------------------

# I want to run baseline for each region and then call the appropriate baseline file
## Run once, maybe move to other script

if (baseline == 1) {
  baselineScenario(T)
  # set baseline scenario
  base <- lapply(region_list, function(x) {
    emissionsModel(census = dat,
                   buf_dist = "buf_0km",
                   region = x,
                   constrain_factor = 'wquint_pca', # these values will not be used but I need a value here to run the function
                   constrain_value = constrain_value) # I think setting this to zero messes things up
  })
  
  # merge together
  base <- bind_rows(base)
  
  # change constrain factor to null as none was used
  base$constrain_factor <- "null"
  
  # write to disk
  write.csv(base, file = "output/tabular/emissions/baseV3.csv", row.names = F) 
}

# Constrain Factors --------------------------------------------

## lpg cook values
constrain_factor <- "maincook_lpg"
constrain_list <- seq(0, 1, 0.05)

## wealth indices
constrain_factor <- "wquint_pca"
constrain_list <- seq(0, 5.5, .5)

## people per household
constrain_factor <- "per_house_weighted"
constrain_list <- seq(0, 9, .25)

# RUN Function ---------------------------------
# non baseline
allChoices(all = 1)


# Future Implmentation (exp and alt) --------------------------------------
library(future.apply)

# the better way to organize this would be to create a list of variables and then apply each option to the emission
# function. This type of organization is more conducive to parallel processing
scens <- expand.grid(buf_list, constrain_factor, constrain_list, stringsAsFactors = F)

# set names
names(scens) <- c("buf_dist", "con_fac", "con_val")

# add id and split
scens$ID <- seq(1:nrow(scens))

scens <- split(scens, scens$ID)

# set future params
plan(multiprocess, workers = 2)
options(future.globals.maxSize = 1200000000)

for (i in 1:length(region_list)) {
  
  # identify region to subset on
 region <- region_list[i]
  
  out <- future.apply::future_lapply(scens, function(scen) {
    emissionsModel(census = dat,
                   buf_dist = scen$buf_dist,
                   region = region, # to remove, i think filter before would be more efficient
                   constrain_factor = scen$con_fac,
                   constrain_value = scen$con_val) 
  }) # end future apply
  
  out <-
    bind_rows(out)

  # output data for graphing
  # need unlist or something to get proper name
  write_csv(out, paste0("output/tabular/emissions/constraint_change/", region, "-", scens[[1]]$con_fac, ".csv"))
}

# Sequential Method -------------------------------------------------------

# Still loop on the region to filter the results
start <- Sys.time()
for (i in 1:length(region_list)) {
  # nested apply to run two lists of variables
  output <- lapply(buf_list, function(x) {
    lapply(constrain_list, function(y) {
      emissionsModel(census = dat, 
                     buf_dist = x,
                     region = region_list[i], 
                     constrain_factor = constrain_factor,
                     constrain_value = y)
    })
  })
  
  output <- unlist(output, recursive = F) # flatten the list
  
  output <- bind_rows(output) # combine into a df
  
  # output data for graphing
  write_csv(output, paste0("output/tabular/emissions/constraint_change/", region_list[i], "-", constrain_factor, ".csv"))
  rm(output)
  gc()
}

Sys.time() - start
