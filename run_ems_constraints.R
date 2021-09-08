
# Scenario Modeling of India Emissions -----------------------------------------

# Max Blasdel
# max.blasdel@gmail.com

#! Constraint Change Script ----------------------------------------------------

## This script is set up for running all fuel shifts with varying constraint factors. 
## A separate file (run_emissions_scenarios.R) 
## is set up to run the constributions of each energy service

# TODO what does constraint change mean
# Run various scenarios changing fuel types for
# cooking, water heating, and space heating
# Emissions from lighting are also calculated,
# but there are no fuel shifts associated with lighting

# Developed in collaboration with Nick Lam
# in support of High Resolution India Modeling Project

# Inputs and Outputs ------------------------------------------------------

# Inputs:
# - Polygon file of India with census information
#   merged and spatial buffers around urban areas
# - Fuel Attributes defining the emissions of certain fuel types
# link to spatial data in script
source("data.R")

# Additional constants and model parameter options
# Regions and spatial buffers can be configured here
source("constants.R")

# Outputs: Sum of CO2, PM, and Black Carbon for each 
# - buffer area,
# - scenario choice,
# - chosen regions

# Load Libraries ----------------------------------------------------------

library(dplyr) # data manipulation
library(stringr) # string subsetting
library(magrittr) # essentially only for use of '%<>%' operator
library(sf) # for manipulating spatial objects
library(readr) # write_csv function

# Enabling parallelization
# This is not requiered and a sequential processing method is supplied below
library(future.apply)


# Load all relavent functions ---------------------------

## loop script function
source("scripts/emissions_model_v2.R")

## Calculate the mass of fuels based on required energy for different uses
source("scripts/energy_to_fuel/energyToFuel_ck_v2.R")
source("scripts/energy_to_fuel/energyToFuel_wh_v2.R")
source("scripts/energy_to_fuel/energyToFuel_lt_v2.R")

# updated to V3; uses different fuels for calculation
source("scripts/energy_to_fuel/energyToFuel_sh_v3.R")

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

# baseline emissions value calculations
# assuems no fuel switching
source("baseline.R")

# Scenarios ------------------------------------------------------------------

# Set scenario variables that dictate model behavior
source("scenario_variaitons.R")

# Data fixes ------------------------------------------------------------------

# These have not been incorporated intot eh spatial data inputs
# change urban wealth indicies to 5 as they were NA values

# dat <- dat %>%
#   mutate(quintiles = case_whe(
#  TRU == "Urban" ~ 5, # 5 is the highest value for the wealth index
#  is.na(quintiles) ~ 0, # set NA values to zero
#  T ~ quintiles))


# fill in missing values of weighted house people
dat <- dat %>%
  mutate(per_house_weighted = ifelse(
    is.na(per_house_weighted),
    mean(dat$per_house_weighted, na.rm = T), # set NA values to zero
    per_house_weighted))

# Baseline --------------------------------

baseline_scenario(dat, baseline, "output/baseline.csv")

# RUN Function ---------------------------------
# non baseline
allChoices(all = 1)

# Future Implmentation (exp and alt) --------------------------------------

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

for (i in seq_len(length(region_list))) {
  
  # identify region to subset on
 region <- region_list[i]
  
  out <- future.apply::future_lapply(scens, function(scen) {
    emissionsModel(census = dat,
                  buf_dist = scen$buf_dist,
                  region = region,
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
for (i in seq_len(length(region_list))) {
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
