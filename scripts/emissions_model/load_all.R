
# Load Libraries ----------------------------------------------------------

library(dplyr) # data manipulation
library(stringr) # string subsetting
library(magrittr) # essentially only for use of '%<>%' operator
library(sf) # for manipulating spatial objects
library(readr) # write_csv function

# Enabling parallelization
# This is not requiered and a sequential processing method is supplied below
library(future.apply)

# Inputs and Outputs ------------------------------------------------------

# Inputs:
# - Polygon file of India with census information
#   merged and spatial buffers around urban areas
# - Fuel Attributes defining the emissions of certain fuel types
# link to spatial data in script
source("scripts/emissions_model/data.R")

# Additional constants and model parameter options
# Regions and spatial buffers can be configured here
source("scripts/emissions_model/constants.R")

# Outputs: Sum of CO2, PM, and Black Carbon for each 
# - buffer area,
# - scenario choice,
# - chosen regions


# Load all relavent functions ---------------------------

## loop script function
source("scripts/emissions_model/emissions_model_v2.R")

## Calculate the mass of fuels based on required energy for different uses
source("scripts/emissions_model/energy_to_fuel/energyToFuel_ck_v2.R")
source("scripts/emissions_model/energy_to_fuel/energyToFuel_wh_v2.R")
source("scripts/emissions_model/energy_to_fuel/energyToFuel_lt_v2.R")

# updated to V3; uses different fuels for calculation
source("scripts/emissions_model/energy_to_fuel/energyToFuel_sh_v3.R")

## Helper function to inform user of scenario choices
source("scripts/emissions_model/scenarioChoices.R")

## Calculate emissions based on fuel type
## Returns CO2, PM, and BC
source("scripts/emissions_model/emissions.R")

## Move fuel functions for scenario modeling
source("scripts/emissions_model/moveFuels.R")

## Energy by fuel type
source("scripts/emissions_model/EnergyByFuel/energyByFuel_ck_v1.R")
source("scripts/emissions_model/EnergyByFuel/energyByFuel_sh_v1.R")
source("scripts/emissions_model/EnergyByFuel/energyByFuel_wh_v1.R")

# baseline emissions value calculations
# assuems no fuel switching
source("scripts/emissions_model/baseline.R")

# Scenarios ------------------------------------------------------------------

# Set scenario variables that dictate model behavior
source("scripts/emissions_model/scenario_variations.R")