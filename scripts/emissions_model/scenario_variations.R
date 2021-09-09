
# Scenario variations ----------------------------------

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
