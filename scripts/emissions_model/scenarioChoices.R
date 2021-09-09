## Scenario choices function to act as a check on user choices

scenarioChoices <- function(vals, buf_dist, region) {
  if (exists('annual_totals') == F) {
    warning("Select value for annual emissions totals")
  }
  if (exists('space_scenario') == F | exists('cooking_scenario') == F | exists('water_scenario') == F) {
    warning("Select values for all scenarios")
  }
  if (baseline == 1) {
    cat(paste("Baseline Emissions"))
  }
  if (urban_scenario == 1) {
    cooking_scenario = 0
    water_scenario = 0
    space_scenario = 0
    cat(paste("You have chosen to model fuel movements for: ","The urban fuel shift"))
    if(vals == "yes"){
      return(list(cooking_scenario, 
                  water_scenario, 
                  space_scenario, 
                  lighting_scenario,
                  annual_totals, 
                  urban_scenario,
                  unconstrain_scenario, 
                  baseline,
                  region))
    }
    }
  else {  
    cat(paste("You have chosen to model fuel movements for:",
              if (unconstrain_scenario == 1) {"An unconstrained scenario"},
              if (cooking_scenario == 1){"Cooking scenario"},
              if (water_scenario == 1) {"water heating scenario"},
              if (space_scenario == 1) {"Space heating scenario"},
              if (lighting_scenario == 1) {"Lighting scenario"},
              "\n At a buffer distance of:",
              gsub(".*_","",buf_dist),
              "\n Emissions will be aggregated on:",
              if (annual_totals == 1) {"annual basis\n"}
              else {"monthly basis"},
              sep = "\n"))
  if(vals == "yes"){
    return(list(cooking_scenario, 
                water_scenario, 
                space_scenario,
                lighting_scenario,
                annual_totals, 
                urban_scenario, 
                unconstrain_scenario, 
                baseline,
                region))
  } 
       }
}

## choice functions which work to override previous choices, hold over from the loop format of this function
## may still be valuable in the new workflow

allChoices <- function(all) {
  if (exists("all", inherits = F)) { # wrapper to make sure variable all exists, could add to other if statements
    if (all == 1) {
      cooking_scenario <<- 1
      water_scenario <<- 1
      space_scenario <<- 1
      urban_scenario <<- 0
      baseline <<- 0
      all <<- 1
    }
  }
}


baselineScenario <- function(baseline) {
  # Baseline - no fuel changes
  if (baseline == 1) {
    # ensure nothing is beyond the constrain factor so no fuels move
    constrain_value <<- 999
    # switch all other scenarios to false
    urban_scenario <<- 0
    cooking_scenario <<- 0
    water_scenario <<- 0
    space_scenario <<- 0
    lighting_scenario <<- 0
    buf_dist <- "buf_0km"
  }
}

urbanScenario <- function(urban_scenario) {
  ## Should produce the same values as the zero buffer
  if (urban_scenario == 1) {
    # change all other scnearions to zero
    cooking_scenario <<- 0
    water_scenario <<- 0
    space_scenario <<- 0
    buf_dist <- "buf_0km"
    # census <- moveUrban(census)
  }
}

