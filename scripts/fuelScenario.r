# function for moving fuel proportions in sf objects
# to be used in modeling different scenarios.

# param eff = effectiveness; either a 1 or 0 value
# param prop_in_buffer = proportion in the spatial buffer; fraction between 0 and 1 

fuelScenario <- function (dat, to, from, eff, prop_in_buffer) {
  census %>% 
    mutate(!!paste(to) := !!as.name(to) + (!!as.name(from) * eff * prop_in_buffer),
           !!paste(from) := !!as.name(from) - (!!as.name(from) * eff * prop_in_buffer))  
}