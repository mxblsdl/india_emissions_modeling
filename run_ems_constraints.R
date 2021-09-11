
# Scenario Modeling of India Emissions -----------------------------------------

# Constraint Change Script ----------------------------------------------------

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

source("scripts/emissions_model/load_all.R")

# Data fixes ------------------------------------------------------------------

# These have not been incorporated in to teh spatial data inputs
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

# Run Function ---------------------------------

# Sets all of the scenarios to TRUE
# This overrides all scenario settings from scenario_variations.R
allChoices(all = 1)

# Parallel Implmentation (exp and alt) --------------------------------------

# Create data frame of model variables
scens <- expand.grid(buf_list, 
constrain_factor,
 constrain_list,
  stringsAsFactors = F)

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
  write_csv(out, paste0("output/", region, "-", scens[[1]]$con_fac, ".csv"))
}

# Sequential Method -------------------------------------------------------

## Uncomment to run sequential method

# Still loop on the region to filter the results
# start <- Sys.time()
# for (i in seq_len(length(region_list))) {
#   # nested apply to run two lists of variables
#   output <- lapply(buf_list, function(x) {
#     lapply(constrain_list, function(y) {
#       emissionsModel(census = dat, 
#                     buf_dist = x,
#                     region = region_list[i], 
#                     constrain_factor = constrain_factor,
#                     constrain_value = y)
#     })
#   })
#   
#   output <- unlist(output, recursive = F) # flatten the list
#   
#   output <- bind_rows(output) # combine into a df
#   
#   # output data for graphing
#   write_csv(output, paste0("output/", region_list[i], "-", constrain_factor, ".csv"))
#   rm(output)
#   gc()
# }
# 
# Sys.time() - start
