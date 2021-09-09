
# Run baselines for each region and then call the appropriate baseline file
## Run once, maybe move to other script

baseline_scenario <- function(dat, baseline, output) {
  if (baseline == 1) {
    baselineScenario(T)

    # set baseline scenario
    base <- lapply(region_list, function(x) {
      emissionsModel(census = dat,
                    buf_dist = "buf_0km",
                    region = x,
                    constrain_factor = 'wquint_pca',
                    constrain_value = constrain_value)
    })
    # merge together
    base <- bind_rows(base)
    # change constrain factor to null as none was used
    base$constrain_factor <- "null"
    # write to disk
    write.csv(base, file = output, row.names = F) 
  }
}
