

# Baseline --------------------------------

source("scripts/emiision_model/load_all.R")

# Creates a baseline output where there is no fuel switching
# Needs region_list and constrain_value
baseline_scenario(dat = dat,
                  baseline = baseline,
                  region_list = region_list,
                  output = "output/baseline.csv")