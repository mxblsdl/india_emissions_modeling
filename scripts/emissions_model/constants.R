
# Model Inputs ----------------------------------------------

# Constants ---------------------------------------------

# This most likely will not change but there is flexibility here
months <- 1:12

# For water heating
reqEne <-  4.87

# Add Model Vectors -------------------------------------------------------

# These are the model options to run through the selected fuel scenarios

#TODO add state filtering
# Zonal Councils
region_list <- c(
    "north"
    , "north_west"
    , "gangetic_plains"
    , "north_east"
    , "central"
    , "west"
    , "south"
    , "india"
    )

# Buffer distances

buf_list <- c(
    "buf_0km" # add this as a check on the outputs, moving Urban only
    , "buf_1km" 
    #, "buf_2km"
    , "buf_3km" # adding a few more buffer distances to aid in the gradient effect
    #, "buf_4km"
    , "buf_5km"
    , "buf_10km"
    , "buf_15km"
    , "buf_20km"
    )


# Mapping Option -----------------------------------------------

## IGNORE THESE PARAMETERS FOR NOW< CHOSE 0 FOR MAPPING OPTION
## THIS IS ADDRESSED IN V2

# mapping_option <- 1
mapping_option <- 0 # NEEDS VALUE TO RUN MODEL

# Options ----------------------------------

# set penalty for scientific notation
options(scipen = 10)
