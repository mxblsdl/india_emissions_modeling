
# Model Inputs ----------------------------------------------

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
    , #"buf_4km"
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
