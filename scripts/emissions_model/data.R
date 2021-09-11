

# Load Data ----------------------------------------------

# Emissions Data

# Fuel attributes define the emissions and heating value for 
# various fuels. These are a direct control on the model.
# Fuel attributes come directly from Tami and only have minor formatting changes
fuel_atts <- read.csv("input/fuelAtts_v5.csv")
ef_names <- c("pm25", "bc", "co2")

# Download the main spatial input file from the below link and move to input/
# Currently hosted on a google drive account
# This spatial file is the product of earlier analysis joining census data and
# the cleaned India polygon layer
# browseURL('https://drive.google.com/file/d/1pTgPTUB2In23_PhS69N1qhHpdVotIMuY/view?usp=sharing')

# Main Data File
# Replace name of downloaded file as needed
tryCatch({
    dat <- as.data.frame(readRDS("input/spatial_input.rds"))
}, warning = function(cond) {
    warning("\n\nDownload spatial input file from linked Google Drive")
    message(cond)
})
