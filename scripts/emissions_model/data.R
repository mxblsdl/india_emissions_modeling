

# Load Data ----------------------------------------------

# Emissions Data

# TODO explain fuel attributes
fuel_atts <- read.csv("input/fuelAtts_v5.csv")
ef_names <- gsub(".*_", "", colnames(fuel_atts[8:10]))

# Download the main spatial input file from the below link and move to input/
# Currently hosted on a google drive account
# This spatial file is the product of earlier analysis joining census data and
# the cleaned India polygon layer
# browseURL('https://drive.google.com/file/d/1pTgPTUB2In23_PhS69N1qhHpdVotIMuY/view?usp=sharing')

# Main Data File
# Replace name of downloaded file as needed
tryCatch({
    dat <- readRDS("input/spatial_input.rds")
}, warning = function(cond) {
    warning("\n\nDownload spatial input file from linked Google Drive")
    message(cond)
})
