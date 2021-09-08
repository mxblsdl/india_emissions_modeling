## Script to visualize emissions results
## adapted from visualizeTotals.Rmd found in Task 4 folder

# load libraries
library(tidyverse) # general data wrangling and filtering
library(magrittr) # use of %<>% operator
library(reshape2) # reformatting data
library(wesanderson) # color palette base

# capitalization function
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# set options
options(scipen = 20)

# load data 
files <- dir("output/tabular/emissions/constraint_change", full.names = T, pattern = ".csv")

# remove quintiles in favor of wquint_pca
files <- grep("quintiles", files, invert = T, value = T)

# filter out base case
files <- grep(pattern = "base", files, invert = T, value = T)

# load base data and constraint specific datasets
base <- dir("output/tabular/emissions/", full.names = T, pattern = "baseV3") # offers fix to how this was calculated
base <- read_csv(base, col_types = cols())

## HACK in the lighting variable so can join
#base$lighting <- 0

### Possible different set-up
# to group different constraints by region, as opposed to by region
data <- lapply(files, read_csv, col_types = cols())

## Attach base values to each region specific dataset
data <- lapply(data, function(x) {

  # filter base by region
  f <- base %>% 
    filter(region == unique(x$region))
  
  # bind to emissions dataset
  x <- rbind(f, x)
  
  # calculate percentages; Note: pm will ultimitely be reported but the other species were calculated as well
  x %<>% 
    mutate(per_base_co2 = co2_tot / first(co2_tot),
           per_base_pm = pm_tot / first(pm_tot),
           per_base_bc = bc_tot / first(bc_tot))
  
  return(x)
})

## reorder distance as factors for legend
data <- lapply(data, function(x) {
  x %>% 
    mutate(dist = factor(dist, levels = unique(dist)))
})

# rebind to dataframe
data <- bind_rows(data)

# need to normalize wealth indices and per household persons to plot on same scale
data <- data %>% 
  group_by(region, constrain_factor) %>% 
  mutate(constrain_value = case_when(constrain_factor == "wquint_pca" ~ constrain_value / max(constrain_value),
                                     constrain_factor == "per_house_weighted" ~ constrain_value / max(constrain_value),
         T ~ constrain_value)) %>% 
  ungroup()

########################### Plotting ######################################
###########################################################################
# Filter out the base case 
data <- data %>% 
  filter(constrain_factor != "null")

# Prepare region names for graphing
data %<>%
  mutate(region = gsub("_", " ", region))

data$region <-
  sapply(data$region, simpleCap)

## I want to get three plots next to each other for each constriant factor, right now the list is by region,
region_list <- unique(data[,"region"]) %>% 
  unlist()

# Rearrange data to fit with ribbon type
data_high_low <- data %>%
  select(dist, per_base_pm, constrain_factor, constrain_value, region)

# Spread values so each column can be called rather than filtering the data in the plot call
data_high_low %<>% 
  spread(key = dist, value = per_base_pm)

# confirm values of maximum reduction
unconstrained <-
  data_high_low %>%
    filter(constrain_value == 0) %>%
    arrange(region) 

# Each region should have the same numbers regardless of the constraint factor

# get colors only 5 colors, need 7
zissou <-
  wes_palette(name = "Zissou1", n = 5)

# Change data to show baseline emissions averted
data_high_low %<>%
  mutate_at(.vars = grep("buf", colnames(.)), .funs = ~1-.)

## Create lists of graphs for each region
gg <- lapply(region_list, function(g) {
  data_high_low %>% 
    filter(region == g) %>% 
    mutate(constrain_factor = case_when(constrain_factor == "maincook_lpg" ~ "Maincook LPG",
                                        constrain_factor == "wquint_pca" ~ "Wealth Index",
                                        constrain_factor == "per_house_weighted" ~ "People per Household")) %>% 
    ggplot() +
    facet_grid(~ constrain_factor) +
    theme_minimal() +
    theme(text = element_text(size = 22)) + # may need to change
    geom_ribbon(aes(x = constrain_value, ymin = buf_20km, ymax = 0),
                fill = '#E06508',
                alpha = 1) +
    geom_ribbon(aes(x = constrain_value, ymin = buf_20km, ymax = buf_0km),
                fill = '#D8A109',
                alpha = 1) +
    geom_ribbon(aes(x = constrain_value, ymin = buf_20km, ymax = buf_1km),
                fill = zissou[4],
                alpha = 1) +  
    geom_ribbon(aes(x = constrain_value, ymin = buf_20km, ymax = buf_3km),
                fill = zissou[3],
                alpha = 1) +  
    geom_ribbon(aes(x = constrain_value, ymin = buf_20km, ymax = buf_5km),
                fill = "#8FB37F",
                alpha = 1) +
    geom_ribbon(aes(x = constrain_value, ymin = buf_20km, ymax = buf_10km),
                fill = zissou[2],
                alpha = 1) +
    geom_ribbon(aes(x = constrain_value, ymin = buf_20km, ymax = buf_15km),
                fill = zissou[1],
                alpha = 1) +
    theme(axis.text.x = element_text(size = 16),
          legend.position = "bottom") +
    scale_y_continuous(limits = c(0, 1)) + # maybe remove numbers for large graph
    labs(y = "Baseline Emissions Averted",
         title = g,
         # subtitle = constrain_factor,
         x = "Constraint Cutoff",
         color = "Distance From Urban Area")
})

# inspect
gg[[3]]

## save output
for (i in 1:length(region_list)) {
  ggsave(paste0("output/charts/", 
                region_list[i],
                ".pdf"), 
         gg[[i]], 
         width = 8.5,
         height = 5,
         dpi = 500)
}

