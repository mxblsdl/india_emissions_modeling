
## script to explore the population numbers in each region of india by buffer distance

# load libraries ----------------------------------------
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(data.table)

# capitalization function
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# set theme
mxblsdl <- theme_minimal() +
  theme(text = element_text(size = 18))

# load data -------------------------------
india <- readRDS("output/spatial/india_with_indicies.rds")

baseline <- fread("output/spatial/all_baseline.csv")

# Analysis ----------------------------------------------------------------


# calculate % of population within each buffer for each zone
data <-
  india %>% 
  group_by(zone) %>% # grouped by zonal coucil
  summarise(people_in_0km = sum(buf_0km * nPeople, na.rm = T) / sum(nPeople, na.rm = T),
            people_in_1km = sum(buf_1km * nPeople, na.rm = T) / sum(nPeople, na.rm = T),
            people_in_3km = sum(buf_3km * nPeople, na.rm = T) / sum(nPeople, na.rm = T),
            people_in_5km = sum(buf_5km * nPeople, na.rm = T) / sum(nPeople, na.rm = T),
            people_in_10km = sum(buf_10km * nPeople, na.rm = T) / sum(nPeople, na.rm = T),
            people_in_15km = sum(buf_15km * nPeople, na.rm = T) / sum(nPeople, na.rm = T),
            people_in_20km = sum(buf_20km * nPeople, na.rm = T) / sum(nPeople, na.rm = T)) %>%
  pivot_longer(cols = c(people_in_0km, 
                        people_in_1km, 
                        people_in_3km, 
                        people_in_5km, 
                        people_in_10km,
                        people_in_15km,
                        people_in_20km))

# add india population as a whole
india_pop <-
  india %>%
  summarise(people_in_0km = sum(buf_0km * nPeople, na.rm = T) / sum(nPeople, na.rm = T),
             people_in_1km = sum(buf_1km * nPeople, na.rm = T) / sum(nPeople, na.rm = T),
             people_in_3km = sum(buf_3km * nPeople, na.rm = T) / sum(nPeople, na.rm = T),
             people_in_5km = sum(buf_5km * nPeople, na.rm = T) / sum(nPeople, na.rm = T),
             people_in_10km = sum(buf_10km * nPeople, na.rm = T) / sum(nPeople, na.rm = T),
             people_in_15km = sum(buf_15km * nPeople, na.rm = T) / sum(nPeople, na.rm = T),
             people_in_20km = sum(buf_20km * nPeople, na.rm = T) / sum(nPeople, na.rm = T)) %>%
              pivot_longer(cols = c(people_in_0km, 
                                    people_in_1km, 
                                    people_in_3km, 
                                    people_in_5km, 
                                    people_in_10km,
                                    people_in_15km,
                                    people_in_20km))


# add identifying column
india_pop <-
  india_pop %>%
  mutate(zone = "india")

# bind together
data <-
  rbind(india_pop, data)

# remove non-numeric characters
data$name <-
  gsub("[^0-9.]", "", data$name)

# convert to numeric
data$name <-
  as.numeric(data$name)

# Format region names
data$zone <-
  gsub("_", " ", data$zone)

# change names of regions
data$zone <-
  sapply(data$zone, simpleCap)

# fix quintiles for urban areas
india <-
  india %>%
  mutate(quintiles = case_when(TRU == "Urban" ~ 5, # 5 is the highest value for the wealth index
                               is.na(quintiles) ~ 0, # set NA values to zero
                               T ~ quintiles))


# Wealth Indice Buffer ----------------------------------------------------


# calculate percentage of people inside each buffer who qualify as poor
# poor is defined as having a wealth indice of 1 or 2
poor_buf <-
  india %>%
  mutate(poor = ifelse(quintiles > 2, 0, 1)) %>%  # create poor indicator
  summarise(
          poor_1km = sum(buf_1km * nPeople * poor, na.rm = T) / sum(nPeople * buf_1km, na.rm = T),
          poor_2km = sum(buf_2km * nPeople * poor, na.rm = T) / sum(nPeople * buf_2km, na.rm = T),
          poor_3km = sum(buf_3km * nPeople * poor, na.rm = T) / sum(nPeople * buf_3km, na.rm = T),
          poor_4km = sum(buf_4km * nPeople * poor, na.rm = T) / sum(nPeople * buf_4km, na.rm = T),
          poor_5km = sum(buf_5km * nPeople * poor, na.rm = T) / sum(nPeople * buf_5km, na.rm = T),
          poor_10km = sum(buf_10km * nPeople * poor, na.rm = T) / sum(nPeople * buf_10km, na.rm = T),
          poor_15km = sum(buf_15km * nPeople * poor, na.rm = T) / sum(nPeople * buf_15km, na.rm = T),
          poor_20km = sum(buf_20km * nPeople * poor, na.rm = T) / sum(nPeople * buf_20km, na.rm = T)) %>%
          pivot_longer(cols =c(
            poor_1km, 
            poor_2km, 
            poor_3km, 
            poor_4km, 
            poor_5km, 
            poor_10km,
            poor_15km,
            poor_20km
          ) 
      )

# remove non-numeric characters
poor_buf$name <-
  as.numeric(gsub("[^0-9.]", "", poor_buf$name))

# Uses Emissions Output ---------------------------------------------------


# summarize emission percentages by buffer distance
emission_buf <-
  baseline %>%
  mutate(pm_tot_perc = pm_tot / sum(pm_tot, na.rm = T),
         pm_1km = pm_tot_perc * buf_1km,
         pm_2km = pm_tot_perc * buf_2km,
         pm_3km = pm_tot_perc * buf_3km,
         pm_4km = pm_tot_perc * buf_4km,
         pm_5km = pm_tot_perc * buf_5km,
         pm_10km = pm_tot_perc * buf_10km,
         pm_15km = pm_tot_perc * buf_15km,
         pm_20km = pm_tot_perc * buf_20km) %>%
  summarise(pm_1km = sum(pm_1km, na.rm = T),
            pm_2km = sum(pm_2km, na.rm = T),
            pm_3km = sum(pm_3km, na.rm = T),
            pm_4km = sum(pm_4km, na.rm = T),
            pm_5km = sum(pm_5km, na.rm = T),
            pm_10km = sum(pm_10km, na.rm = T),
            pm_15km = sum(pm_15km, na.rm = T),
            pm_20km = sum(pm_20km, na.rm = T))



# Emissions by Buffer Plot ------------------------------------------------

emission_buf %>%
  pivot_longer(everything()) %>%
  mutate(name = as.numeric(gsub("[^0-9.]", "", name))) %>%
  ggplot() +
  mxblsdl +
  geom_area(aes(name, value), 
            fill = "#FF3030",
            alpha = .9) + # change transparency
  labs(x = "Distance from Urban Areas (km)",
       y = "PM 2.5 Emissions (%)")

ggsave(plot = last_plot(),
       "output/charts/pm_emissions_by_distance.pdf", 
       height = 6, 
       width = 8,
       dpi = 400)

# # table summarizing percent of population in wealth indices
# india %>%
#   group_by(quintiles) %>%
#   summarise(pop_num = sum(nPeople)) %>%
#   mutate(pop_num = pop_num / sum(pop_num)) %>%
#   write.csv(file = "output/tabular/wealth_percentages.csv")


# Cook Type Table ---------------------------------------------------------

# calculate the percent cook type for each area

# extract cooking columns
cook <-
  india %>%
    select(contains("maincook"))

# find maximum value with max.col
# returns index which subsets data frame name
max_cook <-
  colnames(cook)[max.col(cook, ties.method="first")]

# bind back together
india <-
  cbind(india, max_cook)

# india %>%
#   group_by(max_cook) %>%
#   summarise(cook_cat = sum(nHouseholds)) %>%
#   mutate(cook_cat = cook_cat / sum(cook_cat)) %>%
#   write.csv(file = "output/tabular/cook_type.csv")


# get colors ready
display.brewer.all(colorblindFriendly = T)

cols <-
  RColorBrewer::brewer.pal(n = 8, name = 'Set2')

# facet the graph with India as its own
data <-
  data %>%
    mutate(indicator = case_when(zone == "India" ~ 0,
                               T ~ 1))


# Plots -------------------------------------------------------------------


# plot regions and India seperately
data %>%
  filter(zone != "India") %>%
ggplot() +
  theme_minimal() +
  geom_line(aes(name, value, color = zone), lwd = 2) +
  scale_color_manual(values = cols) +
  labs(title = "Population Affected by Buffers",
       x = "Distance",
       y = "Population (%)",
       color = "Zonal Council") +
  scale_y_continuous(limits = c(0,1)) +
  theme(text = element_text(size = 18),
        legend.position = "bottom",
        strip.text.x = element_blank(),
        legend.title = element_blank())

# write to disk
ggsave(filename = "output/charts/population.pdf", 
       dpi = 400, 
        width = 11, 
       height = 8,
       units = "in")


data %>%
  filter(zone == "India") %>%
  ggplot() +
  theme_minimal() +
  geom_line(aes(name, value, color = zone), lwd = 2) +
  scale_color_manual(values = "#303030") +
  labs(title = "Population Affected by Buffers",
       x = "Distance",
       y = "Percent Population",
       color = "") +
  scale_y_continuous(limits = c(0,1)) +
  theme(text = element_text(size = 18),
        legend.position = "bottom",
        strip.text.x = element_blank())

# write to disk
ggsave(filename = "output/charts/population_india.pdf", 
       dpi = 400, 
       width = 11, 
       height = 8,
       units = "in")

# graph poor buffer populations
# plotting
ggplot(poor_buf) +
  mxblsdl +
  geom_area(aes(x = name, y = value), 
            fill = "#FF3030",
            alpha = .5) +
  labs(x = "Distance from Urban Areas (km)",
       y = "Population Considered Poor (%)")

# output plot
ggsave(plot = last_plot(), "output/charts/poor_by_distance.pdf")





