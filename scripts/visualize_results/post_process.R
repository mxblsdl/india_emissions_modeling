## Post process some of the data

# import baseline emissions output of India

# libraries
library(sf)
library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)

# load large sf output object
india <-
  fread("output/spatial/all_baseline.csv", stringsAsFactors = F)

# reorder so STATE is easier to see
india <-
  india %>%
  select(STATE, everything())

# summarize emission percentages by buffer distance
emission_buf <-
  india %>%
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


# plot out the emissions by buffer distance
emission_buf %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = as.numeric(gsub("\\D+", "",name))) %>%
  ggplot() +
  theme_minimal(base_size = 16) +
  geom_area(aes(x = name, y = value, fill = "red")) +
  theme(legend.position = "none") +
  labs(y = "Percent emissions of PM 2.5",
       x = "Buffer Distance")

# output plot
# ggsave(plot = last_plot(), "output/charts/pm_emissions_by_distance.pdf")

# load state key
state_key <-
  read.csv("sup_info/state_key.csv", stringsAsFactors = F)

# remove nas
state_key <-
  na.omit(state_key)

india <-
  india %>%
  mutate(STATE_N = case_when(STATE == 1 ~ "JAMMU & KASHMIR",
                           STATE == 2 ~ "HIMACHAL PRADESH",
                           STATE == 3 ~ "PUNJAB",
                           STATE == 4 ~ "PUNJAB",
                           STATE == 5 ~ "UTTARAKHAND",
                           STATE == 6 ~ "HARYANA",
                           STATE == 7 ~ "DELHI",
                           STATE == 8 ~ "RAJASTHAN",
                           STATE == 9 ~ "UTTAR PRADESH",
                           STATE == 10 ~ "BIHAR",
                           STATE == 11 ~ "SIKKIM",
                           STATE == 13 ~ "NAGALAND",
                           STATE == 14 ~ "MANIPUR",
                           STATE == 16 ~ "TRIPURA",
                           STATE == 17 ~ "MEGHALAYA",
                           STATE == 18 ~ "ASSAM",
                           STATE == 19 ~ "WEST BENGAL",
                           STATE == 20 ~ "JHARKHAND",
                           STATE == 21 ~ "ODISHA",
                           STATE == 22 ~ "CHHATTISGARH",
                           STATE == 23 ~ "MADHYA PRADESH",
                           STATE == 24 ~ "GUJARAT",
                           STATE == 27 ~ "MAHARASHTRA",
                           STATE == 28 ~ "ANDHRA PRADESH",
                           STATE == 29 ~ "KARNATAKA",
                           STATE == 30 ~ "GOA",
                           STATE == 32 ~ "KERALA",
                           STATE == 33 ~ "TAMIL NADU",
                           STATE == 34 ~ "PUDUCHERRY",
                           STATE == 35 ~ "ANDAMAN & NICOBAR ISLANDS",
                           T ~ "OTHER"))

# check number of other states, essentially unassigned states
  india %>%
    mutate(pm_ck = rowSums(select(., contains("pm25_ck"))),
           pm_sh = rowSums(select(., contains("pm25_sh"))),
           pm_wh = rowSums(select(., contains("pm25_wh"))),
           pm_lt = rowSums(select(., contains("pm25_lt")))) %>%
           is.na() %>%
           sum()
  
# summarizing some of the data by location
sum_stats <-
  india %>%
  mutate(pm_ck = rowSums(select(., contains("pm25_ck"))), # total by energy service
         pm_sh = rowSums(select(., contains("pm25_sh"))),
         pm_wh = rowSums(select(., contains("pm25_wh"))),
         pm_lt = rowSums(select(., contains("pm25_lt")))) %>%
  group_by(STATE_N) %>%
  summarise(pm25 = sum(pm_tot, na.rm = T), # summarise data
            co2 = sum(co2_tot, na.rm = T),
            bc = sum(bc_tot, na.rm = T),
            pop = sum(nPeople, na.rm = T),
            pm_ck = sum(pm_ck, na.rm = T),
            pm_sh = sum(pm_sh, na.rm = T),
            pm_wh = sum(pm_wh, na.rm = T),
            pm_lt = sum(pm_lt, na.rm = T)) %>%
  ungroup() %>%
  mutate(perc_tot_pm25 = pm25 / sum(pm25, na.rm = T) * 100, # calculate percentages
         perc_tot_co2 = co2 / sum(co2, na.rm = T) * 100,
         perc_tot_bc = bc / sum(bc, na.rm = T) * 100,
         perc_tot_pop = pop / sum(pop) * 100) %>%
  select(-c(pm25, co2, bc, pop)) # drop net columns

# display data in console
sum_stats %>%
  as.data.frame()

# save outputs
# fwrite(sum_stats, "output/tabular/emissions_summary.csv")


# Percent people who use solid fuels --------------------------------------

# this will be based on full data before any modeling
india_full <- readRDS("output/spatial/india_with_indicies.rds")

solid_fuels_prop <-
  india_full %>%
  # create aggregate column of solid fuel usage
  mutate(solid_fuels = rowSums(select(., 
                                            maincook_wood,
                                            maincook_coalLigChar,
                                            maincook_cropres,
                                            maincook_dung)),
         # calculate subset of people within each buffer who use solid fuels
         sf_1km = solid_fuels * buf_1km * nPeople,
         sf_2km = solid_fuels * buf_2km * nPeople,
         sf_3km = solid_fuels * buf_3km * nPeople,
         sf_4km = solid_fuels * buf_4km * nPeople,
         sf_5km = solid_fuels * buf_5km * nPeople,
         sf_10km = solid_fuels * buf_10km * nPeople,
         sf_15km = solid_fuels * buf_15km * nPeople,
         sf_20km = solid_fuels * buf_20km * nPeople,
         # calculate people within each buffer
         peop_1km= buf_1km * nPeople,
         peop_2km = buf_2km * nPeople,
         peop_3km = buf_3km * nPeople,
         peop_4km = buf_4km * nPeople,
         peop_5km = buf_5km * nPeople,
         peop_10km = buf_10km * nPeople,
         peop_15km = buf_15km * nPeople,
         peop_20km = buf_20km * nPeople) %>%
  summarise(sf_1km = sum(sf_1km, na.rm = T) / sum(peop_1km),
            sf_2km = sum(sf_2km, na.rm = T) / sum(peop_2km),
            sf_3km = sum(sf_3km, na.rm = T) / sum(peop_3km),
            sf_4km = sum(sf_4km, na.rm = T) / sum(peop_4km),
            sf_5km = sum(sf_5km, na.rm = T) / sum(peop_5km),
            sf_10km = sum(sf_10km, na.rm = T) / sum(peop_10km),
            sf_15km = sum(sf_15km, na.rm = T) / sum(peop_15km),
            sf_20km = sum(sf_20km, na.rm = T) / sum(peop_20km))

# Plot out results
solid_fuels_prop %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = as.numeric(gsub("\\D+", "",name))) %>%
  ggplot() +
  theme_minimal(base_size = 16) +
  geom_area(aes(x = name, y = value, fill = "red")) +
  theme(legend.position = "none") +
  labs(y = "Percent Users of Solid Fuels",
       x = "Buffer Distance",
       caption = "Solid Fuels defined as wood, dung, crop residues, or coal lig char")

# The plot shows that as you go farther from urban areas the proportion of people using solid fuels also increases
ggsave(plot = last_plot(), 
       "output/imgs/percent_solid_fuels.png",
       dpi = 500)



# Population Density Map --------------------------------------------------

# convert to sf object
india_full <-
  india_full %>%
  st_as_sf()

pop <- india_full[1:10000, ] %>%
  mutate(`Population Density` = as.numeric(pop_density)) %>%
  ggplot() +
  theme_void() +
  geom_sf(aes(fill = `Population Density`), lwd = 0, colour = NA) + 
  scale_fill_viridis_c() # color varient

pop

ggsave(filename = "output/spatial/pop_den.png", plot = pop, device = NULL)


write_sf(india_full, "output/spatial/for_mapping.shp")


