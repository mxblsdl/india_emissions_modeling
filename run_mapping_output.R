

# Mapping output -----------------------------------------------------------

source("scripts/emissions_model/load_all.R")

mapping_option = 1

region <- region_list[8] # select india
dist <- buf_list[1] # select any values
constrain_value = 1.5 # manually set to greater than one, forces baseline scenario

# create sf object to represent baseline values
map <-
  emissionsModel(census = dat, 
                 buf_dist = dist,
                 region = region, 
                 constrain_factor = "maincook_lpg",
                 constrain_value = constrain_value)

# subset to columns of interest
map %>%
  names()

# first 60 columns have emissions totals by category
map <-
  map %>%
  select(grep("bc", colnames(.)),
         grep("pm25", colnames(.)),
         grep("co2", colnames(.)),
         # co2_tot, # totals
         # pm_tot,
         # bc_tot,
         # grep("req", colnames(.)), # required and delivered energies
         # grep("del", colnames(.)),
         STATE, # identifying values
         area, 
         NAME_1,
         TRU,
         nPeople,
         nHouseholds,
         zone,
         literacy_rate, # indicator variables
         per_house_weighted,
         wquint_pca, # wealth indices
         wquint_pca_no_lpg,
         buf_1km, # buffer distances
         buf_2km,
         buf_3km, 
         buf_4km, 
         buf_5km,
         buf_10km,
         buf_15km,
         buf_20km)

# addressing issue #1 in draft report
# map %>%
#   select(STATE, area, NAME_1, TRU, nPeople, nHouseholds, zone, contains("pm25_ck"))

# round decimal places
# space saving
map %<>%
  mutate_if(is.numeric, .funs = ~round(., digits = 2))

# alternative base solution to rounding numeric only columns
# nums <- vapply(map, is.numeric, FUN.VALUE = logical(1))
# 
# map[,nums] <- round(map[, nums], digits = 3)


# Write out spatial object
# writing as geopackage since takes long time to write otherwise
st_write(map, "output/spatial/all_em/all_emissions_sf.gpkg", delete_layer = T)

# create dt with no geometry
d_map <- map %>%
  st_set_geometry(NULL) %>%
  as.data.table()

# check each column
sapply(d_map, class, simplify = F)

# write out for analysis
fwrite(d_map, "output/spatial/all_baseline.csv")
# warnings associated with large numbers being written out, consider rounding
