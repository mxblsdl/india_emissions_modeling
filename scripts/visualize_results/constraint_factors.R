

# helper function
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

## function for preparing data to show fuel constraints
constraintFunction <- function(dataframe, constraint, constraint_seq, caps = T) {

  dat_z <- split(dataframe, dataframe$zonal_council) # create list by region
  
  # Created nested list for each factor and council zone
  eff_zones <- lapply(dat_z, function(x) {
    lapply(constraint_seq, function(y) {
      x %>% 
        transmute(eff = case_when(!!as.name(constraint) <= y ~ 0, # when the constraint is less than the constraint factor assign a 0, meaning the polygon is below a threshold and is not affected
                                  TRUE ~ 1) ) %>% 
        count(eff) %>% # get number of polys meeting or not meeting requirement 
        transmute(perc = n/sum(n)) # calculate as a percentage for each region and constraint factor
    })
  })
  
  
  eff_zones <- sapply(eff_zones, simplify = F, USE.NAMES = T, function(x)  {
    t <-  do.call(cbind, x) # combine all values into data.frame
    colnames(t) <- constraint_seq
    t <-  t[1,] %>% # take only the first value of percent affected
      gather()
    return(t)
  })
  
  # bind rows and create new column based on list name ### SUPER USEFUL
  eff_zones <- bind_rows(eff_zones, .id = "zone")
  
  all_constrain <- lapply(constraint_seq, function(x) {
    dat %>% 
      transmute(eff = case_when(!!as.name(constraint) <= x ~ 0,
                                TRUE ~ 1) ) %>% 
      count(eff) %>% 
      transmute(perc = n/sum(n))  
  })
  all_constrain <- do.call(cbind, all_constrain)
  
  # rename columns and rows
  colnames(all_constrain) <- constraint_seq  
  rownames(all_constrain) <- c(1, 0)
  
  # restructure df
  all_constrain <- all_constrain[1,] %>% 
    gather() %>% 
    mutate(zone = "national")
  
  #all_constrain$zone <- sapply(all_constrain$zone, simpleCap)
 
  all_constrain <- rbind(all_constrain, eff_zones)
  
  if(caps == T) {
    all_constrain$zone <- sapply(all_constrain$zone, simpleCap)
    return(all_constrain)
  }
  return(all_constrain)

}
