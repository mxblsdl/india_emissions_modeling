# functionality of poly_census_merge.RMD

polyCensusMerge <- function() {
  # read in the urban dir file and create a new column of the census code number
  udir <- read_dta("../../tabularData/census-link-files_census_2001-2011/udir.dta",encoding = "latin1")
  udir %<>% mutate(code01_full = paste0(stc2001,dtc2001,subdt2001,plcn2001))
  udir %<>% mutate(code11_full = paste0(mdds_stc, mdds_dtc, mdds_subdt, mdds_plcn))
  udir$code01_full %<>% str_pad(width = 16,side = "right",pad="0")
  
  rdir <- read_dta("../../tabularData/census-link-files_census_2001-2011/rdir.dta",encoding = "latin1")
  rdir %<>% mutate(code01_full = paste0(stc2001,dtc2001,subdt2001,plcn2001))
  rdir %<>% mutate(code11_full = paste0(mdds_stc, mdds_dtc, mdds_subdt, mdds_plcn))
  rdir$code01_full %<>% str_pad(width = 16,side = "right",pad="0")
  
}
