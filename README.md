# Organization

This repo contains the relevant R scripts related to emissions modeling India project.

Code is organized into three main bins.
- Data cleaning
- Emissions modeling (the bulk of the code here)
- Post process and visualization of results

Additional documentation will be in a [google drive](https://docs.google.com/document/d/1DY1Ju0BY99eeneIOzG-JDoQUAZLsuIS6Fmijmu-3jFg/edit#)

# Running the model

Four different scenarios are currently setup. These include:
- run_ems_constraints.R
  - Runs the emissions model varying the constraint value and factor. The constraint factor is a category from census data and accompanying values which are used to inhibit the adoption of fuel switching.
  - Constraint factor and values are set in *scripts/emissions_model/setting.R*
- run_ems_fuel_shift.R
  - Runs the emissions model varying the region, buffer size (affected area from urban zones) with a constant constrain factor. The default constrain factor is 'maincook_lpg' set at 5%. 
- run_baseline.R
  - Runs emissions model with no fuel switching to establish baseline emissions values.
- run_mapping_output.R
  - Runs emissions model but outputs a spatial object instead of tabular dataframes. The spatial output is intended to be used for spatial processing and map creation.

## Setting
- The file *scripts/emissions_model/setting.R* can be adjusted to alter the runs of the emissions model.
