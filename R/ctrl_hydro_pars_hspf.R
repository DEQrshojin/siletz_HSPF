# SCRIPT TO MODIFY HSPF MODEL PARAMETERS FROM CSV
# Ryan Shojinaga, Water Quality Analyst, NRS3, Oregon DEQ
# shojinaga.ryan@deq.state.or.us, 503-229-5777

# Source the functions ----
source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_utilities_hspf.R')

ctrF <- read_ctrF_H()

if (ctrF$bldM == 'TRUE') {
  
  source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_hydro_pars_hspf.R')

  # ARGUEMENTS -----------------------------------------------------------------
  parFil <- 'C:/siletz_tmdl/03_models/01_hspf/hydro_pars.csv' # Parameter input file
  basn <- 17 # Number of basins
  indx <- read.csv('C:/siletz_tmdl/01_inputs/01_hspf/lulc/indx.csv',
                   stringsAsFactors = FALSE)
  
  # READ THE PARAMETER FILE ----
  parm <- read_pars(parFil)
  
  # INITIALIZE THE DATA LIST ----
  comp <- initialize_components(parm, basn, indx) # Add arg for number of basins
  
  # RUN THE PARAMETER SETTING FUNCTION ----
  comp <- populate_pars(parm, comp)
  
  # WRITE THE PARAMETERS TO THE CSVS ----
  write_pars_to_csv(comp)
  
}
