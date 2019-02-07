#! C:/Program Files/R/R-3.5.2/bin Rscript

# FUNCTION TO MODIFY HSPF MODEL PARAMETERS FROM CSV
# Ryan Shojinaga, Water Quality Analyst, NRS3, Oregon DEQ
# shojinaga.ryan@deq.state.or.us, 503-229-5777

for (i in 1) {
  
  # Source the functions ----
  sapply(paste0('C:/siletz/scripts/R/',
                c('initialize_components.R',
                  'read_pars.R',
                  'populate_parms.R',
                  'write_parms_to_csv.R')),
         source)
  
  # ARGUEMENTS -----------------------------------------------------------------
  parFil <- 'C:/siletz/parms.csv' # Parameter input file
  basn <- 17 # Number of basins
  indx <- read.csv('C:/siletz/inputs/indx.csv', stringsAsFactors = FALSE)
  
  # READ THE PARAMETER FILE ----
  parm <- read_pars(parFil)
  
  # INITIALIZE THE DATA LIST ----
  comp <- initialize_components(parm, basn, indx) # Add arg for number of basins

  # RUN THE PARAMETER SETTING FUNCTION ----
  comp <- populate_parms(parm, comp)
  
  # WRITE THE PARAMETERS TO THE CSVS ----
  write_parms_to_csv(comp)

}

