# LIBRARIES, SCRIPTS, OPTIONS ----
options(stringsAsFactors = FALSE, warn = -1)

source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_wq_run_hspf.R')
source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_utilities_hspf.R')
source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_wq_calib_hspf.R')

# COUNTER ----
ctrF <- read_ctrF_H(); fNme <- ctrF$name

# READ IN WQ CONTROL FILE VARIABLES (also writes to wq parm csv)----
v <- read_wq_pars(cf = 'C:/siletz_tmdl/03_models/01_hspf/wq_pars.csv',
                  filN = fNme, writeCsv = TRUE)

# Run the master water quality control script ----
hydr <- paste0('C:/siletz_tmdl/02_outputs/01_hspf/', ctrF$name, '_qOut.RData')

rchQLC <- run_wq(v = v, hydros = hydr)

# Run master WQ control script for lateral flows and load only! ----
latQLC <- proc_wq_latQLC(v = v, qOut = hydr)

# Use the following line only to output discrete lateral loads by HRU, not to
# generate outputs for the Q2K modeling
# latQLC <- proc_wq_latQLC_by_HRU(v = v, qOut = hydr)

# Save the reach flows/loads output files (for calibration)
saveRDS(rchQLC, paste0('C:/siletz_tmdl/02_outputs/01_hspf/', ctrF$name,
                       '_rchQLC_', v$pars, '.RData'))

saveRDS(latQLC, paste0('C:/siletz_tmdl/02_outputs/01_hspf/', ctrF$name,
                       '_latQLC_', v$pars, '.RData'))