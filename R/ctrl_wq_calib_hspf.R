rm(list = ls()); cat("\014")

options(stringsAsFactors = FALSE)

# LIBRARIES, SCRIPTS, OPTIONS
sapply(paste0('C:/siletz_tmdl/04_scripts/01_hspf/02_R/',
              c('fnct_utilities_hspf.R',
                'fnct_wq_calib_hspf.R', 'fnct_wq_run_hspf.R')), source)

pars <- c('NOx', 'NH3', 'TKN', 'PO4', 'TP', 'OrC')

for (i in 1 : 6) {

  # READ IN WQ CONTROL FILE VARIABLES (also writes to wq parm csv)
  parF <- paste0('C:/siletz_tmdl/03_models/01_hspf/wq_pars/base/wq_pars_',
                 pars[i], '.csv')

  v <- read_wq_pars(parF, writeCsv = TRUE)

  # calib_emcdwc ----
  calStat <- calib_wq(pars = v$pars, stns = v$stns, strD = v$strD, endD = v$endD,
                      n = 999)

  write.csv(x = calStat, row.names = FALSE,
            file = paste0('C:/siletz_tmdl/02_outputs/01_hspf/calib/plots/',
                                   v$pars, '_calStat_', 999, '.csv'))

  # Check against the 2017 WQ data if run past 2017-07-01
  if (v$endD >= as.Date('2017-07-01', '%Y-%m-%d')) {check_2017_wq(par = v$pars, 999)}

}
