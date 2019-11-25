#! C:/Program Files/R/R-3.5.3/bin Rscript
for (j in 1) {

  # LIBRARIES, SCRIPTS, OPTIONS ----
  options(stringsAsFactors = FALSE)
  
  sapply(paste0('D:/siletz/scripts/R/',c('water_quality_run.R', 'utilities.R',
                                         'water_quality_calib.R')), source)
  
  # COUNTER ----
  countFil <- file('D:/siletz/wqct.txt')
  
  n <- as.numeric(readLines(countFil))
  
  # READ IN WQ CONTROL FILE VARIABLES (also writes to wq parm csv)----
  v <- read_wq_pars('D:/siletz/wq_confil.csv', writeCsv = TRUE)
  
  # Run the master water quality control script ----
  rchQLC <- run_wq(v)

  # rchQLC <- run_wq(strD = v$strD, endD = v$endD, wqDir = v$wqDir,
  #                  emcFil = v$emcFil, basFil = v$basFil)

  # Save the reach flows/loads output files (for calibration)
  saveRDS(rchQLC, paste0('D:/siletz/calib/wq/rchQLC_', v$pars,'.RData'))

  # Run master WQ control script for lateral flows and load only! ----
  # latQLC <- proc_wq_latQLC(strD = v$strD, endD = v$endD, wqDir = v$wqDir,
  #                          emcFil = v$emcFil)
  #
  # Save the lateral flows/loads output files (for input to Q2K) 
  # saveRDS(latQLC, paste0('D:/siletz/calib/wq/latQLC_', v$pars,'.RData'))

  # calib_emcdwc ----
  calStat <- calib_wq(pars = v$pars, stns = v$stns, strD = v$strD, endD = v$endD,
                      n = n)

  write.csv(calStat, file = paste0(v$wqDir, "/calStat/", v$pars,
                                   '_calStat_', n, '.csv'), row.names = FALSE)

  # Check against the 2017 WQ data if run past 2017-07-01
  if (v$endD >= as.Date('2017-07-01', '%Y-%m-%d')) {check_2017_wq(par = v$pars, n)}

  # Copy the control file (confil), rename the copy and move to /calib/parms/wq
  file.copy('D:/siletz/wq_confil.csv',
            paste0('D:/siletz/calib/parms/wq/wq_confil_', v$pars, '_', n, '.csv'))

  # Update the run number and write back to the file
  n = n + 1

  writeLines(as.character(n), countFil)
  
  close(countFil)

}
