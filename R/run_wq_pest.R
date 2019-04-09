#! C:/Program Files/R/R-3.5.3/bin Rscript

for (j in 1) {

  # LIBRARIES, SCRIPTS, OPTIONS ----
  options(stringsAsFactors = FALSE)
  
  sapply(paste0('D:/siletz/scripts/R/', c('run_emcdwc.R',
                                          'calib_wq_pest.R',
                                          'write_wq_2_csv.R')), source)
  
  # COUNTER ----
  countFil <- file('D:/siletz/wqct.txt')
  
  n = as.numeric(readLines(countFil))
  
  # READ IN WQ CONTROL FILE VARIABLES 
  cfv <- read.csv('D:/siletz/wq_confil.csv')
  
  # VARIABLES ----
  pars <- cfv[1, 2]
  stns <- cfv[2, 2]
  strD <- cfv[3, 2]
  endD <- cfv[4, 2]
  wqDir <- cfv[5, 2]
  emcFil <- paste0('D:/siletz/emcdwc_', pars, '.csv')
  basFil <- cfv[6, 2]
  suro <- as.numeric(cfv[7, 2])
  ifwa <- as.numeric(cfv[8, 2])
  ifwb <- as.numeric(cfv[9, 2])
  ifwc <- as.numeric(cfv[10, 2])
  agwa <- as.numeric(cfv[11, 2])
  agwb <- as.numeric(cfv[12, 2])
  agwc <- as.numeric(cfv[13, 2])
  shft <- as.numeric(cfv[14, 2])
  if (length(cfv) == 2) {tmat <- as.numeric(cfv[15, 2])} else {
    tmat <- as.numeric(cfv[15, 2 : 5])}

  # Write wq parameters & seasonality to .csv ----
  df <- write_wq_2_pars(pars = pars, suro = suro, ifwa = ifwa, ifwb = ifwb,
                        ifwc = ifwc, agwa = agwa, agwb = agwb, agwc = agwc,
                        shft = shft, tmat = tmat)
  
  # run_emcdwc ----
  rchQLC <- run_emcdwc(strD = strD, endD = endD, wqDir = wqDir, emcFil = emcFil,
                       basFil = basFil)
  
  saveRDS(rchQLC, 'D:/siletz/calib/wq/rchQLC.RData')
  
  # calib_emcdwc ----
  calStat <- calib_wq_pest(pars = pars, stns = stns, strD = strD, endD = endD,
                           n = n)
  
  write.csv(calStat, file = paste0(wqDir, "/calStat/", pars, '_calStat_', n, '.csv'),
            row.names = FALSE)
  
  # Update the run number and write back to the file
  n = n + 1
  
  writeLines(as.character(n), countFil)
  
  close(countFil)

}
