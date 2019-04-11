#! C:/Program Files/R/R-3.5.3/bin Rscript

for (j in 1) {

  # LIBRARIES, SCRIPTS, OPTIONS ----
  options(stringsAsFactors = FALSE)
  
  sapply(paste0('D:/siletz/scripts/R/', c('run_emcdwc.R',
                                          'calib_wq_pest.R',
                                          'write_wq_2_csv.R')), source)
  
  # COUNTER ----
  countFil <- file('D:/siletz/wqct.txt')
  
  n <- as.numeric(readLines(countFil))
  
  # READ IN WQ CONTROL FILE VARIABLES 
  v <- strsplit(readLines('D:/siletz/wq_confil.csv'), ',')

  for (i in 1 : length(v)) {
    
    names(v)[i] = v[[i]][1] # Name the element from the first item in the element
    
    v[[i]] <- v[[i]][2 : length(v[[i]])] # Remove the first element
    
  }

  numEls <- c('SURO', 'IFWC', 'IFW1', 'IFW2', 'IFW3', 'IFW4', 'IFW5', 'IFW6',
              'AGWC', 'AGW1', 'AGW2', 'AGW3', 'AGW4', 'AGW5', 'AGW6', 'tmat',
              'ovwr', 'lo', 'hi')

  # Coerce numeric elements to numeric
  for (el in numEls) {v[[el]] <- as.numeric(v[[el]])}
  
  v$emcFil <- paste0('D:/siletz/emcdwc_', v$pars, '.csv')
  
  # Overwrite Harmonic parameters ----
  if (v$ovwr == 1) {
    
    wqDF <- read.csv(paste0(v$wqDir, '/', v$pars, '_', v$stns, '.csv'),
                     stringsAsFactors = F)
    
    wqDF <- wqDF[, c(1, 2)]
    
    wqDF$Date <- as.Date(wqDF$Date, '%Y-%m-%d')
    
    source('D:/siletz/scripts/R/seasonal_wq_conc.R')
    
    fit <- seasonal_wq_conc(wqDF = wqDF, par = v$pars, lo = v$lo, hi = v$hi)
    
    v$IFWC = fit$f90[1]; v$IFW1 = fit$f90[2]; v$IFW2 = fit$f90[3]; v$IFW3 = fit$f90[4]
    v$IFW4 = fit$f90[5]; v$IFW5 = fit$f90[6]; v$IFW6 = fit$f90[7]
    v$AGWC = fit$f10[1]; v$AGW1 = fit$f10[2]; v$AGW2 = fit$f10[3]; v$AGW3 = fit$f10[4]
    v$AGW4 = fit$f10[5]; v$AGW5 = fit$f10[6]; v$AGW6 = fit$f10[7] 
    
  }
  
  # Write wq parameters & seasonality to .csv ----
  df <- write_wq_2_csv(pars = v$pars, tmat = v$tmat, SURO = v$SURO, IFWC = v$IFWC,
                       IFW1 = v$IFW1, IFW2 = v$IFW2, IFW3 = v$IFW3, IFW4 = v$IFW4,
                       IFW5 = v$IFW5, IFW6 = v$IFW6, AGWC = v$AGWC, AGW1 = v$AGW1,
                       AGW2 = v$AGW2, AGW3 = v$AGW3, AGW4 = v$AGW4, AGW5 = v$AGW5,
                       AGW6 = v$AGW6)
  
  # run_emcdwc ----
  rchQLC <- run_emcdwc(strD = v$strD, endD = v$endD, wqDir = v$wqDir,
                       emcFil = v$emcFil, basFil = v$basFil)
  
  saveRDS(rchQLC, 'D:/siletz/calib/wq/rchQLC.RData')
  
  # calib_emcdwc ----
  calStat <- calib_wq_pest(pars = v$pars, stns = v$stns, strD = v$strD,
                           endD = v$endD, n = n)
  
  write.csv(calStat, file = paste0(v$wqDir, "/calStat/", v$pars,
                                   '_calStat_', n, '.csv'), row.names = FALSE)
  
  # Update the run number and write back to the file
  n = n + 1
  
  writeLines(as.character(n), countFil)
  
  close(countFil)
  
}
