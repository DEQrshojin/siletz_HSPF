for (j in 1) {

  # LIBRARIES, SCRIPTS, OPTIONS ----
  options(stringsAsFactors = FALSE)
  
  sapply(paste0('D:/siletz/scripts/R/', c('run_emcdwc.R', 'calib_emcdwc.R')),
         source)
  
  # COUNTER ----
  countFil <- file('D:/siletz/wqct.txt')
  
  n = as.numeric(readLines(countFil))
  
  # VARIABLES ----
  for (i in 1) {
    
    pars <- 'nox' # c('tss', 'nox', 'po4', 'tp', 'orc')
    stns <- '10391-ORDEQ'
    strD = '2004-01-01'
    endD = '2018-04-01'
    wqDir = 'D:/siletz/calib/wq'
    emcFil = paste0('D:/siletz/emcdwc_', pars, '.csv')
    basFil = 'D:/siletz/inputs/shp/basins.shp'
    
  }
  
  # run_emcdwc
  rchQLC <- run_emcdwc(strD = strD, endD = endD, wqDir = wqDir, emcFil = emcFil,
                       basFil = basFil)
  
  saveRDS(rchQLC, 'D:/siletz/calib/wq/rchQLC.RData')
  
  # calib_emcdwc
  calStat <- calib_emcdwc(pars = pars, stns = stns, strD = strD, endD = endD, n = n)
  
  write.csv(calStat, file = paste0(wqDir, "/", pars[i], '_calStat_', n, '.csv'),
            row.names = FALSE)
  
  # Update the run number and write back to the file
  n = n + 1
  
  writeLines(as.character(n), countFil)
  
  close(countFil)

}

# wq_data_4_hspf -- COMMENT OUT UNLESS SPECIFIC DATES ARE NEEDED 
# for (i in 1 : length(pars)) {
#   
#   wqDt <- wq_data_4_hspf(strD = strD, endD = endD, pars = pars[i], stns = stns)
#   
#   write.csv(wqDt, file = paste0(wqDir, "/", pars[i], "_", stns, '.csv'),
#             row.names = F)
#   
# }