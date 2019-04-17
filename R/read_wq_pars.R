read_wq_pars <- function(cf, writeCsv = TRUE) { 

  sapply(paste0('D:/siletz/scripts/R/', c('write_wq_2_csv.R')), source)

  # READ IN WQ CONTROL FILE VARIABLES 
  v <- readLines(cf)
  
  # Process out comment lines and blank lines:
  v <- v[-grep('#{3}', v)]
  
  v <- v[lapply(v, length) > 0]

  v <- strsplit(v, ',')
  
  for (i in 1 : length(v)) {
    
    names(v)[i] = v[[i]][1] # Name the element from the first item in the element
    
    v[[i]] <- v[[i]][2 : length(v[[i]])] # Remove the first element
    
  }
  
  # Coerce numeric elements to numeric
  for (el in which(names(v) == 'SURO') : length(v)) {v[[el]] <- as.numeric(v[[el]])}
  
  v$emcFil <- paste0('D:/siletz/emcdwc_', v$pars, '.csv')
  
  # Overwrite Harmonic parameters ----
  if (v$ovwr == 1) {
  
    # Read and process the ambient water quality data file
    wqDF <- read.csv(paste0(v$wqDir, '/', v$pars, '_', v$stns, '.csv'),
                     stringsAsFactors = F)
    
    wqDF <- wqDF[, c(1, 2)]
    
    wqDF$Date <- as.Date(wqDF$Date, '%Y-%m-%d')
    
    source('D:/siletz/scripts/R/seasonal_wq_conc.R')
    
    fit <- seasonal_wq_conc(wqDF = wqDF, par = v$pars, lo = v$lo, hi = v$hi,
                            ts = 'none')
    
    v$IFWC = fit$f90[1] * v$IFM
    v$IFW1 = fit$f90[2]; v$IFW2 = fit$f90[3]; v$IFW3 = fit$f90[4]
    v$IFW4 = fit$f90[5]; v$IFW5 = fit$f90[6]; v$IFW6 = fit$f90[7]
    v$AGWC = fit$f10[1] * v$GWM
    v$AGW1 = fit$f10[2]; v$AGW2 = fit$f10[3]; v$AGW3 = fit$f10[4]
    v$AGW4 = fit$f10[5]; v$AGW5 = fit$f10[6]; v$AGW6 = fit$f10[7] 
    
  }
  
  if (writeCsv) {

    # Write wq parameters & seasonality to .csv ----
    df <- write_wq_2_csv(pars = v$pars, tmat = v$tmat, SURO = v$SURO, IFWC = v$IFWC,
                         IFW1 = v$IFW1, IFW2 = v$IFW2, IFW3 = v$IFW3, IFW4 = v$IFW4,
                         IFW5 = v$IFW5, IFW6 = v$IFW6, AGWC = v$AGWC, AGW1 = v$AGW1,
                         AGW2 = v$AGW2, AGW3 = v$AGW3, AGW4 = v$AGW4, AGW5 = v$AGW5,
                         AGW6 = v$AGW6)
  
  }
  
  return(v)
  
}
