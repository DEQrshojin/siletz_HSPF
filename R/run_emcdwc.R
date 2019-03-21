run_emcdwc <- function(strD = NULL, endD = NULL, wqDir = NULL, emcFil = NULL,
                       basFil = NULL) {
  
  # Libraries, scripts and options ----
  suppressMessages(library('dplyr'))
  
  options(stringsAsFactors = FALSE)

  # CALCULATE OUTFLOW CONCENTRATION AND LOADS
  for (i in 2 : length(latL)) {

    bsn <- lnks[['pOrd']][i - 1, 2] # Retrieve the basin for processing
    
    bcl <- bsn + 1 # Processs basin column
    
    usb <- lnks[['cBas']][[bsn]] # Upstream basin(s)
    
    if (usb != 0) {ucl <- usb + 1} else {ucl <- 0} # Upstream basin columns
    
    # Basin instance (vector) of mass inflows - LATERAL
    IMAT[, bcl] <- latL[, bcl]
    
    # Basin instance (vector) of mass inflows - LATERAL + UPSTREAM REACH(ES)
    if (usb != 0) {
      
      for (k in 1 : length(usb)) IMAT[, bcl] = IMAT[, bcl] + rchL[, ucl[k]]
      
    }
    
    for (j1 in 2 : nrow(rchQ)) {
      
      j0 = j1 - 1 # previous time step
      
      # PAR     PAR           O CONV  Unt1      Unt2  
      xIMAT   = IMAT[j1, bcl]         # kg   -> kg
      xCONCS  = rchC[j0, bcl] * 10^-3 # mg/L -> kg/m3
      xVOLS   = rchV[j0, bcl]         # m3   -> m3
      xSROVOL = rchS[j0, bcl]         # m3   -> m3
      xVOL    = rchV[j1, bcl]         # m3   -> m3
      xEROVOL = rchE[j1, bcl]         # m3   -> m3
      
      # CONC = [IMAT + CONCS * (VOLS - SROVOL)] / (VOL + EROVOL); mg/L
      xCONC <- 10^3 * (xIMAT + xCONCS * (xVOLS - xSROVOL)) / (xVOL + xEROVOL)
      
      rchC[j1, bcl] <- xCONC
      
      # ROMAT = SROVOL * CONCS + EROVOL * CONC
      xROMAT <- xSROVOL * xCONCS + xEROVOL * xCONC * 10^-3 # in kg/m3
      
      rchL[j1, bcl] <- xROMAT
      
    }
    
    cat(paste0('Basin ', bsn, ' complete. Processing time: ',
               round(as.numeric(Sys.time() - a), 2)), 'seconds\n')
    
  }
  
  # Return a list of DFs with flows, loads and concentrations from each reach
  qlcOut <- list(reach_flows = rchQ,
                 reach_loads = rchL,
                 reach_conc  = rchC)
  
  return(qlcOut)
  
}