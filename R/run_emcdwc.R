run_emcdwc <- function(strD = NULL, endD = NULL, wqDir = NULL, emcFil = NULL,
                       basFil = NULL) {
  
  # Libraries, scripts and options ----
  options(stringsAsFactors = FALSE)
  
  sapply(c('D:/siletz/scripts/R/proc_qlc.R', 'D:/siletz/scripts/R/reduce_qlc.R',
           'D:/siletz/scripts/R/proc_flow_4_wq.R',
           'D:/siletz/scripts/R/proc_network_linkage.R',
           'D:/siletz/scripts/R/initialize_QLC_df.R',
           'D:/siletz/scripts/R/preproc_emcdwc.R'), source)
  
  # Load and process data ----
  # qOut <- proc_flow_4_wq(wqDir)
  qOut <- readRDS('D:/siletz/calib/wq/qOut.RData')

  # Reduce from qOut to lateral loads of specified dates 
  qLat <- reduce_qlc(strDte = strD, endDte = endD, df2Red = qOut[["qLat"]])
  
  # Pre-proces emcdwc table
  nmVec <- names(qOut[['qLat']])
  
  emcdwc <- preproc_emcdwc(nmVec = nmVec, emcFil = emcFil)
  
  lLat <- qLat # Initialize the df
  
  cCol <- which(names(emcdwc) == 'conc')
  
  for (i in 2 : length(qLat)) {lLat[, i] <- qLat[, i] * emcdwc[i, cCol] * 3.6}
  
  # Separate out flows, loads and concentrations
  qlcLat <- proc_qlc(emc = emcdwc, parV = 'BAS', qLat, lLat) # Basin aggregate
  
  latL <- qlcLat[['load']]
  
  # Process reach flows and volume ----
  qRch <- reduce_qlc(strDte = strD, endDte = endD, df2Red = qOut[['qRch']])
  
  rchV <- qRch[, c(1, ((length(qRch) - 1) / 2 + 2) : length(qRch))] # Reach Vol 
  
  rchQ <- qRch[, 1 : ((length(qRch) - 1) / 2 + 1)] # Reach outflow
  
  # Reorder because HSPF puts them in a funny order
  nOrd <- unique(emcdwc$BAS)
  
  # Initialize data frame for zeroed-out DFs
  rchL <- initialize_QLC_df(nOrd = nOrd, modDF = rchV, zero = TRUE)
  
  RAT <- IMAT <- rchC <- rchS <- rchE <- rchO <- rchL
  
  rchQ <- initialize_QLC_df(nOrd = nOrd, modDF = rchQ, zero = FALSE)
  
  rchV <- initialize_QLC_df(nOrd = nOrd, modDF = rchV, zero = FALSE)
  
  latL <- initialize_QLC_df(nOrd = nOrd, modDF = latL, zero = FALSE)
  
  # Convert volumes from Mm3 to m3
  rchO[, 2 : length(rchQ)] <- rchQ[, 2 : length(rchQ)] * 3600 # Rch Out Vol (m3)
  
  rchV[, 2 : length(rchV)] <- rchV[, 2 : length(rchV)] * 10^6 # Rch Vol (m3)
  
  # RAT AND CRRAT
  RAT[, 2 : length(rchV)] <- rchV[, 2 : length(rchV)] / rchO[, 2 : length(rchO)]
  
  meanRAT <- colMeans(RAT[, 2 : length(RAT)])
  
  # Calculate JS and COJS
  JS <- ifelse(meanRAT / 1.5 >= 1, 1, meanRAT / 1.5)
  
  COJS <- 1 - JS
  
  # Convert reach flow rate (m3/s) to reach out volume (m3/dt)
  for (i in 2 : length(rchS)) {
    
    rchS[, i] <- JS[i - 1] * rchQ[, i] * 3600
    
    rchE[, i] <- COJS[i - 1] * rchQ[, i] * 3600
    
  }
  
  # Import reach processing information
  lnks <- proc_network_linkage(basFil)

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
    
    a <- Sys.time()
    
    for (j1 in 2 : nrow(rchQ)) {
      
      j0 = j1 - 1 # previous time step
      
      # PAR     PAR           O CONV    UNIT1   UNIT2  
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
  qlcOut <- list(reach_flows = rchQ, reach_loads = rchL, reach_conc  = rchC)
  
  return(qlcOut)
  
}