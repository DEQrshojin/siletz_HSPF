proc_emcdwc <- function(restart = NULL, strD = NULL, endD = NULL, wqDir = NULL,
                        emcFil = NULL, basFil = NULL) {
  
  # This function takes lateral flows, specifiec concentrations or seasonal
  # concentrations and calculates lateral loads and concentrations based on
  # partitioned runoff components (i.e., surface, interflow, GW). It then uses
  # specified routing information, and flow routing data (reach flows) to 
  # route loads and concentrations to downstream reaches, accepting upstream
  # flows and loads and lateral flows/loads/concentrations, and calculating
  # downstream loads and concentrations.

  # Libraries, scripts and options ----
  options(stringsAsFactors = FALSE)
  suppressMessages(library('dplyr'))
  
  sapply(c('D:/siletz/scripts/R/proc_qlc.R', 'D:/siletz/scripts/R/reduce_qlc.R',
           'D:/siletz/scripts/R/proc_flow_4_wq.R',
           'D:/siletz/scripts/R/proc_network_linkage.R',
           'D:/siletz/scripts/R/initialize_QLC_df.R',
           'D:/siletz/scripts/R/preproc_emcdwc.R',
           'C:/Users/rshojin/Desktop/006_scripts/github/General/day_of_hydro_year.R',
           'C:/Users/rshojin/Desktop/006_scripts/github/General/hydro_year.R'), source)
  
  # Load and process data ----
  # qOut <- proc_flow_4_wq(wqDir)
  qOut <- readRDS('D:/siletz/calib/wq/qOut.RData')

  # Reduce from qOut to lateral loads of specified dates 
  qLat <- reduce_qlc(strDte = strD, endDte = endD, df2Red = qOut[["qLat"]])
  
  # Pre-proces emcdwc table
  nmVec <- names(qOut[['qLat']])
  
  emcdwc <- preproc_emcdwc(nmVec = nmVec, emcFil = emcFil)
  
  lLat <- qLat # Initialize the df
  
  # Extract column indeces of SURO, IFWO and AGWO
  ind <- list(SURO = which(emcdwc$ROC == 'SURO'),
              IFWO = which(emcdwc$ROC == 'IFWO'),
              AGWO = which(emcdwc$ROC == 'AGWO'))
  
  # Create a dataframe to calculate seasonality (periodicity) of the time series
  int <- data.frame(Date = lLat$Date)
  
  int <- int %>%
         mutate(hyr = hydro_year(Date), doy = day_of_hydro_year(Date)) %>%
         mutate(dys = ifelse((hyr %% 4) == 0, 366, 365), yr = hyr - hyr[1]) %>%
         mutate(per = yr + (doy + emcdwc[2, length(emcdwc)]) / dys,
                SURO = 0, IFWO = 0, AGWO = 0)
  
  # CALCULATE LATERAL LOADS ----
  for (i in 1 : 3) {
    
    if(names(ind)[i] == 'SURO') {
      
      for (j in ind[[i]]) {lLat[, j] <- qLat[, j] * emcdwc[j, 'SURO'] * 3.6}
      
    } else {
      
      for (j in ind[[i]]) {
      
        # Select columns for seasonal variation function coefficients
        if (i == 2) {k <- 6 : 8} else {k <- 9 : 11}
        
        intConc <- emcdwc[j, k[1]] * sin(2 * pi * int$per) + 
                   emcdwc[j, k[2]] * cos(2 * pi * int$per) + 
                   emcdwc[j, k[3]]

        lLat[, j] <- qLat[, j] * intConc * 3.6
        
      }
    }
  }
  
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

  rchQ <- initialize_QLC_df(nOrd = nOrd, modDF = rchQ, zero = FALSE)
  
  rchV <- initialize_QLC_df(nOrd = nOrd, modDF = rchV, zero = FALSE)
  
  latL <- initialize_QLC_df(nOrd = nOrd, modDF = latL, zero = FALSE)
  
  RAT <- IMAT <- rchC <- rchS <- rchE <- rchO <- rchL
  
  # RESTART Need restart for rchC and rchL ----
  if (length(restart) > 1) {
    
    rchL[1, ] <- restart[['ldsRst']]
    
    rchC[1, ] <- restart[['conRst']]
    
  }

  # Convert volumes from Mm3 to m3
  rchO[, 2 : length(rchQ)] <- rchQ[, 2 : length(rchQ)] * 3600 # Rch Out Vol (m3)
  
  rchV[, 2 : length(rchV)] <- rchV[, 2 : length(rchV)] * 10^6 # Rch Vol (m3)
  
  # RAT AND CRRAT
  RAT[, 2 : length(rchV)] <- rchV[, 2 : length(rchV)] / rchO[, 2 : length(rchO)]
  
  meanRAT <- colMeans(RAT[, 2 : length(RAT)], na.rm = T)
  
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
    
    if (usb[1] != 0) {ucl <- usb + 1} else {ucl <- 0} # Upstream basin columns
    
    # Basin instance (vector) of mass inflows - LATERAL
    IMAT[, bcl] <- latL[, bcl]
    
    # Basin instance (vector) of mass inflows - LATERAL + UPSTREAM REACH(ES)
    if (usb[1] != 0) {
      
      for (k in 1 : length(usb)) IMAT[, bcl] = IMAT[, bcl] + rchL[, ucl[k]]
      
    }
    
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
  }
  
  # Return a list of DFs with flows, loads and concentrations from each reach
  qlcOut <- list(reach_flows = rchQ, reach_loads = rchL, reach_conc  = rchC)
  
  return(qlcOut)
  
}