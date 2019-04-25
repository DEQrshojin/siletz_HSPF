proc_emcdwc <- function(restart = NULL, strD = NULL, endD = NULL, wqDir = NULL,
                        emcFil = NULL, basFil = NULL) {
  
  # Synopsis ----
  # This function takes lateral flows, specified concentrations or seasonal
  # concentrations (emc/dwc) and calculates lateral loads and concentrations based
  # on partitioned runoff components (i.e., surface, interflow, GW). It then uses
  # specified routing information, and flow routing data (reach flows) to route
  # loads and concentrations to downstream reaches, accepting upstream flows and
  # loads and lateral flows/loads/concentrations, and calculating downstream loads
  # and concentrations.

  # Libraries, scripts and options ----
  options(stringsAsFactors = FALSE)
  
  suppressMessages(library('dplyr'))
  
  sapply(c(paste0('D:/siletz/scripts/R/', c('proc_qlc.R', 'reduce_qlc.R',
                                            'proc_network_linkage.R',
                                            'initialize_QLC_df.R',
                                            'preproc_emcdwc.R','read_wq_pars.R')),
           'C:/Users/rshojin/Desktop/006_scripts/github/General/day_of_hydro_year.R',
           'C:/Users/rshojin/Desktop/006_scripts/github/General/hydro_year.R'),
         source)
  
  # Load and process data ----
  # qOut <- proc_flow_4_wq(wqDir) Not needed -> current qOut = whole hydro period
  qOut <- readRDS('D:/siletz/calib/wq/qOut.RData')

  # Reduce from qOut to lateral loads of specified dates 
  qLat <- reduce_qlc(strDte = strD, endDte = endD, df2Red = qOut[["qLat"]])
  
  # Pre-proces emcdwc table
  nmVec <- names(qOut[['qLat']])
  
  # SIB = Surface, interflow, baseflow runoff concentration parameters
  sib <- preproc_emcdwc(nmVec = nmVec, emcFil = emcFil)
  
  lLat <- qLat # Initialize the df

  # Extract column indeces of SURO, IFWO and AGWO
  ind <- list(SURO = which(sib$ROC == 'SURO'), IFWO = which(sib$ROC == 'IFWO'),
              AGWO = which(sib$ROC == 'AGWO'))
  
  # Create a dataframe to calculate seasonality (periodicity) of the time series
  it <- data.frame(Date = lLat$Date)
  
  it <- it %>%
        mutate(hyr = hydro_year(Date), doy = day_of_hydro_year(Date)) %>%
        mutate(dys = ifelse((hyr %% 4) == 0, 366, 365), yr = hyr - hyr[1]) %>%
        mutate(p = yr + (doy + sib[2, length(sib)]) / dys)
  
  # Calculate lateral loads ----
  # Catchment Inflows - calculates from all three RO components (SURO, IFWO & AGWO)
  for (i in 1 : 3) {
    
    if(names(ind)[i] == 'SURO') {
      
      for (j in ind[[i]]) {lLat[, j] <- qLat[, j] * sib[j, 'SURO'] * 3.6}
      
    } else {
      
      for (j in ind[[i]]) {
      
        # Select columns for seasonal variation function coefficients
        if (i == 2) {k <- 6 : 12} else {k <- 13 : 19}
        
        # Calculate the concentration for each HRU and baseQ/itfwQ component
        itCnc <- sib[j, k[1]] +
                 sib[j, k[2]] * sin(2*pi*it$p) + sib[j, k[3]] * cos(2*pi*it$p) + 
                 sib[j, k[4]] * sin(4*pi*it$p) + sib[j, k[5]] * cos(4*pi*it$p) + 
                 sib[j, k[6]] * sin(6*pi*it$p) + sib[j, k[7]] * cos(6*pi*it$p)
        
        # Break if any concentrations is less than 0
        if (length(which(itCnc < 0)) != 0) {
          
          print(paste0('Warning: negative concentrations in ', names(qLat)[j]))
        
        }
        
        # Apply to lateral flows
        lLat[, j] <- qLat[, j] * itCnc * 3.6
        
      }
    }
  }

  # Pull in the control file variables
  v <- read_wq_pars('D:/siletz/wq_confil.csv', writeCsv = FALSE)
  
  if (v$mod_loads == 1) {
   
    x <- as.numeric(unique(sib$BAS[2 : length(sib$BAS)]))
    
    v$modB <- v$modB[order(x)]; v$modL <- v$modL[order(x)]
  
    # Process lateral Q&L  ----
    # Modified loads (Red alder community multpliers)
    for (i in 1 : length(v$modB)) {
      
      # Modify for specific basins, only for forested and AGWO & IFWO
      cond <- which(sib$BAS == v$modB[i] &
                   (sib$HRU == 'FORHI' | sib$HRU == 'FORLO') &
                    sib$ROC != 'SURO')
      
      lLat[, cond] <- lLat[, cond] * v$modL[i]
      
    }
  }

  # Separate out flows, loads and concentrations
  qlcLat <- proc_qlc(emc = sib, parV = 'BAS', qLat, lLat) # Basin aggregate
  
  # Output lateral flows, loads and concentrations
  # saveRDS(qlcLat, 'D:/siletz/calib/wq/latQLC.RDS')

  latL <- qlcLat[['load']]

  # Added loads (OSSF or animal unit (livestock) feces); use Qual-2Kw for NPDES
  if (v$add_loads == 1) { 
  
    basn <- paste0('Bas', v$addB)
    
    for (i in 1 : length(v$addB)) {latL[[basn[i]]] <- latL[[basn[i]]] + v$addL[i]}

  }
  
  # Pre-process reach flows & loads for reach processing ----
  qRch <- reduce_qlc(strDte = strD, endDte = endD, df2Red = qOut[['qRch']])
  
  rchV <- qRch[, c(1, ((length(qRch) - 1) / 2 + 2) : length(qRch))] # Reach Vol 
  
  rchQ <- qRch[, 1 : ((length(qRch) - 1) / 2 + 1)] # Reach outflow
  
  # Reorder because HSPF puts them in a funny order
  nOrd <- unique(sib$BAS)
  
  # Initialize data frame for zeroed-out DFs
  rchL <- initialize_QLC_df(nOrd = nOrd, modDF = rchV, zero = TRUE)

  rchQ <- initialize_QLC_df(nOrd = nOrd, modDF = rchQ, zero = FALSE)
  
  rchV <- initialize_QLC_df(nOrd = nOrd, modDF = rchV, zero = FALSE)
  
  latL <- initialize_QLC_df(nOrd = nOrd, modDF = latL, zero = FALSE)
  
  RAT <- IMAT <- rchC <- rchS <- rchE <- rchO <- rchL
  
  # Restart for rchC and rchL ----
  if (length(restart) > 1) {
    
    rchL[1, ] <- restart[['ldsRst']]
    
    rchC[1, ] <- restart[['conRst']]
    
  }

  # Process reach flows and volume ----
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

  # Calculate reach outflow loads and concentrations ----
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
      
      j0 = j1 - 1 # Previous time step
      
      # PAR     PAR           O CONV    UNIT1   UNIT2  
      xIMAT   = IMAT[j1, bcl]         # kg   -> kg
      xCONCS  = rchC[j0, bcl] * 10^-3 # mg/L -> kg/m3
      xVOLS   = rchV[j0, bcl]         # m3   -> m3
      xSROVOL = rchS[j0, bcl]         # m3   -> m3
      xVOL    = rchV[j1, bcl]         # m3   -> m3
      xEROVOL = rchE[j1, bcl]         # m3   -> m3

      # Reach outflow concentration_____________________________________________
      # CONC = [IMAT + CONCS * (VOLS - SROVOL)] / (VOL + EROVOL); mg/L
      xCONC <- 10^3 * (xIMAT + xCONCS * (xVOLS - xSROVOL)) / (xVOL + xEROVOL)
      
      # First-order decay (mass loss - NOT CORRECTED FOR TEMP)__________________
      # DDQALT = DQAL * (1.0 - EXP(-KTOTD)) * VOL = loss of qual from decay
      # Volume not factored because using outflow concentration as 'mass' term
      xCONC <- xCONC * exp(-v$kGen / 24)
      
      # Reach outflow load______________________________________________________
      # ROMAT = SROVOL * CONCS + EROVOL * CONC
      xROMAT <- xSROVOL * xCONCS + xEROVOL * xCONC * 10^-3 # in kg

      # Assign reach concentration and load to the time-series data frame_______
      rchC[j1, bcl] <- xCONC; rchL[j1, bcl] <- xROMAT
      
    }
  }

  # Prep outputs ----
  # Return a list of DFs with flows, loads and concentrations from each reach
  qlcOut <- list(reach_flows = rchQ, reach_loads = rchL, reach_conc  = rchC)
  
  return(qlcOut)
  
}
