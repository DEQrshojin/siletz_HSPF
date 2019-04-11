runoff_components <- function(strD = NULL, endD = NULL, wqDir = NULL,
                              emcFil = NULL) {
  
  # Libraries, scripts and options ----
  options(stringsAsFactors = FALSE)
  
  sapply(c('D:/siletz/scripts/R/proc_qlc.R', 'D:/siletz/scripts/R/reduce_qlc.R',
           'D:/siletz/scripts/R/preproc_emcdwc.R',
           'C:/Users/rshojin/Desktop/006_scripts/github/General/day_of_hydro_year.R',
           'C:/Users/rshojin/Desktop/006_scripts/github/General/hydro_year.R'),
         source)
  
  # Load and process data ----
  qOut <- readRDS('D:/siletz/calib/wq/qOut.RData')
  
  # Reduce from qOut to lateral loads of specified dates 
  qLat <- reduce_qlc(strDte = strD, endDte = endD, df2Red = qOut[["qLat"]])
  
  # Pre-proces emcdwc table
  nmVec <- names(qOut[['qLat']])
  
  emcdwc <- preproc_emcdwc(nmVec = nmVec, emcFil = emcFil)
  
  # Calculate lateral loads ----
  lLat <- qLat # Initialize the df

  # Extract column indeces of SURO, IFWO and AGWO
  ind <- list(SURO = which(emcdwc$ROC == 'SURO'),
              IFWO = which(emcdwc$ROC == 'IFWO'),
              AGWO = which(emcdwc$ROC == 'AGWO'))
  
  # Create a dataframe to calculate seasonality (periodicity) of the time series
  int <- data.frame(Date = lLat$Date)
  
  int  <- int %>%
    mutate(hyr = hydro_year(Date), doy = day_of_hydro_year(Date)) %>%
    mutate(dys = ifelse((hyr %% 4) == 0, 366, 365), yr = hyr - hyr[1]) %>%
    mutate(per = yr + doy / dys)
  
  # CALCULATE LATERAL LOADS ----
  for (i in 1 : 3) {
    
    if(names(ind)[i] == 'SURO') {
      
      for (j in ind[[i]]) {lLat[, j] <- qLat[, j] * emcdwc[j, 'SURO'] * 3.6}
      
    } else {
      
      for (j in ind[[i]]) {
        
        # Select columns for seasonal variation function coefficients
        if (i == 2) {k <- 6 : 8} else {k <- 9 : 11}
        
        intConc <- emcdwc[j, k[1]] * (sin(2 * pi * int$per)) + 
          emcdwc[j, k[2]] * (cos(2 * pi * int$per)) + 
          emcdwc[j, k[3]]
        
        lLat[, j] <- qLat[, j] * intConc * 3.6
        
      }
    }
  }   
  
  # Non-seasonal interflow & baseflow concentrations
  # cCol <- which(names(emcdwc) == 'conc')
  # for (i in 2 : length(qLat)) {lLat[, i] <- qLat[, i] * emcdwc[i, cCol] * 3.6}

  # Separate out flows, loads and concentrations
  qlcLat <- proc_qlc(emc = emcdwc, parV = 'ROC', qLat, lLat) # Basin aggregate
  
  roComp <- qlcLat[['flow']]
  
  return(roComp)

}