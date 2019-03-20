runoff_components <- function(strD = NULL, endD = NULL, wqDir = NULL,
                              emcFil = NULL) {
  
  # Libraries, scripts and options ----
  options(stringsAsFactors = FALSE)
  
  sapply(c('D:/siletz/scripts/R/proc_qlc.R', 'D:/siletz/scripts/R/reduce_qlc.R',
           'D:/siletz/scripts/R/proc_flow_4_wq.R',
           'D:/siletz/scripts/R/preproc_emcdwc.R'), source)
  
  # Load and process data ----
  qOut <- proc_flow_4_wq(wqDir)
  
  # Reduce from qOut to lateral loads of specified dates 
  qLat <- reduce_qlc(strDte = strD, endDte = endD, df2Red = qOut[["qLat"]])
  
  # Pre-proces emcdwc table
  nmVec <- names(qOut[['qLat']])
  
  emcdwc <- preproc_emcdwc(nmVec = nmVec, emcFil = emcFil)
  
  # Calculate lateral loads ----
  lLat <- qLat # Initialize the df
  
  cCol <- which(names(emcdwc) == 'conc')
  
  for (i in 2 : length(qLat)) {lLat[, i] <- qLat[, i] * emcdwc[i, cCol] * 3.6}
  
  # Separate out flows, loads and concentrations
  qlcLat <- proc_qlc(emc = emcdwc, parV = 'ROC', qLat, lLat) # Basin aggregate
  
  roComp <- qlcLat[['flow']]
  
  return(roComp)

}