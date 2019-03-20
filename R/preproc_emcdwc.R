preproc_emcdwc <- function(nmVec = NULL, emcFil = NULL) {

  # Set concentrations from emcdwc inputs
  emcdwc <- data.frame(do.call(rbind, strsplit(nmVec, '_')))
  
  names(emcdwc) <- c('ROC', 'BAS', 'HRU')
  
  emcdwc$indx <- as.numeric(row.names(emcdwc))
  
  emcdwc <- merge(emcdwc, read.csv(emcFil), by.x = 'HRU', by.y = 'HRU',
                  all.x = TRUE, all.y = FALSE)
  
  # Set the outflow concentration to either EMC (SURO & IFWO) or DWC (AGWO) 
  emcdwc$conc <- ifelse(emcdwc$ROC == 'AGWO', emcdwc$AGWO,
                        ifelse(emcdwc$ROC == 'IFWO', emcdwc$IFWO, emcdwc$SURO))
  
  emcdwc <- emcdwc[order(emcdwc$indx), ]
  
  row.names(emcdwc) <- emcdwc$indx
  
  return(emcdwc)

}