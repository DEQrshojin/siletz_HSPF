preproc_emcdwc <- function(nmVec = NULL, emcFil = NULL) {

  # Synopsis ----
  # This function takes the raw wq parameter file and processes it for use by 
  # proc_emcdwc(). This includes creating a data frame with all of the HRUs and 
  # runoff components and all of the wq concentrations and seasonal harmonic 
  # parameters
  
  # Set concentrations from emcdwc inputs
  emcdwc <- data.frame(do.call(rbind, strsplit(nmVec, '_')))
  
  names(emcdwc) <- c('ROC', 'BAS', 'HRU')
  
  emcdwc$indx <- as.numeric(row.names(emcdwc))
  
  # Bring in the concentration values
  emcdwc2 <- read.csv(emcFil)
  
  emcdwc2 <- rbind(emcdwc2, emcdwc2[1 : 4, ])
  
  emcdwc2$HRU[1 : 4] <- paste0(emcdwc2$HRU[1 : 4], 'HI')
  
  emcdwc2$HRU[6 : 9] <- paste0(emcdwc2$HRU[6 : 9], 'LO')
  
  # Combine the tables
  emcdwc <- merge(emcdwc, emcdwc2, by.x = 'HRU', by.y = 'HRU',
                  all.x = TRUE, all.y = FALSE)
  
  # Set the outflow concentration to either EMC (SURO & IFWO) or DWC (AGWO) 
  # emcdwc$conc <- ifelse(emcdwc$ROC == 'AGWO', emcdwc$AGWO,
  #                       ifelse(emcdwc$ROC == 'IFWO', emcdwc$IFWO, emcdwc$SURO))
  
  emcdwc <- emcdwc[order(emcdwc$indx), ]
  
  row.names(emcdwc) <- emcdwc$indx
  
  return(emcdwc)

}