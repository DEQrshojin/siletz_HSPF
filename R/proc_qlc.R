proc_qlc <- function(emc = NULL, parV = NULL, qLat = NULL, lLat = NULL) {
  
  # Synopsis ----
  # This function takes the raw lateral flows/loads/conc and aggregates them into
  # either A) QLC by basin B) QLC by HRU or C) QLC by runoff component
  
  # By basin pre-processing ----
  lstB <- data.frame(BAS = unique(emc$BAS))
  
  if(parV != 'BAS') {
    
    lstPar <- data.frame(unique(emc[, c(parV, 'BAS')]))
    
    pars <- unique(lstPar[-1, 1])
    
    parDim <- length(pars)
    
    if(parV == 'HRU') {x <- 1} else if (parV == 'ROC') {x = 2}
    
  }
  
  # PROCESS FLOWS, LOADS & CONC AGGREGATED BY BASIN ----
  if(parV == 'BAS') {
    
    # Initialize basin data frames
    tQ <- tL <- tC <- data.frame(qLat$Date, matrix(0, nrow = nrow(qLat),
                                                   ncol = (nrow(lstB) - 1)))
    
    names(tQ)[1] <- names(tL)[1] <- names(tC)[1] <- 'Date'
    
    for (i in 2 : nrow(lstB)) {
      
      tInd <- which(emc[, 3] == lstB[i, 1]) # These are the column indx to add
      
      tQ[, i] <- rowSums(qLat[, tInd]) # temporary flow DF of basin i
      
      tL[, i] <- rowSums(lLat[, tInd]) # temporary load DF of basin i
      
      tC[, i] <- rowSums(lLat[, tInd]) / (rowSums(qLat[, tInd] * 3.6))
      
      tC[which(is.nan(tC[, i]) | is.na(tC[, i])), i] <- 0
      
    }
    
    latQLC <- list(flow = tQ, load = tL, conc = tC)
    
    for (j in 1 : length(latQLC)) {
      
      names(latQLC[[j]]) <- c('Date', paste0('Bas', lstB[2 : nrow(lstB), 1]))
      
    }
    
    # PROCESS FLOWS, LOADS & CONC AGGREGATED BY HRU or RUNOFF COMPONENT ----
  } else {
    
    parV = c(parV, 'BAS')
    
    tQ <- tL <- tC <- data.frame(qLat$Date, matrix(0, nrow = nrow(qLat),
                                                   ncol = parDim))
    
    names(tQ) <- names(tL) <- names(tC) <- c('Date', pars)
    
    latQLC <- list()
    
    for (i in 2 : nrow(lstB)) {
      
      tmpEMC <- emc[which(emc$BAS == lstB[i, 1]), ]
      
      for (j in 1 : parDim) {
        
        tInd <- tmpEMC[which(tmpEMC[, x] == pars[j]), 4]
        
        tQ[, j + 1] <- rowSums(qLat[, tInd]) # temporary flow DF of basin i
        
        tL[, j + 1] <- rowSums(lLat[, tInd]) # temporary load DF of basin i
        
        tC[, j + 1] <- rowSums(lLat[, tInd]) / (rowSums(qLat[, tInd] * 3.6))
        
        tC[which(is.nan(tC[, j + 1]) | is.na(tC[, j + 1])), j + 1] <- 0
        
      }
      
      latQLC[['flow']][[i]] <- tQ
      
      latQLC[['load']][[i]] <- tL
      
      latQLC[['conc']][[i]] <- tC
      
    }
    
    for (j in 1 : length(latQLC)) {
      
      latQLC[[j]][[1]] <- NULL
      
      names(latQLC[[j]]) <- paste0('Bas', lstB[2 : nrow(lstB), 1])
      
    }
  }

  return(latQLC)
  
}
