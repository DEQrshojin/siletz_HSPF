# LIBRARIES AND OPTIONS ----

options(stringsAsFactors = FALSE)

# LOAD DATA ----
wqFil <- 'D:/siletz/calib/wq'

source('D:/siletz/scripts/R/proc_flow_4_wq.R')

qOut <- proc_flow_4_wq(wqFil)

# Load EMC/DWC
emcdwcTmp <- read.csv('D:/siletz/emcdwc.csv')

# SET CONCENTRATIONS FOR HRUs/RUNOFF ----
# Set EMC/DWC for each column
emcdwc <- data.frame(do.call(rbind, strsplit(names(qOut[['qLat']]), '_')))

names(emcdwc) <- c('ROC', 'BAS', 'HRU')

emcdwc$indx <- as.numeric(row.names(emcdwc))

emcdwc <- merge(emcdwc, emcdwcTmp, by.x = 'HRU', by.y = 'HRU', all.x = TRUE,
                all.y = FALSE)

# Set the outflow concentration to either EMC (SURO & IFWO) or DWC (AGWO) 
emcdwc$conc <- ifelse(emcdwc$ROC == 'AGWO', emcdwc$DWC, emcdwc$EMC)

# Tidy up!
emcdwc <- emcdwc[order(emcdwc$indx), ]

row.names(emcdwc) <- emcdwc$indx

# DELETE THESE LINES WHEN DONE WITH FUNCTION!!! --------------------------------
# Pare q data down for the moment for processing times
ts <- as.POSIXct(c('2004-09-01', '2004-11-01'), '%Y-%m-%d',
                 tz = 'America/Los_Angeles')

ts <- seq(ts[1], ts[2], 3600)


qLat <- qOut[['qLat']][qOut[['qLat']]$Date %in% ts, ]
# DELETE THESE LINES WHEN DONE WITH FUNCTION!!! --------------------------------

# CALCULATE ALL LATERAL LOADS ----
# Initialize the lateral water quality loads 
lLat <- qLat

# Calculate load (kg) per time interval (hour)
for (i in 2 : length(qLat)) {lLat[, i] <- qLat[, i] * emcdwc[i, 7] * 3.6}

# REACH STUFF ----
# Set initial Condition


# Loop through each basin and 



# SCRATCH ----
tmp1 <- qLat1[6550 : 6850, ]
tmp2 <- lLat1[6550 : 6850, ]
tmp3 <- cLat1[6550 : 6850, ]
tmp4 <- qLat[6550 : 6850, ]
tmp5 <- lLat[6550 : 6850, ]
write.csv(tmp1, 'D:/siletz/calib/wq/testQ.csv', row.names = FALSE)
write.csv(tmp2, 'D:/siletz/calib/wq/testL.csv', row.names = FALSE)
write.csv(tmp3, 'D:/siletz/calib/wq/testC.csv', row.names = FALSE)
write.csv(tmp4, 'D:/siletz/calib/wq/testQ_all.csv', row.names = FALSE)
write.csv(tmp5, 'D:/siletz/calib/wq/testL_all.csv', row.names = FALSE)




# ----
# pass emc/dwc data frame and a vector of the parameters to isolate
emc
parV <- c('ROC') # BAS/HRU/ROC # ROC = Runoff Component, e.g., GW, intrflw
qLat
lLat

proc_qlc <- function(emc, parV, qLat, lLat) {
  
  lstB <- data.frame(BAS = unique(emc$BAS))
  
  if(parV != 'BAS') {
    
    lstPar <- data.frame(unique(emc[, c(parV, 'BAS')]))
    
    pars <- unique(lstPar[-1, 1])
    
    parDim <- length(pars)
    
    if(parV == 'HRU') {x <- 1} else if (parV == 'ROC') {x = 2}
    
  }

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
      
    }
    
    latQLC <- list(flow = tQ, load = tL, conc = tC)

  } else {
    
    parV = c(parV, 'BAS')
    
    bInd <- bLst <- list()

    tQ <- tL <- tC <- data.frame(qLat$Date, matrix(0, nrow = nrow(qLat),
                                                   ncol = parDim))
    
    names(tQ) <- names(tL) <- names(tC) <- c('Date', pars)
    
    tLst <- list()
    
    for (i in 2 : nrow(lstB)) {

      tmpEMC <- emc[which(emc$BAS == as.character(i - 1)), ]
      
      for (j in 1 : parDim) {
        
        tInd <- tmpEMC[which(tmpEMC[, x] == pars[j]), 4]
        
        tQ[, i] <- rowSums(qLat[, tInd]) # temporary flow DF of basin i
        
        tL[, i] <- rowSums(lLat[, tInd]) # temporary load DF of basin i
        
        tC[, i] <- rowSums(lLat[, tInd]) / (rowSums(qLat[, tInd] * 3.6))
        
        tLst[[j]] <- list(flow = tQ, load = tL, conc = tC)
        
        names(tLst)[j] <- pars[j]
        
      }
      
      

    }

    latQLC <- list(flow = tQ, load = tL, conc = tC)

  }

  return(latQLC)
  
}
