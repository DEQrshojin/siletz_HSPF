# LIBRARIES AND OPTIONS ----

options(stringsAsFactors = FALSE)

# LOAD DATA ----
wqFil <- 'C:/siletz/calib/wq'

filDir <- wqFil

sapply(c('C:/siletz/scripts/R/proc_flow_4_wq.R',
         'C:/siletz/scripts/R/proc_qlc.R',
         'C:/siletz/scripts/R/reduce_qlc.R',
         'C:/siletz/scripts/R/proc_routing.R'),
       source)

qOut <- proc_flow_4_wq(wqFil)

# Reduce qOut to specified dates 
# DELETE WHEN THIS IS ALL DONE!!!!##############################################
qLat <- reduce_qlc(strDte = '2004-10-01', endDte = '2004-12-01',
                   df2Red = qOut[["qLat"]])

qRch <- reduce_qlc(strDte = '2004-10-01', endDte = '2004-12-01',
                   df2Red = qOut[['qRch']])
# DELETE WHEN THIS IS ALL DONE!!!!##############################################

emcdwcTmp <- read.csv('C:/siletz/calib/wq/emcdwc.csv') # Load EMC/DWC

# SET CONCENTRATIONS FOR HRUs/RUNOFF ----
emcdwc <- data.frame(do.call(rbind, strsplit(names(qOut[['qLat']]), '_')))

names(emcdwc) <- c('ROC', 'BAS', 'HRU')

emcdwc$indx <- as.numeric(row.names(emcdwc))

emcdwc <- merge(emcdwc, emcdwcTmp, by.x = 'HRU', by.y = 'HRU', all.x = TRUE,
                all.y = FALSE)

# Set the outflow concentration to either EMC (SURO & IFWO) or DWC (AGWO) 
emcdwc$conc <- ifelse(emcdwc$ROC == 'AGWO', emcdwc$DWC, emcdwc$EMC)

emcdwc <- emcdwc[order(emcdwc$indx), ]

row.names(emcdwc) <- emcdwc$indx

# CALCULATE ALL LATERAL LOADS ----
lLat <- qLat # Initialize the df

for (i in 2 : length(qLat)) {lLat[, i] <- qLat[, i] * emcdwc[i, 7] * 3.6}

# Separate out flows, loads and concentrations
qlcLat <- proc_qlc(emc = emcdwc, parV = 'BAS', qLat, lLat) # Basin aggregate
# qlcHRU <- proc_qlc(emc = emcdwc, parV = 'HRU', qLat, lLat) # HRU aggregate
# qlcROC <- proc_qlc(emc = emcdwc, parV = 'ROC', qLat, lLat) # Runoff comp agg
latQ <- qlcLat[['flow']]

latL <- qlcLat[['load']]

latC <- qlcLat[['conc']]

# REACH STUFF ----
# Initialize Reach Components
rchV <- qRch[, c(1, ((length(qRch) - 1) / 2 + 2) : length(qRch))] # Reach Volume 

rchQ <- qRch[, 1 : ((length(qRch) - 1) / 2 + 1)] # Reach outflow

# Reorder because HSPF puts them in a funny order
nOrd <- unique(emcdwc$BAS)

nOrd <- data.frame(BAS = as.numeric(nOrd[-1]))

nOrd$ord <- as.numeric(row.names(nOrd)) + 1

nOrd <- nOrd[order(nOrd$BAS), ]

rchV <- rchV[, c(1, nOrd$ord)]
rchQ <- rchQ[, c(1, nOrd$ord)]
latQ <- latQ[, c(1, nOrd$ord)]
latL <- latL[, c(1, nOrd$ord)]
latC <- latC[, c(1, nOrd$ord)]

rchL <- rchC <- rchS <- rchE <- rchV

# Initialize the df
rchC[, 2 : length(rchC)] <- rchL[, 2 : length(rchL)] <- 0

x1 <- x2 <- x3 <- x4 <- rchC

JS <- 0.5

# Convert reach flow rate (m3/s) to reach out volume (m3/dt)
rchS[, 2 : length(rchS)] <- JS * rchQ[, 2 : length(rchQ)] * 3600

rchE[, 2 : length(rchE)] <- (1 - JS) * rchQ[, 2 : length(rchQ)] * 3600

names(rchQ) <- names(rchV) <- names(rchC) <- c('Date', paste0('Bas', 1 : 17))

names(rchE) <- names(rchL) <- names(rchS) <- c('Date', paste0('Bas', 1 : 17))

# Import reach processing information
shpFile <- 'C:/siletz/inputs/shp/basins.shp'

lnks <- proc_routing(shpFile)

# CALCULATE OUTFLOW CONCENTRATION AND LOADS
for (i in 2 : length(latQ)) {

  bsn <- lnks[['pOrd']][i - 1, 2] # Retrieve the basin for processing
  
  bcl <- bsn + 1 # Processs basin column
  
  usb <- lnks[['cBas']][[bsn]] # Upstream basin(s)
  
  if (usb != 0) {ucl <- usb + 1} else {ucl <- 0} # Upstream basin columns

  # Mass inflows - LATERAL
  IMAT <- latL[, bcl]
  
  # Mass inflows - UPSTREAM REACH(ES)
  if (usb != 0) {
    
    for (k in 1 : length(usb)) IMAT = IMAT + rchL[, ucl[k]]
    
  }

  for (j1 in 2 : nrow(rchQ)) {

    j0 = j1 - 1 # previous time step

    # CONC = [IMAT + CONCS * (VOLS - SROVOL)] / (VOL + EROVOL)
    # Reach ouflow concentration
    # PAR    PAR           O CONV    Unt1    Unt2  
    # ----    ------------ - ----- ------    -----
    xIMAT   = IMAT[j1]              # kg   -> kg
    xCONCS  = rchC[j0, bcl] * 10^-3 # mg/L -> kg/m3
    xVOLS   = rchV[j0, bcl] * 10^6  # Mm3  -> m3
    xSROVOL = rchS[j1, bcl]         # m3   -> m3
    xVOL    = rchV[j1, bcl] * 10^6  # Mm3  -> m3
    xEROVOL = rchE[j1, bcl]         # m3   -> m3
    # --------------------------------------------

    # CONC = (xIMAT + xCONCS * (xVOLS - xSROVOL)) / (xVOL + xEROVOL)
    x1[j1, bcl] <- (xVOLS - xSROVOL)
    x2[j1, bcl] <- xCONCS * (xVOLS - xSROVOL)
    x3[j1, bcl] <- txNmr <- xIMAT + xCONCS * (xVOLS - xSROVOL)
    x4[j1, bcl] <- txDmr <- (xVOL + xEROVOL)

    rchC[j1, bcl] <- txNmr / txDmr * 10^3 # Convert from kg/m3 back to mg/L
    
    # Reach ouflow mass

  }

}











































