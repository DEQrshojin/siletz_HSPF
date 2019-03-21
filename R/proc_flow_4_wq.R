proc_flow_4_wq <- function(filDir) {

  # LIBRARIES AND OPTIONS ----
  # library(ggplot2)
  
  options(stringsAsFactors = FALSE, scipen = -1, warn = -1)

  # IMPORT DATA ----
  # Import lateral flows (mm/ts) and reach outflows (m3/s)
  qLat <- read.csv(paste0(filDir, '/siletz_out_runoff.csv')) # Lateral
  HRUs <- read.csv(paste0(filDir, '/hru_lng.csv')) # HRU areas
  qRch <- read.csv(paste0(filDir, '/siletz_out_reach_vol.csv')) # Reach flows
  latNme <- read.csv(paste0(filDir, '/siletz_perlnd_runoff_names.csv')) # Names
  
  # ORGANIZE DATA ----
  # Dates
  ts <- seq(as.POSIXct(qRch$Date[1], '%Y-%m-%d %H:%M:%S', tz = 'America/Los_Angeles'),
            as.POSIXct(qRch$Date[nrow(qRch)], '%Y-%m-%d %H:%M:%S', tz = 'America/Los_Angeles'),
            3600)
  
  if (length(ts) < nrow(qRch)) {ts <- c(ts, ts[length(ts)] + 3600)}
  
  qRch$Date <- qLat$Date <- ts
  
  # Lateral inflow names
  oNms <- data.frame(do.call(rbind, strsplit(names(latNme), '_')))
  
  oNms <- oNms[-1, ]
  
  oNms$X1 <- unique(HRUs$HRU)
  
  names(qLat) <- c('Date', paste0(oNms$X3, '_', oNms$X2, '_', oNms$X1))
  
  # Convert the runoff depth (mm) -> flow (m3/s)
  for (i in 1 : nrow(HRUs)) {
    
    qLat[, i + 1] <- qLat[, i + 1] * HRUs[i, 2] * 1000 / 3600     # Surface RO
    
    qLat[, i + 154] <- qLat[, i + 154] * HRUs[i, 2] * 1000 / 3600 # Interflow RO
    
    qLat[, i + 307] <- qLat[, i + 307] * HRUs[i, 2] * 1000 / 3600 # Active GW RO
  
  }
  
  # Fix the dates--they have NAs
  
  
  comp <- list(qLat = qLat, qRch = qRch, HRUs = HRUs)
  
  return(comp)

}
