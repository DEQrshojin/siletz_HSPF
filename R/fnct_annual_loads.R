annual_loads <- function(scen = NULL, par = NULL, type = 'total') {
  
  dir <- 'C:/siletz_tmdl/02_outputs/01_hspf/'
  
  loads <- readRDS(paste0(dir, scen, '_latQLC_', par, '.RData'))
  
  loads <- loads[['load']]

  # Need to re-order the basins (HSPF re-ordering)
  nms <- names(loads)[2 : (length(loads))]
  
  nms <- data.frame(hspfOrder = as.numeric(gsub('Bas', '', nms)),
                    numrOrder = (1 : length(nms)) + 1)
  
  nms <- nms[order(nms$hspfOrder), ]
  
  loads <- loads[, c(1, nms$numrOrder)]

  # Calculate hydrologic year for aggregation
  loads$hy <- hydro_year(loads$Date)
  
  # Remove the first and last years (incomplete)
  loads <- loads[-which(loads$hy == max(loads$hy) | loads$hy == min(loads$hy)), ]
  
  # Annual loads in tons
  annLoads <- aggregate(loads[2 : (length(loads) - 1)], by = list(loads$hy),
                        FUN = sum)
  
  if (type == 'aerial') {
    
    # Import basin areas
    basAr <- read.csv(paste0('C:/siletz_misc/from_tmdl_drive/001_data/nlcd_2011',
                             '/siletz_HRU_summary_RMS.csv'), stringsAsFactors = F)
    
    basAr <- rowSums(basAr)
    
    for (i in 2 : length(annLoads)) {
      
      annLoads[, i] <- annLoads[, i] / basAr[i - 1]
      
    }    
  }

  loads <- round(colMeans(annLoads[, 2 : length(annLoads)]), 2)
  
  basNms <- names(loads)
  
  loads <- data.frame(Basin = basNms, loads)
  
  return(loads)
  
}

annual_flows <- function(scen = NULL, type = 'total') {

  # Returns annual flows 
  
  dir <- 'C:/siletz_tmdl/02_outputs/01_hspf/'
  
  tmp1 <- readRDS(paste0(dir, scen, '_latQLC_NOx.RData'))
  
  tmp1 <- tmp1[['flow']]
  
  # Reorder
  nms <- names(tmp1)[2 : (length(tmp1))]
  
  nms <- data.frame(hspfOrder = as.numeric(gsub('Bas', '', nms)),
                    numrOrder = (1 : length(nms)) + 1)
  
  nms <- nms[order(nms$hspfOrder), ]
  
  tmp1 <- tmp1[, c(1, nms$numrOrder)]

  # Convert flow from m3/s to MG
  tmp1[, 2 : length(tmp1)] <- tmp1[, 2 : length(tmp1)] * 264.172 * 3600 / 10^6

  # Calculate hydrologic year for aggregation
  tmp1$hy <- hydro_year(tmp1$Date)

  # Annual loads in tons
  annFlw <- aggregate(tmp1[2 : (length(tmp1) - 1)], by = list(tmp1$hy),
                      FUN = sum)
  
  if (type == 'aerial') {

    # Import basin areas
    basAr <- read.csv(paste0('C:/siletz_misc/from_tmdl_drive/001_data/nlcd_2011',
                             '/siletz_HRU_summary_RMS.csv'), stringsAsFactors = F)
    
    basAr <- rowSums(basAr)
    
    for (i in 2 : length(annFlw)) {annFlw[, i] <- annFlw[, i] / basAr[i - 1]}

  }
  
  flows <- round(colMeans(annFlw[, 2 : length(annFlw)]), 2)
  
  basNms <- names(flows)
  
  if (type == 'aerial') {
    
    flows <- data.frame(Basin = basNms, flows_MG_per_haYr = flows)
    
  } else {
    
    flows <- data.frame(Basin = basNms, flows_MG_per_Yr = flows)
    
  }
  
  row.names(flows) <- 1 : nrow(flows)
  
  return(flows)

}

day_of_hydro_year <- function(hDates = NULL) {
  
  # Check to see if dates vector is a date, don't coerce, but raise exception
  if (!lubridate::is.Date(hDates) & !lubridate::is.POSIXct(hDates)) {
    
    stop('Please convert the dates using as.Date()')
    
  }
  
  pYear <- ifelse(lubridate::month(hDates) >= 10, lubridate::year(hDates) + 1,
                  lubridate::year(hDates)) - 1
  
  # Check data type -- IF is.DATE
  if (lubridate::is.Date(hDates)) {
    
    fdohy <- as.Date(paste0(pYear, '-10-01'), '%Y-%m-%d')
    
    dohy <- as.numeric(hDates) - as.numeric(fdohy) + 1
    
  }
  
  # Check data type -- IF is.POSIXct
  if (lubridate::is.POSIXct(hDates)) {
    
    fdohy <- as.POSIXct(paste0(pYear, '-10-01'), '%Y-%m-%d',
                        tz = 'America/Los_Angeles')
    
    dohy <- (as.numeric(hDates) - as.numeric(fdohy)) / 86400
    
  }
  
  return(dohy)
  
}

hydro_year <- function(hDates = NULL) {
  
  # library('')
  
  # Check to see if dates vector is a date, don't coerce, but raise exception
  if (!lubridate::is.Date(hDates) & !lubridate::is.POSIXct(hDates)) {
    
    stop('Please convert the dates using as.Date() or as.POSIXct()')
    
  }
  
  hYear <- ifelse(lubridate::month(hDates) >= 10, lubridate::year(hDates) + 1,
                  lubridate::year(hDates))
  
  return(hYear)
  
}

# ----
# annual_loads_by_HRU <- function(par = NULL, type = 'total') {
#   
#   # Import data 
#   sapply('C:/Users/rshojin/Desktop/006_scripts/github/hydroRMS/R/hydro_year.R',
#          source)
#   
#   tmp1 <- readRDS(paste0('D:/siletz/outputs/calib_20190611/latQLC_by_HRU_',
#                          par, '.RData'))
#   
#   tmp1 <- tmp1[['load']]
#   
#   # Reorder basins in the list
#   bsns <- as.numeric(substr(names(tmp1), 4, 5))
#   
#   ordr <- 1 : 17; ordr <- ordr[order(bsns)]
#   
#   tmp2 <- list()
#   
#   for (i in 1 : length(tmp1)) {tmp2 <- append(tmp2, tmp1[ordr[i]])}
#   
#   # Set up aerial loads data frame
#   loads <- tmp2[[1]][which(is.character(tmp2[[1]]$Date)), ]
#   
#   loads$Date <- as.character(loads$Date); names(loads)[1] <- 'Basin'
#   
#   dfNms <- names(loads)
#   
#   # Import basin areas
#   basAr <- read.csv(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Ox',
#                            'ygen/Middle_Siletz_River_1710020405/001_data/nlcd_',
#                            '2011/siletz_HRU_summary_RMS.csv'),
#                     stringsAsFactors = F)
#   
#   for (i in 1 : length(tmp2)) {
#     
#     # Get name of basin
#     basNme <- names(tmp2)[i]
#     
#     # Calculate hydrologic year for aggregation
#     tmp2[[i]]$hy <- hydro_year(tmp2[[i]]$Date)
#     
#     # Calculate annual loads
#     annLds <- aggregate(tmp2[[i]][2 : (length(tmp2[[i]]) - 1)],
#                         by = list(tmp2[[i]]$hy), FUN = sum)
#     
#     mnLds <- round(colMeans(annLds[, 2 : length(annLds)]), 2)
#     
#     if (type == 'aerial') {mnLds <- mnLds / basAr[i, ]} else {mnLds <- t(mnLds)}
#     
#     tmp3 <- data.frame(basNme, mnLds); names(tmp3) <- dfNms
#     
#     # Annual loads in tons
#     loads <- rbind(loads, tmp3)
#     
#   }
#   
#   return(loads)
#   
# }
# 
# annual_flows_by_HRU <- function(type = 'total') {
#   
#   # Import data 
#   sapply('C:/Users/rshojin/Desktop/006_scripts/github/hydroRMS/R/hydro_year.R',
#          source)
#   
#   tmp1 <- readRDS('D:/siletz/outputs/calib_20190611/latQLC_by_HRU_NOx.RData')
#   
#   tmp1 <- tmp1[['flow']]
#   
#   # Reorder basins in the list
#   bsns <- as.numeric(substr(names(tmp1), 4, 5))
#   
#   ordr <- 1 : 17; ordr <- ordr[order(bsns)]
#   
#   tmp2 <- list()
#   
#   for (i in 1 : length(tmp1)) {tmp2 <- append(tmp2, tmp1[ordr[i]])}
#   
#   # Set up aerial loads data frame
#   flows <- tmp2[[1]][which(is.character(tmp2[[1]]$Date)), ]
#   
#   flows$Date <- as.character(flows$Date); names(flows)[1] <- 'Basin'
#   
#   dfNms <- names(flows)
#   
#   # Import basin areas
#   basAr <- read.csv(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Ox',
#                            'ygen/Middle_Siletz_River_1710020405/001_data/nlcd_',
#                            '2011/siletz_HRU_summary_RMS.csv'),
#                     stringsAsFactors = F)
#   
#   for (i in 1 : length(tmp2)) {
#     
#     # Get name of basin
#     basNme <- names(tmp2)[i]
#     
#     # Calculate hydrologic year for aggregation
#     tmp2[[i]]$hy <- hydro_year(tmp2[[i]]$Date)
#     
#     # Calculate annual loads
#     annFlw <- aggregate(tmp2[[i]][2 : (length(tmp2[[i]]) - 1)],
#                         by = list(tmp2[[i]]$hy), FUN = sum)
#     
#     mnFlw <- round(colMeans(annFlw[, 2 : length(annFlw)]), 2)
#     
#     if (type == 'aerial') {mnFlw <- mnFlw / basAr[i, ]} else {mnFlw <- t(mnFlw)}
#     
#     tmp3 <- data.frame(basNme, mnFlw); names(tmp3) <- dfNms
#     
#     # Annual flows in MG/yr or MG/ha/yr
#     flows <- rbind(flows, tmp3)
#     
#   }
#   
#   return(flows)
#   
# }
