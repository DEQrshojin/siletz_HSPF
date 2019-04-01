wq_data_4_hspf <- function(strD, endD, pars, stns) { 
  
  # LIBRARIES AND OPTIONS ----
  library(ggplot2)
  library(reshape2)
  
  options(scipen = -1, warn = -1, stringsAsFactors = FALSE)
  
  # LOAD DATA ----
  wqAll <- readRDS(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxy',
                          'gen/Middle_Siletz_River_1710020405/001_data/wq_data',
                          '/awqms_WQ_data.Rdata'))
  
  # PROCESS DATA ----
  wqDt <- wqAll[[1]]
  
  # Remove dql = E
  wqDt <- wqDt[which(wqDt$dql != 'DQL=E'), ] 
  
  # Set ND to half the reported value
  wqDt$vnd <- ifelse(wqDt$opr == '<', wqDt$val / 2, wqDt$val)
  
  # Trim to dates, stations and parameters
  wqDt <- wqDt[wqDt$new %in% pars, ]
  
  pDates <- as.POSIXct(c(strD, endD), '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  wqDt <- wqDt[which(wqDt$dt >= pDates[1] & wqDt$dt <= pDates[2]), ]
  
  wqDt <- wqDt[wqDt$stn %in% stns, ]
  
  # Create a column of dates
  wqDt$date <- as.Date(wqDt$dt, '%Y-%m-%d %H:%M:%S', tz = 'America/Los_Angeles')
  
  # Average dates with multiple points
  wqDt <- wqDt[, c(17 : 16)]
  
  wqDt <- aggregate(wqDt$vnd, FUN = 'mean', by = list(wqDt$date))
  
  names(wqDt) <- c('Date', 'C_mgL')
  
  # Load flows
  qSlz <- read.csv('C:/siletz/calib/gge_wq.csv')
  
  qSlz$Date <- as.Date(qSlz$Date, '%Y-%m-%d')
  
  qSlz <- qSlz[, -ncol(qSlz)]
  
  # Calculate loads
  wqDt <- merge(wqDt, qSlz, by.x = 'Date', by.y = 'Date', all.x = TRUE,
                all.y = FALSE)
  
  wqDt$Load <- wqDt$qSlz * wqDt$C_mgL * 0.028316847 * 1000 * 86400 * 10^-9
  
  return(wqDt)
  
}