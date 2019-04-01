calib_emcdwc <- function(pars, stns, strD, endD) { 

  # LIBRARIES, OPTIONS AND FUNCTIONS ----
  options(warn = -1)
  
  library('reshape2')
  library('ggplot2')
  library('lubridate')
  
  sapply(c('C:/siletz/scripts/R/runoff_components.R',
           'C:/Users/rshojin/Desktop/001_scripts/R/General/hydro_year.R',
           'C:/Users/rshojin/Desktop/001_scripts/R/General/day_of_hydro_year.R',
           'C:/siletz/scripts/R/ro_comp_analysis.R',
           'C:/siletz/scripts/R/get_sparrow_loads.R'), source)
  
  plain <- function(x) {format(x, scientific = FALSE, trim = TRUE)}
  
  # LOAD WQ OBS DATA ----
  rchQLC <- readRDS('C:/siletz/calib/wq/rchQLC.RData') # Model flows/loads/conc
  
  qRO <- runoff_components(strD = strD, endD = endD, wqDir = 'C:/siletz/calib/wq',
                           emcFil = 'C:/siletz/emcdwc.csv')

  wqDt <- read.csv(paste0('C:/siletz/calib/wq/', pars, '_', stns, '.csv'),
                   stringsAsFactors = FALSE)
  
  wqDt$Date <- as.Date(wqDt$Date, '%Y-%m-%d')
  
  # PROCESS MODEL DATA ----
  basin <- 'Bas14'
  
  col <- which(names(rchQLC$reach_flows) == basin)
  
  # AGGREGATE MODEL DATA TO DAILY ----
  aggFun <- c('mean', 'sum', 'mean')
  
  dlyQLC <- list()
  
  for (i in 1 : length(rchQLC)) {
  
    dlyQLC[[i]] <- rchQLC[[i]][, c(1, col)]
    
    dlyQLC[[i]]$Date2 <- as.Date(dlyQLC[[i]]$Date, '%Y-%m-%d %H:%M:%S',
                                 tz = 'America/Los_Angeles')
      
    dlyQLC[[i]] <- aggregate(dlyQLC[[i]][, 2],
                             by = list(dlyQLC[[i]]$Date2),
                             FUN = aggFun[i])
    
    names(dlyQLC)[i] <- names(rchQLC)[i]
    
    names(dlyQLC[[i]]) <- c('Date', basin)
  
  }
  
  # Extract flows/loads/concentrations
  datM <- data.frame(dlyQLC[['reach_flows']],
                     LM = dlyQLC[['reach_loads']]$Bas14,
                     CM = dlyQLC[['reach_conc']]$Bas14)
  
  names(datM)[2] <- 'QM'
  
  datM$LM <- datM$LM / 1000 # Convert from kg to tons (metric)
  
  datM$QM <- datM$QM * 35.314666213 # Convert flows to cfs
  
  # Merge model and observation data
  datM <- merge(datM, wqDt, by.x = 'Date', by.y = 'Date', all.x = TRUE)
  
  names(datM)[5 : 7] = c('CO', 'QO', 'LO')
  
  datM <- datM[, c(1, 2, 6, 3, 7, 4, 5)]
  
  # Calculate hydrologic year and day of hydrologic year
  datM$dohy <- day_of_hydro_year(datM$Date)
  
  datM$hy <- hydro_year(datM$Date)
  
  datM <- datM[-nrow(datM), ]
  
  # ADD RUNOFF COMPONENTS ----
  roCmp <- ro_comp_analysis(qRO)
  
  roCmp <- roCmp[-nrow(roCmp), ]
  
  # AGWO, IFWO and SURO thresholds based on giving ~equal distribution in each
  roCmp$ROC <- ifelse(roCmp$SURO >= 0.002, 'SURO', ifelse(roCmp$AGWO >= 0.98,
                                                          'AGWO', 'IFWO'))
  
  tmp <- roCmp[, c(1, 5)]
  
  datM <- merge(datM, tmp, by.x = 'Date', by.y = 'Date', all.x = TRUE,
                all.y = FALSE)
  
  # CALIBTRATION COMPONENTS ----
  # Add a seasons column 
  datM$Mn <- month(datM$Date)
  
  mnth <- data.frame(Mnth = c(1 : 12),
                     Seas = c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3)))
  
  datM <- merge(datM, mnth, by.x = 'Mn', by.y = 'Mnth', all.x = T, all.y = T)
  
  # Intialize calibration data frame
  cal <- data.frame(Measure = c('Total load error', 'Winter load error', 
                                'Spring load error', 'Summer load error',
                                'Autumn load error', 'Export rate error',
                                'AGWO conc error', 'IFWO conc error',
                                'SURO conc error'),
                    ObsVal = rep(0, 9), ModVal = rep(0, 9), ERROR = rep(0, 9))
  
  # ______________________________________________________________________________
  # Total selected loads comparison
  datMCC <- datM[complete.cases(datM), ]
  
  cal[1, 2 : 4] <- c(round(sum(datMCC$LM), 1), round(sum(datMCC$LO), 1),
                     round(100 * (sum(datMCC$LO) - sum(datMCC$LM)) / 
                           sum(datMCC$LO), 2))
  
  # ______________________________________________________________________________
  # Seasonal selected loads comparison
  seaLds <- aggregate(datMCC[, 5 : 6], by = list(datMCC$Seas), FUN = 'sum',
                      na.rm = T)
  
  cal[2 : 5, 2 : 3] <- seaLds[, 2 : 3]
  
  cal[2 : 5, 4] <- round((100 * (seaLds$LO - seaLds$LM) / seaLds$LO), 2)
  
  # ______________________________________________________________________________
  # Annual export rates for HRUs compared to SPARROW data
  comid <- data.frame(CMID = c(23880860, 23880884),
                      DESC = c('Siletz at Euchre', 'Siletz at Sam'),
                      USAR = c(597.09, 504.61))
  
  sprLd <- get_sparrow_loads(comid$CMID, 'TSS')
  
  ojaAr <- 583.13
  
  # Loads in kg/ha/yr
  cal[6, 2] <- (sprLd[2, 2] + (sprLd[1, 2] - sprLd[2, 2]) *
                              (ojaAr - comid[2, 3]) / 
                              (comid[1, 3] - comid[2, 3])) / ojaAr / 100 
  
  modLd <- aggregate(datM$LM, by = list(datM$hy), FUN = 'sum')
  
  cal[6, 3] <- sum(modLd$x) / nrow(modLd) * 1000 / ojaAr / 100
  
  cal[6, 4] <- round(100 * ((cal[6, 3] - cal[6, 2]) / cal[6, 2]), 2)
  
  # ______________________________________________________________________________
  # Partitioned mean concentrations (SURO/INFW/AGWO)
  rocCon <- aggregate(datMCC[, 7 : 8], by = list(datMCC$ROC), FUN = 'mean')
  
  rocCon <- rocCon[, c(1, 3, 2)]
  
  rocCon$Err <- round(100 * ((rocCon$CM - rocCon$CO) / rocCon$CO), 2)
  
  cal[7 : 9, 2 : 4] <- rocCon[, 2 : 4]
  
  cal[, 2 : 3] <- round(cal[, 2 : 3], 2)
  
  # PLOT DATA ----
  pltL <- ggplot(data = datM, aes(x = dohy)) +
    geom_line(aes(y = LM), size = 0.5) +
    geom_point(aes(y = LM, color = ROC), size = 1.15) +
    geom_point(aes(y = LO), size = 2, shape = 23, color = 'darkred',
               stroke = 1.4, fill = 'yellow') +
    scale_y_log10(labels = plain) + 
    facet_wrap(~hy, ncol = 3)
  
  ggsave(paste0(pars, '_loads.png'), plot = pltL, path = 'C:/siletz/calib/wq/plots',
         width = 15, height = 10, units = 'in', dpi = 300)
  
  pltC <- ggplot(data = datM, aes(x = dohy)) +
    geom_line(aes(y = CM), size = 0.5) +
    geom_point(aes(y = CM, color = ROC), size = 1.15) +
    geom_point(aes(y = CO), size = 2, shape = 23, color = 'darkred',
               stroke = 1.4, fill = 'yellow') +
    scale_y_log10(labels = plain) + 
    facet_wrap(~hy, ncol = 3)
  
  ggsave(paste0(pars, '_concs.png'), plot = pltC, path = 'C:/siletz/calib/wq/plots',
         width = 15, height = 10, units = 'in', dpi = 300)
  
  return(cal)

}