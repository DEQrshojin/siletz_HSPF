calib_wq_pest <- function(pars, stns, strD, endD, n) { 

  # LIBRARIES, OPTIONS AND FUNCTIONS ----
  options(warn = -1)
  
  suppressMessages(library('reshape2'))
  suppressMessages(library('ggplot2'))
  suppressMessages(library('lubridate'))
  suppressMessages(library('dplyr'))
  
  sapply(c('D:/siletz/scripts/R/runoff_components.R',
           'C:/Users/rshojin/Desktop/006_scripts/github/General/hydro_year.R',
           'C:/Users/rshojin/Desktop/006_scripts/github/General/day_of_hydro_year.R',
           'D:/siletz/scripts/R/ro_comp_analysis.R',
           'D:/siletz/scripts/R/flow_load_corr.R'), source)
  
  plain <- function(x) {format(x, scientific = FALSE, trim = TRUE)}
  
  # LOAD WQ OBS DATA ----
  rchQLC <- readRDS('D:/siletz/calib/wq/rchQLC.RData') # Model flows/loads/conc
  
  qRO <- runoff_components(strD = strD, endD = endD,
                           wqDir = 'D:/siletz/calib/wq',
                           emcFil = paste0('D:/siletz/emcdwc_', pars, '.csv'))

  wqDt <- read.csv(paste0('D:/siletz/calib/wq/', pars, '_', stns, '.csv'),
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
  cal <- data.frame(a = c('winld01',  'sprld01', 'sumld01', 'autld01',
                          'surcn01', 'ifwcn01', 'agwcn01'),
                    ObsVal = rep(0, 7), b = rep(0, 7), ERROR = rep(0, 7))
  
  # ____________________________________________________________________________
  # Seasonal selected loads comparison
  datMCC <- datM[complete.cases(datM), ]
  
  seaLds <- aggregate(datMCC[, 5 : 6], by = list(datMCC$Seas), FUN = 'sum',
                      na.rm = T)
  
  cal[1 : 4, 2 : 3] <- seaLds[, 2 : 3]
  
  cal[1 : 4, 4] <- round((100 * (seaLds$LO - seaLds$LM) / seaLds$LO), 2)
  
  # ____________________________________________________________________________
  # Partitioned mean concentrations (SURO/INFW/AGWO)
  rocCon <- aggregate(datMCC[, 7 : 8], by = list(datMCC$ROC), FUN = 'mean')
  
  rocCon <- rocCon[, c(1, 3, 2)]
  
  rocCon$Err <- round(100 * ((rocCon$CM - rocCon$CO) / rocCon$CO), 2)
  
  cal[5 : 7, 2 : 4] <- rocCon[, 2 : 4]

  # CREATE MODEL.OUT FILE ----
  # Create data frame for output
  datOut <- datMCC[order(datMCC$Date), c(2, 7, 5)]
  
  datOut$t1 <- paste0('dconc',
                      as.character(ifelse((year(datOut$Date) - 2000) < 10, '0', '')),
                      year(datOut$Date) - 2000,
                      as.character(ifelse(month(datOut$Date) < 10, '0', '')),
                      month(datOut$Date),
                      as.character(ifelse(day(datOut$Date) < 10, '0', '')),
                      day(datOut$Date))
  
  datOut$t2 <- gsub('dconc', 'dload', datOut$t1)
  
  names(datOut) <- c('Date', 'b', 'b', 'a', 'a')
  
  datOut <- rbind(datOut[, c(4, 2)], datOut[, c(5, 3)], cal[, c(1, 3)])
  
  # Format for output
  datOut$length <- 20 - sapply(datOut$a, nchar) - 1
  
  datOut$length <- paste0('%', datOut$length, 's')
  
  datOut$pre <- sapply(datOut$length, sprintf, ' ')
  
  datOut$prelen <- sapply(datOut$pre, nchar)
  
  datOut$line <- paste0(' ', datOut$a, datOut$pre, datOut$b)

  write.table(datOut$line, 'D:/siletz/model_nox.out', row.names = FALSE,
              col.names = FALSE, quote = FALSE)

  # PLOT DATA ----
  pltL <- ggplot(data = datM, aes(x = dohy)) +
    geom_line(aes(y = LM), size = 0.5, color = 'darkblue') +
    geom_point(aes(y = LO), size = 1.2, shape = 23, color = 'darkred',
               stroke = 1.0, fill = 'yellow') +
    xlab('Day of the Year (Oct 1 to Sep 30)') + ylab('Load (ton/day)') +
    scale_y_log10(labels = plain) + facet_wrap(~hy, ncol = 4)
  
  ggsave(paste0(pars, '_loads_ts_', n, '.png'), plot = pltL,
         path = 'D:/siletz/calib/wq/plots',
         width = 10, height = 7.5, units = 'in', dpi = 300)
  
  pltC <- ggplot(data = datM, aes(x = dohy)) +
    geom_line(aes(y = CM), size = 0.5, color = 'darkblue') +
    geom_point(aes(y = CO), size = 1.2, shape = 23, color = 'darkred',
               stroke = 1.0, fill = 'yellow') +
    xlab('Day of the Year (Oct 1 to Sep 30)') + ylab('Concentration (mg/L)') +
    scale_y_log10(labels = plain) + facet_wrap(~hy, ncol = 4)
  
  ggsave(paste0(pars, '_concs_ts_', n, '.png'), plot = pltC,
         path = 'D:/siletz/calib/wq/plots',
         width = 10, height = 7.5, units = 'in', dpi = 300)
  
  flow_load_corr(pars = pars, datM = datM, n = n)

  return(cal)

}
