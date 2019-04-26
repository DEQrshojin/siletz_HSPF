
#_______________________________________________________________________________
calib_wq <- function(pars, stns, strD, endD, n) { 
  
  # Synopsis ----
  
  
  
  # LIBRARIES, OPTIONS AND FUNCTIONS ----
  options(warn = -1)
  
  suppressMessages(library('reshape2'))
  suppressMessages(library('ggplot2'))
  suppressMessages(library('lubridate'))
  suppressMessages(library('dplyr'))
  
  sapply(c('C:/Users/rshojin/Desktop/006_scripts/github/General/hydro_year.R',
           'C:/Users/rshojin/Desktop/006_scripts/github/General/day_of_hydro_year.R'),
         source)
  
  plain <- function(x) {format(x, scientific = FALSE, trim = TRUE)}
  
  # LOAD WQ OBS DATA ----
  rchQLC <- readRDS('D:/siletz/calib/wq/rchQLC.RData') # Model flows/loads/conc
  
  qRO <- runoff_components(strD = strD, endD = endD, wqDir = 'D:/siletz/calib/wq',
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
  cal <- data.frame(a = c('winld01',  'sprld01', 'sumld01', 'autld01', 'ttlld01',
                          'agwcn01', 'ifwcn01', 'surcn01', 'meanc01', 'loadreg',
                          'concreg'),
                    ObsVal = rep(0, 11), b = rep(0, 11), ERROR = rep(0, 11))
  
  # ____________________________________________________________________________
  # Seasonal selected loads comparison
  datMCC <- datM[complete.cases(datM), ]
  
  seaLds <- aggregate(datMCC[, c(6, 5)], by = list(datMCC$Seas), FUN = 'sum',
                      na.rm = T)
  
  cal[1 : 4, 2 : 3] <- seaLds[, 2 : 3]
  
  cal[1 : 4, 4] <- round((100 * (seaLds$LM - seaLds$LO) / seaLds$LO), 2)
  
  # Total (all seasons combined) loads
  cal[5, 2 : 3] <- colSums(datMCC[, c(6, 5)])
  
  cal[5, 4] <- round((100 * (cal[5, 3] - cal[5, 2]) / cal[5, 2]), 2)
  
  # ____________________________________________________________________________
  # Partitioned mean concentrations (SURO/INFW/AGWO)
  rocCon <- aggregate(datMCC[, 7 : 8], by = list(datMCC$ROC), FUN = 'mean')
  
  rocCon <- rocCon[, c(1, 3, 2)]
  
  rocCon$Err <- round(100 * ((rocCon$CM - rocCon$CO) / rocCon$CO), 2)
  
  cal[6 : 8, 2 : 4] <- rocCon[, 2 : 4]
  
  # Mean annual concentrations
  cal[9, 2 : 3] <- colMeans(datMCC[, c(8, 7)])
  
  cal[9, 4] <- round((100 * (cal[9, 3] - cal[9, 2]) / cal[9, 2]), 2)
  
  # PLOT DATA ----
  pltL <- ggplot(data = datM, aes(x = dohy)) + theme_bw() +
    geom_line(aes(y = LM), size = 0.5, color = 'darkblue') +
    geom_point(aes(y = LO), size = 1.2, shape = 23, color = 'darkred',
               stroke = 1.0, fill = 'yellow') +
    xlab('Day of the Year (Oct 1 to Sep 30)') + ylab('Loads (ton/day)') +
    # scale_y_continuous(labels = plain) + facet_wrap(~hy, ncol = 4)
    scale_y_log10(labels = plain) + facet_wrap(~hy, ncol = 4)
  
  ggsave(paste0(pars, '_loads_ts_', n, '.png'), plot = pltL, width = 10,
         height = 7.5,  path = 'D:/siletz/calib/wq/plots', units = 'in',
         dpi = 300)
  
  pltC <- ggplot(data = datM, aes(x = dohy)) + theme_bw() +
    geom_line(aes(y = CM), size = 0.5, color = 'darkblue') +
    geom_point(aes(y = CO), size = 1.2, shape = 23, color = 'darkred',
               stroke = 1.0, fill = 'yellow') +
    xlab('Day of the Year (Oct 1 to Sep 30)') + ylab('Concentration (mg/L)') +
    # scale_y_continuous(labels = plain) + facet_wrap(~hy, ncol = 4)
    scale_y_log10(labels = plain) + facet_wrap(~hy, ncol = 4)
  
  ggsave(paste0(pars, '_concs_ts_', n, '.png'), plot = pltC, width = 10, 
         height = 7.5, path = 'D:/siletz/calib/wq/plots', units = 'in',
         dpi = 300)
  
  corr <- qlc_corr(pars = pars, datM = datM, n = n)
  
  cal[10, 2 : 4] <- corr$Loads; cal[11, 2 : 4] <- corr$Concs
  
  return(cal)
  
}

#_______________________________________________________________________________
qlc_corr <- function(pars, datM, n) {
  
  # Synopsis ----
  
  
  
  suppressMessages(library('ggplot2'))
  suppressMessages(library('stats'))
  suppressMessages(library('dplyr'))
  
  plain <- function(x) {format(x, scientific = FALSE, trim = TRUE)}
  
  # ALL DATA ----
  plt <- ggplot(data = datM) +
    geom_point(aes(x = QM, y = LM), size = 0.25, color = 'darkblue') +
    geom_point(aes(x = QO, y = LO), size = 1.2, shape = 23, color = 'darkred',
               stroke = 1.0, fill = 'yellow') +
    xlab('Flow (cfs)') + ylab('Load (ton/day)') + 
    scale_x_log10(labels = plain, limits = c(30, 10000)) +
    scale_y_log10(labels = plain) + theme_bw()
  # scale_y_continuous(labels = plain) + theme_bw() limits = c(0.001, 0.2000)
  
  ggsave(paste0(pars, '_flow_v_load_', n, '.png'), plot = plt, width = 12,
         height = 6, path = 'D:/siletz/calib/wq/plots', units = 'in', dpi = 300)
  
  plt <- ggplot(data = datM) +
    geom_point(aes(x = QM, y = CM), size = 0.25, color = 'darkblue') +
    geom_point(aes(x = QO, y = CO), size = 1.2, shape = 23, color = 'darkred',
               stroke = 1.0, fill = 'yellow') +
    xlab('Flow (cfs)') + ylab('Concentration (mg/L)') + 
    scale_x_log10(labels = plain, limits = c(30, 10000)) +
    scale_y_log10(labels = plain) + theme_bw()
  # scale_y_continuous(labels = plain) + theme_bw() limits = c(0.003, 1.20)
  
  ggsave(paste0(pars, '_flow_v_conc_', n, '.png'), plot = plt, width = 12,
         height = 6, path = 'D:/siletz/calib/wq/plots', units = 'in', dpi = 300)
  
  # PAIRED DATA ----
  datMCC <- datM[complete.cases(datM), 3 : 8]
  
  # Linear regressions
  fL <- summary(lm(LM ~ LO, data = datMCC)); fC <- summary(lm(CM ~ CO, data = datMCC))
  
  # Paired load/conc, Adj R2, median residual
  qlcCorr <- list(Loads = c(fL$coefficients[2, 1], fL$adj.r.squared, median(fL$residuals)),
                  Concs = c(fC$coefficients[2, 1], fC$adj.r.squared, median(fC$residuals)))
  
  maxL <- 10^floor(log10(max(max(datMCC$LO), max(datMCC$LM))))
  
  maxC <- 10^ceiling(log10(max(max(datMCC$CO), max(datMCC$CM))))
  
  L121 <- data.frame(x = c(0, maxL), y = c(0, maxL))
  
  C121 <- data.frame(x = c(0, 5.0), y = c(0, 5.0))
  
  plt <- ggplot(data = datMCC) +
         geom_point(aes(x = LO, y = LM), size = 1.2, shape = 23, color = 'darkred',
                    stroke = 1.0, fill = 'yellow') +
         geom_line(data = L121, aes(x = x, y = y)) + 
         xlab('Observed Loads (tons/day)') + ylab('Modeled Loads (ton/day)') + 
         scale_x_log10(labels = plain, limits = c(0.01, 10.0)) +
         scale_y_log10(labels = plain, limits = c(0.01, 10.0)) + theme_bw()
         # scale_y_continuous(labels = plain) + theme_bw()
  
  ggsave(paste0(pars, '_obsmod_loads_', n, '.png'), plot = plt, width = 6,
         height = 6, path = 'D:/siletz/calib/wq/plots', units = 'in', dpi = 300)
  
  plt <- ggplot(data = datMCC) +
         geom_point(aes(x = CO, y = CM), size = 1.2, shape = 23, color = 'darkred',
                    stroke = 1.0, fill = 'yellow') +
         geom_line(data = C121, aes(x = x, y = y)) + 
         xlab('Observed Concentrations (mg/L)') + ylab('Modeled Concentrations (mg/L)') + 
         scale_x_log10(labels = plain, limits = c(0.05, 5)) +
         scale_y_log10(labels = plain, limits = c(0.05, 5)) + theme_bw()
         # scale_y_continuous(labels = plain) + theme_bw()
  
  ggsave(paste0(pars, '_obsmod_concs_', n, '.png'), plot = plt, width = 6,
         height = 6, path = 'D:/siletz/calib/wq/plots', units = 'in', dpi = 300)
  
  return(qlcCorr)
  
}

#_______________________________________________________________________________
check_2017_wq <- function(par, n) {
  
  # Synopsis ----
  
  
  
  # Use upper case for PAR where needed (i.e., TP, NOx, PO4)
  
  library(dplyr); library(reshape2); library(ggplot2)
  
  # IMPORT AMBIENT WATER QUALITY DATA ----
  wqAll <- readRDS(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxy',
                          'gen/Middle_Siletz_River_1710020405/001_data/wq_data',
                          '/awqms_WQ_data.Rdata'))
  
  dates <- seq(as.Date('2017-07-01', '%Y-%m-%d'),
               as.Date('2017-10-01', '%Y-%m-%d'), 1)
  
  # JUST WORRY ABOUT NITRATE, TP and PO4 for the moment
  wqDt <- wqAll[[1]] %>% 
          mutate(date = as.Date(dt), vnd = ifelse(opr == '<', val / 2, val)) %>%
          filter(dql %in% c('DQL=A', 'DQL=B'), date %in% dates)
  
  wqDt <- wqDt[, c(16, 3, 5, 17, 9, 2)]
  
  wqDt <- wqDt[which(wqDt$new %in% par & wqDt$date %in% dates), ]
  
  wqLC <- read.csv(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxy',
                          'gen/Middle_Siletz_River_1710020405/001_data/wq_data',
                          '/Monitoring 2017/LSWCD/CCAL_SILETZ_092017_092617_RMS.csv'),
                   stringsAsFactors = F)
  
  wqLC$date <- as.Date(wqLC$date, '%Y-%m-%d')
  
  wqLC$dt <- as.POSIXct(wqLC$dt, '%Y-%m-%d %H:%M', tz = 'America/Los_Angeles')
  
  wqLC <- wqLC[wqLC$new %in% par, ]
  
  wqDt$src <- 'ORDEQ'
  
  if (nrow(wqLC) !=0) {wqLC$src <- 'LSWCD'; wqDt <- rbind(wqDt, wqLC)}
  
  mtch <- data.frame(site = c('10391-ORDEQ', '37396-ORDEQ', '38918-ORDEQ',
                              '11246-ORDEQ', '29287-ORDEQ', '38919-ORDEQ',
                              '38930-ORDEQ', '36367-ORDEQ', '38928-ORDEQ', '38929-ORDEQ'),
                     basn = c(14, 6, 11, 7, 13, 13, 10, 15, 8, 10),
                     cols = c(14, 6, 11, 7, 13, 13, 10, 15, 8, 10) + 1,
                     stringsAsFactors = FALSE)
  
  mdDt <- readRDS(paste0('D:/siletz/calib/wq/rchQLC.RData'))
  
  mdDt <- mdDt[['reach_conc']] %>% mutate(Date2 = as.Date(Date)) %>%
          filter(Date2 %in% dates)
  
  mdDt <- aggregate(mdDt[2 : (length(mdDt) - 1)], by = list(mdDt$Date2), FUN = mean)
  
  mdDt <- mdDt[, c(1, mtch$cols)]
  
  names(mdDt) <- c('Date', mtch$site)
  
  mdDt <- melt(mdDt, id.vars = 'Date', value.name = 'C_mgL', variable.name = 'stn')
  
  plt <- ggplot(mdDt, aes(x = Date, y = C_mgL)) +
         geom_line(color = 'darkblue', size = 0.5) +
         geom_point(data = wqDt, aes(x = date, y = vnd), size = 1.2, shape = 23,
                    color = 'darkred', stroke = 1.0, fill = 'yellow') +
         facet_wrap(~stn, ncol = 4)
  
  ggsave(paste0(par, '_2017_conc_ts_', n, '.png'), plot = plt, dpi = 300, units = "in",
         path = 'D:/siletz/calib/wq/plots', width = 10, height = 7.5)
  
}
