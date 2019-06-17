#! D:/Program Files/R/R-3.5.2/bin Rscript

# RUN CALIBRATION STATISTICS AND GRAPHS FOR GIVEN MODEL RUN
# Ryan Shojinaga, Water Quality Analyst, NRS3, Oregon DEQ
# shojinaga.ryan@deq.state.or.us, 503-229-5777

for (i in 1) {
  
  # LIBRARIES ----
  suppressMessages(library(hydroGOF))
  suppressMessages(library(ggplot2))
  suppressMessages(library(scales))
  suppressMessages(library(lubridate))
  suppressMessages(library(stats))
  suppressMessages(library(reshape2))
  
  options(warn = -1)

  # Source the functions
  source('D:/siletz/scripts/R/hydrology_calib_functions.R')
  
  # FILE PATHS ----
  filPath <- 'D:/siletz/'
  
  datPath <- paste0(filPath, 'calib')
  
  pltPath <- paste0(datPath, '/plots')

  # CALIBRATION INPUTS ----
  # Dates
  strCal <- '2004-10-01'; endCal <- '2017-09-30'
  
  strCal <- as.POSIXct(strCal, format = '%Y-%m-%d')
  
  endCal <- as.POSIXct(endCal, format = '%Y-%m-%d')
  
  ts <- data.frame('Date' = seq(strCal, endCal, 3600))
  
  # Read the counter
  countFil = file(paste0(filPath, 'count.txt'))
  
  n = as.numeric(readLines(countFil))  
  
  # LOAD, MERGE, AND PROCESS DATA ----
  # qData <- read.csv('D:/siletz/calib/siletz_out_sed.csv',
  #                   stringsAsFactors = FALSE)
  # 
  # qData <- qData[['reach_flows']][, c(1, 12, 4)]
  
  qData <- readRDS("D:/siletz/calib/wq/rchQLC.RData")
  
  qData <- qData[['reach_flows']][, c(1, 12, 5)]
  
  # Convert m3/s to cfs
  qData[, 2 : 3] <- qData[, 2 : 3] * 35.314666721
  
  qGage <- read.csv('D:/siletz/calib/gge.csv', stringsAsFactors = FALSE)
  
  qData$Date <- as.POSIXct(qData$Date, '%Y-%m-%d %H:%M:%S',
                           tz = 'America/Los_Angeles')
  
  qGage$Date <- as.Date(qGage$Date, '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  calDat <- merge(ts, qData, by.x = 'Date', by.y = 'Date', all.x = TRUE,
                  all.y = FALSE)
  
  names(calDat) <- c('Datetime', 'qSlz_M', 'qSun_M')
  
  calDat$YR <- year(calDat$Date)
  
  calDat$MN <- month(calDat$Date)
  
  calDat$Date <- as.Date(calDat$Datetime, '%Y-%m-%d %H:%M:%S')
  
  calDat$DOY <- yday(calDat$Date)
  
  calDat$Mndt <- as.Date(paste0(calDat$YR, '-',
                                ifelse(calDat$MN < 10, 0, ''), calDat$MN,
                                '-01'), '%Y-%m-%d')
  
  calDat <- calDat[complete.cases(calDat$Datetime), ]
  
  # MEAN DAILY FLOWS ----
  datDly <- aggregate(calDat[, 2 : 3], by = list(calDat$Date), mean,
                      na.rm = TRUE)
  
  colnames(datDly)[1] <- 'Date'
  
  datDly <- merge(datDly, qGage, by.x = 'Date', by.y = 'Date',
                  all.x = TRUE, all.y = FALSE)
  
  names(datDly) <- c('Date', 'qSlz_M', 'qSun_M', 'qSlz_G', 'qSun_G')
  
  datDly <- datDly[, c(1, 4, 2, 5, 3)]
  
  datDly[, 2 : 5] <- round(datDly[, 2 : 5], 1)
  
  datDly$YR <- year(datDly$Date)
  
  datDly$MO <- month(datDly$Date)
  
  pAud <- data.frame('p1' = as.Date(c('2007-11-25', '2009-08-19'), '%Y-%m-%d'),
                     'p2' = as.Date(c('2015-08-31', '2016-06-07'),'%Y-%m-%d'),
                     stringsAsFactors = FALSE)
  
  datDly[which(datDly[, 1] > pAud[1, 1] & datDly[, 1] < pAud[2, 1]), 4] <- NA
  
  datDly[which(datDly[, 1] > pAud[1, 2] & datDly[, 1] < pAud[2, 2]), 4] <- NA
  
  datDly[is.na(datDly$qSlz_G), 3] <- NA
  
  datDly[is.na(datDly$qSun_G), 5] <- NA
  
  # Hydrologic year
  datDly$HY <- ifelse(datDly$MO >= 10, datDly$YR + 1, datDly$YR)
  
  datDly$HDOY <- 0
  
  # Day of year beginning at the start of the hydro year (Oct)
  for (i in 1 : nrow(datDly)) {
    
    if ((day(datDly[i, 1]) == 1) & (datDly[i, 7] == 10)) {
      
      datDly[i, 9] = 1
      
    } else {
      
      datDly[i, 9] = datDly[i - 1, 9] + 1
      
    }
  }
  
  datDly$Mndt <- as.Date(paste0(datDly$YR, '-',
                                ifelse(datDly$MO < 10, 0, ''),
                                datDly$MO, '-01'), '%Y-%m-%d')
  
  datDly$qSun_M <- ifelse(datDly$qSun_M == 0, 1e-6, datDly$qSun_M)
  
  # MEAN MONTHLY VOLUMES ----
  datMnt <- aggregate(datDly[, 2 : 5], by = list(datDly$Mndt), mean,
                      na.rm = TRUE)
  
  colnames(datMnt)[1] <- 'Date'
  
  dlyPrior2 <- as.Date('2014-10-01', '%Y-%m-%d')
  
  # Set the zeros in Sunshine Gage data to NA
  datMnt[is.nan(datMnt[, 4]), 4 : 5] <- NA
  
  # Convert to 1000 x AF (1 cfs = 1.98347 AF/day)
  datMnt[, 2 : 5] <- datMnt[, 2 : 5] * 1.98347 / 1000
  
  # Sunshine gage data with only daily data, multiply the volume by 24 (hours)
  datMnt$YR <- year(datMnt$Date)
  
  datMnt$MN <- month(datMnt$Date)
  
  datMnt$HY <- ifelse(datMnt$MN < 10, datMnt$YR, datMnt$YR + 1)
  
  # QUANTILES ----
  pct <- c(0.10, 0.25, 0.50, 0.75, 0.90)
  
  qntSlz <- quantile(datDly$qSlz_G, pct, na.rm = TRUE)
  
  qntSun <- quantile(datDly$qSun_G, pct, na.rm = TRUE)
  
  pcNm <- names(qntSun)
  
  # PEAK STORM FLOWS ---- 
  qStm <- read.csv('D:/siletz/pest/stmObs.csv', stringsAsFactors = FALSE)
  
  qStm$Date = as.Date(paste0(20, substr(qStm$name, 7, 12)), '%Y%m%d')
  
  qStm <- qStm[, c(5, 4, 2)]
  
  # qStmSlz <- merge
  qStmSlz <- merge(qStm[qStm$group == 'qstmsz', ], datDly, by.x = 'Date',
                   by.y = 'Date', all.x = TRUE, all.y = FALSE)

  qStmSun <- merge(qStm[qStm$group == 'qstmsn', ], datDly, by.x = 'Date',
                   by.y = 'Date', all.x = TRUE, all.y = FALSE)

  slzDat <- melt(datDly, id.vars = c('Date', 'HY', 'HDOY'),
                 measure.vars = c('qSlz_G', 'qSlz_M', 'qSun_G', 'qSun_M'),
                 value.name = 'flow_cfs', variable.name = 'srce')
  
  # PLOT TIME SERIES ----
  slzPlot = ggplot(data = slzDat[which(slzDat$srce == 'qSlz_G' | slzDat$srce == 'qSlz_M'), ],
                   aes(x = HDOY, y = flow_cfs, color = srce)) +
            geom_line(size = 0.6) + facet_wrap(~ HY, ncol = 4) + 
            scale_color_manual(values = c('darkblue', 'darkred'),
                               labels = c('Gage data', 'Model data')) +
            scale_y_log10(limits = c(50, 50000), labels = comma) +
            scale_x_continuous(breaks = c(15, 46, 76, 107, 138, 166, 197, 227,
                                          258, 288, 319, 350),
                               labels = c('15' = 'O', '46' = 'N', '76' = 'D',
                                          '107' = 'J', '138' = 'F', '166' = 'M',
                                          '197' = 'A', '227' = 'M', '258' = 'J',
                                          '288' = 'J', '319' = 'A', '350' = 'S')) +
            ylab("Flow (cfs)") +
            geom_hline(yintercept = qntSlz[1], size = 0.4, linetype = 2) +
            geom_hline(yintercept = qntSlz[3], size = 0.4, linetype = 2) +
            geom_hline(yintercept = qntSlz[5], size = 0.4, linetype = 2) +
            annotate("text", 330, qntSlz[1], label = pcNm[1], vjust = 0, size = 3.0) +
            annotate("text", 330, qntSlz[3], label = pcNm[3], vjust = 0, size = 3.0) +
            annotate("text", 330, qntSlz[5], label = pcNm[5], vjust = 0, size = 3.0) +
            theme_bw() + theme(legend.position = c(0.75, 0.10),
                               axis.title.x = element_blank()) +
            guides(color = guide_legend(title = 'Flow data source'))
  
  sunPlot = ggplot(data = slzDat[which(slzDat$srce == 'qSun_G' | slzDat$srce == 'qSun_M'), ],
                   aes(x = HDOY, y = flow_cfs, color = srce)) +
            geom_line(size = 0.6) + facet_wrap(~ HY, ncol = 4) + 
            scale_color_manual(values = c('darkblue', 'darkred'),
                               labels = c('Gage data', 'Model data')) +
            scale_y_log10(limits = c(0.1, 5000), labels = comma) +
            scale_x_continuous(breaks = c(15, 46, 76, 107, 138, 166, 197, 227,
                                          258, 288, 319, 350),
                               labels = c('15' = 'O', '46' = 'N', '76' = 'D',
                                          '107' = 'J', '138' = 'F', '166' = 'M',
                                          '197' = 'A', '227' = 'M', '258' = 'J',
                                          '288' = 'J', '319' = 'A', '350' = 'S')) +
            ylab("Flow (cfs)") +
            geom_hline(yintercept = qntSun[1], size = 0.4, linetype = 2) +
            geom_hline(yintercept = qntSun[3], size = 0.4, linetype = 2) +
            geom_hline(yintercept = qntSun[5], size = 0.4, linetype = 2) +
            annotate("text", 330, qntSun[1], label = pcNm[1], vjust = 0, size = 3.0) +
            annotate("text", 330, qntSun[3], label = pcNm[3], vjust = 0, size = 3.0) +
            annotate("text", 330, qntSun[5], label = pcNm[5], vjust = 0, size = 3.0) +
            theme_bw() + theme(legend.position = c(0.75, 0.10),
                               axis.title.x = element_blank()) +
            guides(color = guide_legend(title = 'Flow data source'))
  
  ggsave(filename = paste0('ts_plot_slz_', n, '.png'), plot = slzPlot,
         path = pltPath, width = 10, height = 6.5, dpi = 300, units = 'in')  
  
  ggsave(filename = paste0('ts_plot_sun_', n, '.png'), plot = sunPlot,
         path = pltPath, width = 10, height = 6.5, dpi = 300, units = 'in')
  
  # CALIBRATION COMPONENTS ----
  # TIME (MATCHING) DEPENDENT:
  
  # - Mean Daily NSE      (12 * 365 = 4382 observations)
  dayNSESlz <- round(NSE(datDly$qSlz_M,datDly$qSlz_G,
                         na.rm = TRUE, FUN = log), 3) 
  
  dayNSESun <- round(NSE(datDly$qSun_M, datDly$qSun_G,
                         na.rm = TRUE, FUN = log), 3)
  
  # - Monthly NSE         (12 *  12 =  144 observations)
  mntNSESlz <- round(NSE(datMnt$qSlz_M, datMnt$qSlz_G,
                         na.rm = TRUE, FUN = NULL), 3)
  
  mntNSESun <- round(NSE(datMnt$qSun_M, datMnt$qSun_G,
                         na.rm = TRUE, FUN = NULL), 3)
  
  # - Peak Storm Flows (2 x 150 = 300 observations)
  stmNSESlz <- round(NSE(qStmSlz$qSlz_M, qStmSlz$qSlz_G,
                         na.rm = TRUE, FUN = log), 3)
  
  stmNSESun <- round(NSE(qStmSun$qSun_M, qStmSun$qSun_G,
                         na.rm = TRUE, FUN = log), 3)
  
  # TIME INDEPENDENT:
  # - Flow duration curve
  fdcNSESlz = round(calib_FDC(datDly$qSlz_G, datDly$qSlz_M,
                              pltPath, 'slz', n), 3)
  
  fdcNSESun = round(calib_FDC(datDly$qSun_G, datDly$qSun_M, 
                              pltPath, 'sun', n), 3)
  
  # - Annual volume error
  datYrl <- aggregate(datMnt[, 2 : 5], by = list(datMnt$HY), sum, na.rm = FALSE)
  
  datYrl[is.na(datYrl$qSun_G), 5] <- NA
  
  colnames(datYrl)[1] <- 'Date'
  
  datYrl$SZErr <- round(100 * (datYrl$qSlz_M - datYrl$qSlz_G) /
                          datYrl$qSlz_G, 1)
  
  datYrl$SnErr <- round(100 * (datYrl$qSun_M - datYrl$qSun_G) /
                          datYrl$qSun_G, 1)
  
  # Total Percent BIAS (PBIAS)
  volTotal <- colSums(datYrl[, 2 : 5], na.rm = TRUE)
  
  pBallSlz <- round(100 * (volTotal[2] - volTotal[1]) / volTotal[1], 1)
  
  pBallSun <- round(100 * (volTotal[4] - volTotal[3]) / volTotal[3], 1)
  
  # - Dry season volume error (July - Sept)
  datDry <- datMnt[datMnt$MN %in% c(7, 8, 9), ]
  
  datDry[is.na(datDry$qSun_G), 5] <- NA
  
  dryTotal <- colSums(datDry[, 2 : 5], na.rm = TRUE)
  
  pBDrySlz <- round(100 * (dryTotal[2] - dryTotal[1]) / volTotal[1], 1)
  
  pBDrySun <- round(100 * (dryTotal[4] - dryTotal[3]) / dryTotal[3], 1)
  
  # Storm (Upper 10% flows)
  datStrSlz <- datDly[datDly$qSlz_G >= qntSlz[5], c(1 : 3)]
  
  datStrSun <- datDly[datDly$qSun_G >= qntSun[5], c(1, 4, 5)]
  
  datStrSun <- datStrSun[complete.cases(datStrSun), ]
  
  # Convert to 1000 x ac-ft  
  datStrSlz[, 2 : 3] <- datStrSlz[, 2 : 3] * 1.98347 / 1000
  
  datStrSun[, 2 : 3] <- datStrSun[, 2 : 3] * 1.98347 / 1000
  
  volStrSlz <- colSums(datStrSlz[, 2 : 3], na.rm = TRUE)
  
  volStrSun <- colSums(datStrSun[, 2 : 3], na.rm = TRUE)
  
  pBStrSlz <- round(100 * (volStrSlz[2] - volStrSlz[1]) / volStrSlz[1], 2)
  
  pBStrSun <- round(100 * (volStrSun[2] - volStrSun[1]) / volStrSun[1], 2)
  
  calSttOut <- data.frame('dNSESz' = dayNSESlz,
                          'mNSESz' = mntNSESlz,
                          'sNSESz' = stmNSESlz,
                          'FDCSz' = fdcNSESlz,
                          'PBSz' = pBallSlz,
                          'PBDSz' = pBDrySlz,
                          'PBSSz' = pBStrSlz,
                          'dNSESn' = dayNSESun,
                          'mNSESn' = mntNSESun,
                          'sNSESn' = stmNSESun,
                          'FDCSn' = fdcNSESun,
                          'PBSn' = pBallSun, 
                          'PBDSn' = pBDrySun,
                          'PBSSn' = pBStrSun,
                          stringsAsFactors = FALSE)
  
  write.csv(calSttOut, 'D:/siletz/calib/cal_stat.csv', row.names = FALSE)
  
  # TIDY UP! ----
  # Move the output files to storage folders
  move_hspf_files(filPath, n)
  
  # Update the run number and write back to the file
  n = n + 1
  
  writeLines(as.character(n), countFil)
  
  close(countFil)
  
}
