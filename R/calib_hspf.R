for (i in 1) {
  
  suppressMessages(library(hydroGOF))
  suppressMessages(library(ggplot2))
  suppressMessages(library(scales))
  suppressMessages(library(lubridate))

  options(warn = -1)
  
  # Load the functions ----
  filPath <- 'C:/siletz/'
  
  datPath <- paste0(filPath, 'calib')
  
  pltPath <- paste0(datPath, '/plots')
  
  # Source the functions ----
  sapply(paste0(filPath, 'scripts/R/',
                c('calib_monNSE.R', 'calib_FDC.R',
                  'move_hspf_files.R')), source)
  
  # Read the counter from file to get the run number ----
  countFil = file(paste0(filPath, 'count.txt'))
  
  n = as.numeric(readLines(countFil))
  
  # Calibration Inputs ----
  strCal <- '2008-10-01'
  endCal <- '2014-09-30'
  strCal <- as.POSIXct(strCal, format = '%Y-%m-%d')
  endCal <- as.POSIXct(endCal, format = '%Y-%m-%d')
  
  ts <- data.frame('Date' = seq(strCal, endCal, 3600))
  
  objFun <- list('dayNSE' = TRUE,            # Daily Nash-Sutcliffe Efficiency
                 'monNSE' = TRUE,            # Monthly Nash-Sutcliffe Efficiency
                 'flwDur' = TRUE,            # Flow Duration Curves
                 'transf' = NULL,            # Specify transformation
                 'annPer' = c('year round')) # Period of assessment; e.g.'fall'
  
  fdcPar <- c(100, 1) # Inputs for Flow Duration Curve
  
  calStt = list('dayNSE' = NULL,           # Daily Nash-Sutcliffe Efficiency
                'monNSE' = NULL,           # Monthly Nash-Sutcliffe Efficiency
                'fdcRMSE' = NULL)          # Flow duration curve RMSE
  
  # Load model data ----
  qData <- read.csv('C:/siletz/calib/qmod.csv', stringsAsFactors = FALSE)
  
  # qData <- qData[, c(1, 12)]
  
  names(qData) <- c('Date', 'B11')
  
  qData$Date <- as.POSIXct(qData$Date,
                          '%Y-%m-%d %H:%M:%S',
                          tz = 'America/Los_Angeles')
  
  qData <- qData[(qData$Date >= strCal & qData$Date <= endCal), ]

  # Load observations ----
  qGage <- read.csv('C:/siletz/calib/gge.csv', stringsAsFactors = FALSE)
  
  qGage$DATE <- as.POSIXct(qGage$DATE,
                           '%m/%d/%Y %H:%M',
                           tz = 'America/Los_Angeles')

  qGage$Q_slz <- as.numeric(qGage$Q_slz)
  
  # Merge the tables into one ----
  calDat <- merge(ts, qData, by.x = 'Date', by.y = 'Date', all.x = TRUE)
  
  calDat <- merge(calDat, qGage, by.x = 'Date', by.y = 'DATE', all.x = TRUE)
  
  names(calDat) <- c('Date', 'MDL_Q', 'GGE_Q')
  
  # Run the calibration ----
  trnsFun = objFun[['transf']]
  
  if(!is.null(objFun[['dayNSE']])) {
    
    calStt[['dayNSE']] = NSE(calDat$MDL_Q,
                             calDat$GGE_Q,
                             na.rm = TRUE,
                             FUN = trnsFun)
    
  }
  
  if (!is.null(objFun[['monNSE']])) {
    
    calStt[['monNSE']] = calib_monNSE(calDat, trnsFun)
    
  }
  
  if (!is.null(objFun[['flwDur']])) {
    
    calStt[['fdcRMSE']] = calib_FDC(calDat, fdcPar, pltPath, n)
    
  }
  
  calSttOut = data.frame(dailyNSE = calStt[['dayNSE']],
                         monthNSE = calStt[['monNSE']],
                         fdcRMSE = calStt[['fdcRMSE']],
                         stringsAsFactors = FALSE)
  
  write.csv(calSttOut, 'C:/siletz/calib/cal_stat.csv')
  
  # SET NEW PLOT LIMITS TO ONE YEAR; COMMENT OUT IF YOU WANT THE WHOLE YEAR ----
  strCal <- '2008-10-01'
  endCal <- '2014-09-30'
  strCal <- as.POSIXct(strCal, format = '%Y-%m-%d')
  endCal <- as.POSIXct(endCal, format = '%Y-%m-%d')
  # ----------------------------------------------------------------------------

  # Print out calibration graphs 1) timeseries 2) scatterplot 3) ----
  calPlot = ggplot(data = calDat) +
    geom_line(aes(x = Date, y = MDL_Q), size = 1.1, color = "blue") + 
    geom_point(aes(x = Date, y = GGE_Q), size = 1.2, color = 'red') + 
    scale_y_log10(limits = c(10, 50000)) + 
    scale_x_datetime(limits = c(strCal, endCal),
                     breaks = date_breaks("3 months"),
                     labels = date_format("%Y-%b")) +
    xlab("Date") + ylab("Flow (cfs)") +
    theme_bw() + theme(legend.position = c(0, 1),
                       panel.grid.minor=element_blank(),
                       axis.title.x = element_blank(),
                       axis.text.x = element_text(size = 13,
                                                  angle = 45,
                                                  hjust = 1),
                       axis.title.y = element_text(size = 13),
                       axis.text.y = element_text(size = 13),
                       plot.title = element_text(size = 13, hjust = 0.5)) +
    annotate("text", x = strCal + 120 * 86400, y = 10, size = 16,
             label = paste0('RUN ', n), hjust = 0)

  ggsave(filename = paste0('calibration_plot_', n, '.png'),
         plot = calPlot, path = pltPath,
         width = 15, height = 10,
         dpi = 300, units = 'in')
  
  # Move the output files to storage folders
  move_hspf_files(filPath, n)
  
  # Update the run number and write back to the file
  n = n + 1
  
  writeLines(as.character(n), countFil)

  close(countFil)

}
