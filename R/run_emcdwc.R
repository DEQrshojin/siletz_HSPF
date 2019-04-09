run_emcdwc <- function(strD = NULL, endD = NULL, wqDir = NULL, emcFil = NULL,
                       basFil = NULL) {
  
  # Libraries, scripts and options ----
  options(stringsAsFactors = FALSE)
  
  suppressMessages(library('lubridate'))
  suppressMessages(library('dplyr'))

  sapply(c('D:/siletz/scripts/R/proc_emcdwc.R'), source)
  
  # Create a data frame of date iterations
  yrs <- year(strD) : year(endD)
  
  if (length(yrs) == 1) {
    
    dts <- data.frame(as.POSIXct(strD, '%Y-%m-%d', tz = 'America/Los_Angeles'),
                      as.POSIXct(endD, '%Y-%m-%d', tz = 'America/Los_Angeles'))
    
  } else {
    
    intDts <- paste0(yrs[2 : length(yrs)], '-01-01')
    
    dts <- data.frame(as.POSIXct(c(strD, intDts), '%Y-%m-%d',
                                 tz = 'America/Los_Angeles'),
                      as.POSIXct(c(intDts, endD), '%Y-%m-%d',
                                 tz = 'America/Los_Angeles'))
    
  }

  if (dts[nrow(dts), 1] == dts[nrow(dts), 2]) {dts <- dts[-nrow(dts), ]}
  
  # Initialize the restart (must be NA or proc_emcdwc will crash)
  restart <- NA
  
  qlcTmp <- list()
  
  tTime <- 0
  
  for (n in 1 : nrow(dts)) {
    
    a <- as.numeric(Sys.time())
    
    # Run the proc_emcdwc with calculates reach loads and concentrations
    qlcTmp <- proc_emcdwc(restart = restart, strD = dts[n, 1],
                          endD = dts[n, 2], wqDir = wqDir, emcFil = emcFil,
                          basFil = basFil)
    
    # Restart = list of 1st line of previous iteration of rchL and rchC  
    restart <- list(ldsRst = qlcTmp[[2]][nrow(qlcTmp[[2]]), ],
                    conRst = qlcTmp[[3]][nrow(qlcTmp[[3]]), ])
    
    # Set the first instance of the output list if n = 1
    if (n == 1) {qlcOut <- qlcTmp} else { 
    
      for (o in 1 : 3) { 
        
        qlcOut[[o]] <- rbind(qlcOut[[o]][-nrow(qlcOut[[o]]), ], qlcTmp[[o]])
        
      }
    }
    
    pTime <- round((as.numeric(Sys.time() - a)) / 60, 2) # time in minutes
    
    tTime <- tTime + pTime
    
    cat(paste0('Year: ', yrs[n], " processed in ", pTime, ' minutes\n'))
    
  }
  
  cat(paste0('Total processing time: ', tTime, ' minutes\n'))
  
  return(qlcOut)
  
}