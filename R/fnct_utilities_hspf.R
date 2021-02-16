#_______________________________________________________________________________
# READ THE HSPF CONTROL FILE
read_ctrF_H <- function() {
  
  ctrF <- readLines("C:/siletz_tmdl/03_models/hspf_ctrl.csv")
  
  ctrF <- ctrF[-which(substr(ctrF, 1, 3) == '###' | ctrF == '')]
  
  ctrF <- strsplit(ctrF, ',')
  
  for (i in 1 : length(ctrF)) {
    names(ctrF)[i] <- ctrF[[i]][1]; ctrF[[i]] <- ctrF[[i]][2]
  }
  
  return(ctrF)
}

#_______________________________________________________________________________
proc_flow_4_wq <- function(hydros = 'C:/siletz_tmdl/02_outputs/01_hspf/',
                           oFil = 'C:/siletz_tmdl/02_outputs/01_hspf/base_qOut.RData',
                           nFil = 'NoNameSpecified') {

  # Process pyHSPF flow output data for use in the WQ model component
  options(stringsAsFactors = FALSE, scipen = -1, warn = -1)
  
  if (substr(hydros, nchar(hydros), nchar(hydros)) != '/') {hydros <- paste0(hydros, '/')}

  # If hydros are not being run, but wq is copy old hydros to new hydros  
  if (!is.null(oFil)) {
    
    file.copy(from = oFil, to = paste0(hydros, nFil, '_qOut.RData'))
    
  } else {

    tmZn <- 'America/Los_Angeles'
    
    # IMPORT DATA 
    qLat   <- read.csv(paste0(hydros, nFil, '_runoff.csv')) # (mm/ts)
    HRUs   <- read.csv('C:/siletz_tmdl/01_inputs/01_hspf/lulc/hru_lng.csv') # HRU areas
    qRch   <- read.csv(paste0(hydros, nFil, '_reach_flow.csv')) # (m3/s)
    latNme <- read.csv(paste0('C:/siletz_tmdl/01_inputs/01_hspf/lulc/siletz_per',
                              'lnd_runoff_names.csv')) # Names
    
    # ORGANIZE DATA 
    # Dates
    ts <- seq(as.POSIXct(qRch$Date[1], '%Y-%m-%d %H:%M:%S', tz = tmZn),
              as.POSIXct(qRch$Date[nrow(qRch)], '%Y-%m-%d %H:%M:%S', tz = tmZn),
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
    
    comp <- list(qLat = qLat, qRch = qRch, HRUs = HRUs)
    
    saveRDS(comp, paste0('C:/siletz_tmdl/02_outputs/01_hspf/', nFil, '_qOut.RData'))
    
    # Delete the .csv hydros
    qLat <- file.remove(paste0(hydros, nFil, '_runoff.csv'))
    qRch <- file.remove(paste0(hydros, nFil, '_reach_flow.csv'))
    
    return(comp)

  }
}

#_______________________________________________________________________________
proc_qlc <- function(emc = NULL, parV = NULL, qLat = NULL, lLat = NULL) {
  
  # Synopsis 
  # This function takes the raw lateral flows/loads/conc and aggregates them into
  # either A) QLC by basin B) QLC by HRU or C) QLC by runoff component
  
  # By basin pre-processing 
  lstB <- data.frame(BAS = unique(emc$BAS))
  
  if(parV != 'BAS') {
    
    lstPar <- data.frame(unique(emc[, c(parV, 'BAS')]))
    
    pars <- unique(lstPar[-1, 1])
    
    parDim <- length(pars)
    
    if(parV == 'HRU') {x <- 1} else if (parV == 'ROC') {x = 2}
    
  }
  
  # PROCESS FLOWS, LOADS & CONC AGGREGATED BY BASIN 
  if(parV == 'BAS') {
    
    # Initialize basin data frames
    tQ <- tL <- tC <- data.frame(qLat$Date, matrix(0, nrow = nrow(qLat),
                                                   ncol = (nrow(lstB) - 1)))
    
    names(tQ)[1] <- names(tL)[1] <- names(tC)[1] <- 'Date'
    
    for (i in 2 : nrow(lstB)) {
      
      tInd <- which(emc[, 3] == lstB[i, 1]) # These are the column indx to add
      
      tQ[, i] <- rowSums(qLat[, tInd]) # temporary flow DF of basin i
      
      tL[, i] <- rowSums(lLat[, tInd]) # temporary load DF of basin i
      
      tC[, i] <- rowSums(lLat[, tInd]) / (rowSums(qLat[, tInd] * 3.6))
      
      tC[which(is.nan(tC[, i]) | is.na(tC[, i])), i] <- 0
      
    }
    
    latQLC <- list(flow = tQ, load = tL, conc = tC)
    
    for (j in 1 : length(latQLC)) {
      
      names(latQLC[[j]]) <- c('Date', paste0('Bas', lstB[2 : nrow(lstB), 1]))
      
    }
    
  # PROCESS FLOWS, LOADS & CONC AGGREGATED BY HRU or RUNOFF COMPONENT 
  } else {
    
    parV = c(parV, 'BAS')
    
    tQ <- tL <- tC <- data.frame(qLat$Date, matrix(0, nrow = nrow(qLat),
                                                   ncol = parDim))
    
    names(tQ) <- names(tL) <- names(tC) <- c('Date', pars)
    
    latQLC <- list()
    
    for (i in 2 : nrow(lstB)) {
      
      tmpEMC <- emc[which(emc$BAS == lstB[i, 1]), ]
      
      for (j in 1 : parDim) {
        
        tInd <- tmpEMC[which(tmpEMC[, x] == pars[j]), 4]
        
        tQ[, j + 1] <- rowSums(qLat[, tInd]) # temporary flow DF of basin i
        
        tL[, j + 1] <- rowSums(lLat[, tInd]) # temporary load DF of basin i
        
        tC[, j + 1] <- rowSums(lLat[, tInd]) / (rowSums(qLat[, tInd] * 3.6))
        
        tC[which(is.nan(tC[, j + 1]) | is.na(tC[, j + 1])), j + 1] <- 0
        
      }
      
      latQLC[['flow']][[i]] <- tQ
      
      latQLC[['load']][[i]] <- tL
      
      latQLC[['conc']][[i]] <- tC
      
    }
    
    for (j in 1 : length(latQLC)) {
      
      latQLC[[j]][[1]] <- NULL
      
      names(latQLC[[j]]) <- paste0('Bas', lstB[2 : nrow(lstB), 1])
      
    }
  }
  
  return(latQLC)
  
}

#_______________________________________________________________________________
preproc_wq <- function(nmVec = NULL, emcFil = NULL) {
  
  # Synopsis 
  # This function takes the raw wq parameter file and processes it for use by 
  # proc_emcdwc(). This includes creating a data frame with all of the HRUs and 
  # runoff components and all of the wq concentrations and seasonal harmonic 
  # parameters
  
  # Set concentrations from emcdwc inputs
  emcdwc <- data.frame(do.call(rbind, strsplit(nmVec, '_')))
  
  names(emcdwc) <- c('ROC', 'BAS', 'HRU')
  
  emcdwc$indx <- as.numeric(row.names(emcdwc))
  
  # Bring in the concentration values
  emcdwc2 <- read.csv(emcFil)
  
  emcdwc2 <- rbind(emcdwc2, emcdwc2[1 : 4, ])
  
  emcdwc2$HRU[1 : 4] <- paste0(emcdwc2$HRU[1 : 4], 'HI')
  
  emcdwc2$HRU[6 : 9] <- paste0(emcdwc2$HRU[6 : 9], 'LO')
  
  # Combine the tables
  emcdwc <- merge(emcdwc, emcdwc2, by.x = 'HRU', by.y = 'HRU',
                  all.x = TRUE, all.y = FALSE)
  
  # Set the outflow concentration to either EMC (SURO & IFWO) or DWC (AGWO) 
  # emcdwc$conc <- ifelse(emcdwc$ROC == 'AGWO', emcdwc$AGWO,
  #                       ifelse(emcdwc$ROC == 'IFWO', emcdwc$IFWO, emcdwc$SURO))
  
  emcdwc <- emcdwc[order(emcdwc$indx), ]
  
  row.names(emcdwc) <- emcdwc$indx
  
  return(emcdwc)
  
}

#_______________________________________________________________________________
proc_network_linkage = function(shpFile) {
  
  # Synopsis 
  # This function processes the stream flow network from upstream to downstream 
  # based on a user specified shapefile which contains each basin and corresponding
  # upstream and downstream basins. Only considers multiple U/S basins, not
  # multiple D/S basins. This function returns a list of upstream basin(s) for each
  # basin and the processing order for reach outflow, i.e., which basins need to
  # be processed first to last.
  
  suppressMessages(library(raster))
  
  shpFile = shapefile(shpFile)
  
  lnks = data.frame(cbind('Basn' = as.numeric(shpFile@data[["HSPF_Bas"]]),
                          'DSBs' = as.numeric(shpFile@data[["DS_Basin"]])),
                    stringsAsFactors = FALSE)
  
  # List of each basin & vector of upstream basin(s); not cumulative, just the
  # immediately upstream basin(s) for upstream inflow inputs
  # Basins with no upstream basins (headwaters) have a value of 0
  
  usBas = rep(list(0), nrow(lnks)) # Upstream basins
  
  twBas = unique(lnks$DSBs) # Tailwater basins
  
  twBas = twBas[which(twBas != 0)] # remove zeros (0 = watershed outlet)
  
  for (basin in twBas) {
    
    usBas[[basin]] = lnks[which(lnks$DSBs == basin), 1]
    
  }
  
  # Make a vector of the basin processing order
  hwBas = lnks$Basn # All headwater basins
  
  hwBas = hwBas[!(hwBas %in% twBas)] # Which basins are not a tailwater basin
  
  # First order basins (headwaters)
  nthOrdBas = list()
  
  nthOrdBas[[1]] = procOrd = hwBas # initialize processing order
  
  n = 2 # Counter for indexing the loop; n = 1 addressed with headwaters
  
  # subsequent order basins
  repeat {
    
    nthOrdBas[[n]] = lnks[which(lnks$Basn %in% nthOrdBas[[n - 1]]), 2]
    
    nthOrdBas[[n]] = unique(nthOrdBas[[n]]) # remove duplicates
    
    nthOrdBas[[n]] = nthOrdBas[[n]][which(nthOrdBas[[n]] != 0)] # remove zeros
    
    if (length(nthOrdBas[[n]]) == 0) { # exit when the nthOrdBas has no elements
      
      break
      
    }
    
    procOrd = c(procOrd, nthOrdBas[[n]])
    
    n = n + 1      
  }
  
  # Now, keep the LAST indexed instance of dup basins. The first column is the
  # process order, the second is the basin to process in that order
  tmpOrd = data.frame(cbind('ORD' = as.vector(tapply(seq_along(procOrd),
                                                     procOrd, max)),
                            'BAS' = 1 : nrow(lnks)), stringsAsFactors = FALSE)
  
  tmpOrd = tmpOrd[order(tmpOrd$ORD), ]
  
  tmpOrd$ORD = 1 : nrow(lnks)
  
  rownames(tmpOrd) = 1 : nrow(lnks)
  
  procLnks = list('pOrd' = tmpOrd, 'cBas' = usBas)
  
  return(procLnks)
  
}

#_______________________________________________________________________________
runoff_components <- function(strD = NULL, endD = NULL, wqDir = NULL,
                              emcFil = NULL) {
  
  # Synopsis 
  # Function to parse out components of runoff (flow volume) associated with
  # Surface, interflow and base flow. Contains unused water quality components.
  # This function is only used in the watershed model calibration process.
  
  # Libraries, scripts and options 
  options(stringsAsFactors = FALSE)

  # Load and process data 
  qOut <- readRDS('C:/siletz_tmdl/02_outputs/01_hspf/base_qOut.RData')
  
  # Reduce from qOut to lateral loads of specified dates 
  qLat <- reduce_qlc(strDte = strD, endDte = endD, df2Red = qOut[["qLat"]])
  
  # Pre-proces emcdwc table
  nmVec <- names(qOut[['qLat']])
  
  emcdwc <- preproc_wq(nmVec = nmVec, emcFil = emcFil)
  
  # Calculate lateral loads 
  lLat <- qLat # Initialize the df
  
  # Extract column indeces of SURO, IFWO and AGWO
  ind <- list(SURO = which(emcdwc$ROC == 'SURO'),
              IFWO = which(emcdwc$ROC == 'IFWO'),
              AGWO = which(emcdwc$ROC == 'AGWO'))
  
  # Create a dataframe to calculate seasonality (periodicity) of the time series
  int <- data.frame(Date = lLat$Date)
  
  int  <- int %>%
          mutate(hyr = hydro_year(Date), doy = day_of_hydro_year(Date)) %>%
          mutate(dys = ifelse((hyr %% 4) == 0, 366, 365), yr = hyr - hyr[1]) %>%
          mutate(per = yr + doy / dys)
  
  # CALCULATE LATERAL LOADS 
  for (i in 1 : 3) {
    
    if(names(ind)[i] == 'SURO') {
      
      for (j in ind[[i]]) {lLat[, j] <- qLat[, j] * emcdwc[j, 'SURO'] * 3.6}
      
    } else {
      
      for (j in ind[[i]]) {
        
        # Select columns for seasonal variation function coefficients
        if (i == 2) {k <- 6 : 8} else {k <- 9 : 11}
        
        intConc <- emcdwc[j, k[1]] * (sin(2 * pi * int$per)) + 
                   emcdwc[j, k[2]] * (cos(2 * pi * int$per)) + 
                   emcdwc[j, k[3]]
        
        lLat[, j] <- qLat[, j] * intConc * 3.6
        
      }
    }
  }   
  
  # Non-seasonal interflow & baseflow concentrations
  # cCol <- which(names(emcdwc) == 'conc')
  # for (i in 2 : length(qLat)) {lLat[, i] <- qLat[, i] * emcdwc[i, cCol] * 3.6}
  
  # Separate out flows, loads and concentrations
  qlcLat <- proc_qlc(emc = emcdwc, parV = 'ROC', qLat, lLat) # Basin aggregate
  
  roComp <- qlcLat[['flow']]
  
  return(roComp)
  
}

#_______________________________________________________________________________
reduce_qlc <- function(strDte = NULL, endDte = NULL, df2Red = NULL) {
  
  # Synopsis 
  # This function accepts a data frame of flows and start/end dates and
  # Returns a modified data frame truncated to the start/end dates. 
  
  ts <- as.POSIXct(c(strDte, endDte), '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  ts <- seq(ts[1], ts[2], 3600)
  
  names(df2Red)[1] <- 'Date'
  
  redDF <- df2Red[df2Red$Date %in% ts, ]
  
  return(redDF)
  
}

#_______________________________________________________________________________
seasonal_wq_conc <- function(wqDF = NULL, par = NULL, lo = 0.10, hi = 0.90,
                             ts = 'none') {
  
  suppressMessages(library(TSA)); suppressMessages(library(ggplot2));
  suppressMessages(library(dplyr)); suppressMessages(library(lubridate))
  suppressMessages(library(reshape2));
  
  # ORGANIZE DATA 
  names(wqDF) <- c('Date', 'C_mgL')
  
  wqDF <- wqDF %>% mutate(mth = month(Date), day = day(Date))
  
  # Adjust dates for Apr & Dec samples to the nearest month on sample schedule:
  # Jan, Mar, May, Jul, Sep, Nov
  adj <- which(wqDF$mth == 4 | wqDF$mth == 12)
  
  wqDF$mth[adj] <- ifelse(wqDF$day[adj] >= 15,
                          wqDF$mth[adj] + 1,
                          wqDF$mth[adj] - 1)
  
  # Extract 10th and 90th percentile values (representative of IFW & AGW)
  p10 <- aggregate(wqDF$C_mgL, by = list(wqDF$mth), FUN = 'quantile', lo)
  
  p90 <- aggregate(wqDF$C_mgL, by = list(wqDF$mth), FUN = 'quantile', hi)
  
  # Create data frame of quantile values
  qtls <- data.frame(mth = p10$Group.1, p10 = p10$x, p90 = p90$x)
  
  amth <- 1 : 12
  
  adds <- data.frame(mth = amth[-qtls$mth], p10 = 0, p90 = 0)
  
  qtls <- rbind(qtls, adds)
  
  qtls <- qtls[order(qtls$mth), ]
  
  row.names(qtls) <- qtls$mth
  
  for (i in adds$mth) {
    
    if (i != 12) {
      
      qtls$p10[i] <- mean(c(qtls$p10[i - 1], qtls$p10[i + 1]))
      
      qtls$p90[i] <- mean(c(qtls$p90[i - 1], qtls$p90[i + 1]))
      
    } else {
      
      qtls$p10[i] <- mean(c(qtls$p10[1], qtls$p10[11]))
      
      qtls$p90[i] <- mean(c(qtls$p90[1], qtls$p90[11]))
      
    }
  }
  
  # Create harmonic function of quantiles
  qtls$hmt = ifelse(qtls$mth < 10, qtls$mth + 3, qtls$mth + 2 - 11)
  
  qtls$per = (qtls$hmt - 0.5) / 12
  
  f10 <- lm(p10 ~ sin(2 * pi * per) + cos(2 * pi * per) +
                  sin(4 * pi * per) + cos(4 * pi * per) +
                  sin(6 * pi * per) + cos(6 * pi * per), data = qtls)
    
  f10 <- round(f10[['coefficients']], 4)
  
  f90 <- lm(p90 ~ sin(2 * pi * per) + cos(2 * pi * per) +
                  sin(4 * pi * per) + cos(4 * pi * per) +
                  sin(6 * pi * per) + cos(6 * pi * per), data = qtls)
  
  f90 <- round(f90[['coefficients']], 4)
  
  fit <- list(f10 = f10, f90 = f90)
  
  if (ts == 'doy' | ts == 'Date') {
    
    mdDF <- data.frame(Date = seq(wqDF$Date[1], wqDF$Date[nrow(wqDF)], 1))
    
    mdDF <- mdDF %>%
            mutate(hyr = hydro_year(Date), doy = day_of_hydro_year(Date)) %>%
            mutate(dys = ifelse((hyr %% 4) == 0, 366, 365), yr = hyr - hyr[1]) %>%
            mutate(per = yr + doy / dys) %>%
            mutate(ifw = f90[2] * sin(2 * pi * per) + f90[3] * cos(2 * pi * per) +
                         f90[4] * sin(4 * pi * per) + f90[5] * cos(4 * pi * per) +
                         f90[6] * sin(6 * pi * per) + f90[7] * cos(6 * pi * per) +
                         f90[1],
                   agw = f10[2] * sin(2 * pi * per) + f10[3] * cos(2 * pi * per) +
                         f10[4] * sin(4 * pi * per) + f10[5] * cos(4 * pi * per) +
                         f10[6] * sin(6 * pi * per) + f10[7] * cos(6 * pi * per) +
                         f10[1])
    
  }
  
  if (ts == 'doy') {
    
    mdDF <- mdDF[which(mdDF$hyr == 2005), ]
    
    # Modify the WQ data for graphing
    wqDF$doy <- day_of_hydro_year(wqDF$Date)
    
    tmp <- melt(mdDF, id.vars = 'doy', measure.vars = c('ifw', 'agw'),
                variable.name = 'ROComp', value.name = 'C_mgL')
    
    lbls <- data.frame(par = c('NOx', 'NH3', 'TKN', 'TP', 'PO4', 'OrC'),
                       nme = c('Nitrate', 'Ammonia', 'Total Kjeldahl nitrogen',
                               'Total phosphorus', 'Orthophosphate',
                               'Total organic carbon'), stringsAsFactors = F)
    
    plt <- ggplot(tmp, aes(x = doy, y = C_mgL, color = ROComp)) + geom_line() +
           ylab(paste0(lbls[which(lbls$par == par), 2], ' (mg/L)')) +
           scale_color_manual(values = c('darkblue', 'darkgreen', 'darkred'),
                              labels = c('Groundwater', 'Interflow', 'Observations')) +
           theme_bw() +
           theme(legend.position = c(0.45, 0.800), axis.title.x = element_blank(),
                              legend.background = element_rect(fill = alpha('white', 0.4),
                                                               color = 'black',
                                                               size = 0.3)) +
           guides(color = guide_legend(title = 'Runoff Component',
                                       override.aes = list(linetype = c('solid',
                                                                        'solid',
                                                                        'blank'),
                                                           shape = c(45, 45, 2)))) +
           geom_point(data = wqDF, aes(x = doy, y = C_mgL, color = 'Observations'),
                      size = 1.2, shape = 2, stroke = 0.8) +
           scale_x_continuous(breaks = c(15, 46, 76, 107, 138, 166, 197, 227,
                                         258, 288, 319, 350),
                              labels = c('15' = 'O', '46' = 'N', '76' = 'D',
                                         '107' = 'J', '138' = 'F', '166' = 'M',
                                         '197' = 'A', '227' = 'M', '258' = 'J',
                                         '288' = 'J', '319' = 'A', '350' = 'S'))
    
    ggsave(paste0('seasonal_', par, '.png'), plot = plt, width = 4.250,
           height = 4.144, path = 'C:/siletz_tmdl/05_misc/figures/seasonal_wq',
           units = 'in', dpi = 300)
    
  }
  
  if (ts == 'Date') {
    
    # Modify the WQ data for graphing
    wqDF$doy <- day_of_hydro_year(wqDF$Date)
    
    plt <- ggplot(mdDF, aes(x = Date)) +
           geom_line(aes(y = ifw), color = 'darkblue') +
           geom_line(aes(y = agw), color = 'darkred') +
           geom_point(data = wqDF, aes(x = Date, y = C_mgL), size = 1.2,
                      shape = 2, stroke = 1.2, color = 'darkred', fill = 'yellow')
    
    ggsave(paste0('seasonal_ts_', par, '.png'), plot = plt, width = 10,
           height = 7.5, path = 'C:/siletz_tmdl/05_misc/figures/seasonal_wq',
           units = 'in', dpi = 300)
    
  }
  
  return(fit)
  
}

#_______________________________________________________________________________
ro_comp_analysis <- function(roCmp) {
  
  # roPct is the list of basins with percentage of each component of total (hr)
  roPct <- roCmp
  
  # roAll is a mean of percentage of each component of total flows for all basns
  roAll <- roCmp[[1]]
  
  roAll[, 2 : 4] <- 0
  
  for (i in 1 : length(roPct)) {
    
    tmp <- rowSums(roCmp[[i]][, 2 : 4])
    
    for (j in 2 : 4) {
      
      roPct[[i]][, j] <- roCmp[[i]][, j] / tmp
      
      roPct[[i]][which(is.nan(roPct[[i]][, j])), j] <- 0
      
      roAll[, j] <- roAll[, j] + roPct[[i]][, j]
      
    }
  }
  
  for (j in 2 : 4) {roAll[, j] <- roAll[, j] / length(roPct)}
  
  # compress to daily means
  roAll$Date2 <- as.Date(roAll$Date)
  
  roAll <- aggregate(roAll[, 2 : 4], by = list(roAll$Date2), FUN = 'mean')
  
  names(roAll)[1] <- 'Date'
  
  return(roAll)
  
}

#_______________________________________________________________________________
proc_daily_flows <- function(mDat, gDat) {
  
  # Synopsis 
  # This function takes two files (model and gage data) and returns a data frame
  # combined data frame of both
  
  # Read and aggregate model data 
  qData <- readRDS(mDat)
  
  qData <- qData[['reach_flows']][, c(1, 12, 5)]
  
  qData$Date2 <- as.Date(qData$Date, '%Y-%m-%d %H:%M:%S', tz = 'America/Los_Angeles')
  
  qData <- aggregate(qData[, 2 : 3], by = list(qData$Date2), FUN = 'mean')
  
  names(qData)[1] <- 'Date'
  
  # Convert m3/s to cfs
  qData[, 2 : 3] <- qData[, 2 : 3] * 35.314666721
  
  # Read and clean up gage data 
  qGage <- read.csv(gDat, stringsAsFactors = FALSE)
  
  qGage$Date <- as.Date(qGage$Date, '%Y-%m-%d', tz = 'America/Los_Angeles')
  
  # Reduce the model data set to the min-max and max-min dates
  dTrim <- c(max(min(qData$Date), min(qGage$Date)),
             min(max(qData$Date), max(qGage$Date)))
  
  qData <- qData[which(qData$Date >= dTrim[1] & qData$Date <= dTrim[2]), ]
  
  qGage <- qGage[which(qGage$Date >= dTrim[1] & qGage$Date <= dTrim[2]), ]
  
  calDat <- merge(qData, qGage, by.x = 'Date', by.y = 'Date', all.x = TRUE,
                  all.y = TRUE)
  
  names(calDat) <- c('Datetime', 'qSlz_M', 'qSun_M', 'qSlz_G', 'qSun_G')
  
  calDat[, 2 : 5] <- round(calDat[, 2 : 5], 1)
  
  calDat <- calDat[, c(1, 2, 4, 3, 5)]
  
  return(calDat)
  
}

#_______________________________________________________________________________
read_ts_loads <- function(tFil = NULL, ts = NULL, strD = NULL, endD = NULL) {
  
  # read in the loads time series data
  df <- read.csv(file = tFil)
  
  names(df)[1] <- 'date'
  
  # Find the number of basins:
  nBas <- length(df) - 1
  
  bsns <- names(df)[2 : length(df)]; bsns <- as.numeric(gsub('X', '', bsns))

  # Set up the basin time series loads object
  tsLd <- list()

  # make the time series data dates into midpoints, e.g., monthly values peak
  # on the 15th at noon, daily at noon each day
  if (ts == 'month') {
    df$date = as.POSIXct(paste0(year(strD), '-', df$date, '-15 12:00'),
                         '%Y-%m-%d %H:%M', tz = 'America/Los_Angeles')
  } else if (ts == 'day') {
    df$date = as.POSIXct(df$date, '%Y-%m-%d', tz = 'America/Los_Angeles') +
              hours(12)
  } else if (ts == 'hour') {
    df$date = as.POSIXct(df$date, '%Y-%m-%d %H:%M', tz = 'America/Los_Angeles')
  } else {break} 
  
  # create a time series
  ts <- data.frame(date = seq(strD, endD, 3600), stringsAsFactors = F)
  
  ts <- merge(ts, df, by.x = 'date', by.y = 'date', all.x = T, all.y = F)
  
  # Iterate through each basin and create a time series interporating each
  # intermediate value to an hourly time series

  for (i in 1 : nBas) {

    temp <- ts[, c(1, i + 1)]; names(temp)[2] <- 'basn'
    
    # Find the min/max indeces with non-NA values
    ends <- c(min(which(!is.na(temp$basn))), max(which(!is.na(temp$basn))))
    
    # Calculate mean of these, as this will be the value for the NA indeces
    temp[1, 2] <- temp[nrow(temp), 2] <- (temp[ends[1], 2] + temp[ends[2], 2]) / 2
    
    # Interpolate the values between the min/max valued indeces
    temp$basn <- unlist(approx(x = temp$date, y = temp$basn, xout = temp$date,
                               method = 'linear', rule = 1)[2])
    
    tsLd[[i]] <- temp; names(tsLd)[i] <- paste0('BAS', bsns[i])
    
  }

  return(tsLd)
  
}

#_______________________________________________________________________________
hspf_output <- function(iDir = NULL, iFil = NULL, oDir = NULL) {
 
  # This function runs output for Q2k input. Input parameters include:
  # 1) The directory where the input HSPF WQ Control Files
  # 2) The control file iteration name 
  # 3) The output directory location
   
  sapply(paste0('C:/siletz_tmdl/04_scripts/01_hspf/02_R/',c('fnct_wq_run_hspf.R',
                                                            'fnct_utilities_hspf.R',
                                                            'fnct_wq_calib_hspf.R')), source)
  
  cFil <- c('NOx', 'NH3', 'TKN', 'TP', 'PO4', 'OrC')
  
  #Check input file directory for end forward slash, add if doesn't exist
  if (substr(iDir, length(iDir), length(iDir)) != '/') {iDir <- paste0(iDir, '/')}
  
  # Check existence of output file directory, create if doesn't exist
  if (!file.exists(oDir)) {dir.create(oDir)}
  
  cFil <- paste0(iDir, 'wq_confil_', cFil, '_', iFil, '.csv')
  
  for (i in 1 : length(cFil)) {

    # Read the control file
    v <- read_wq_pars(cFil[i], writeCsv = TRUE)
    
    # Run the reach-based water quality modules
    rchQLC <- run_wq(v)

    # Save the reach-based water quality output files
    saveRDS(rchQLC, paste0(oDir, '/rchQLC_', v$pars,'.RData'))
    
    # Run the lateral-based water quality outpuyt modules
    latQLC <- proc_wq_latQLC(v)
    
    # Save the lateral-based water quality output files
    saveRDS(latQLC, paste0(oDir, '/latQLC_', v$pars,'.RData'))  
    
  }
  
  print('Writing output files complete. Check your output directories')
  
}

#_______________________________________________________________________________
# CALCULATE THE DAY OF THE HYDROLOGIC YEAR
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

#_______________________________________________________________________________
# CALCULATE THE HYDROLOGIC YEAR
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

