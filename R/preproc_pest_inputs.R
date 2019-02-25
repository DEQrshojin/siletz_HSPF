for (i in 1) {
  # LOAD BEC WEIGHTS ----
  pstFil <- paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/HSPF/',
                   'Big-Elk-Cadmus-HydCal-Updated-WDM/pest-hspf-files/',
                   'pred-base-pre-cal/temp.pst')
  
  source('C:/siletz/scripts/R/analyze_bec_objfun.R')
  
  regs <- analyze_bec_objfun(pstFil)
  
  totalWeight <- sum(regs[['weights']]$x)
  
  regs[['weights']]$pct <- 100 * (regs[['weights']]$x / totalWeight)
  
  library(tidyverse)
  library(lubridate)
  
  # Create data frame for pest control file observation data
  obsDat <- data.frame(obsNme = as.character(),
                       value = as.numeric(),
                       weight = as.numeric(),
                       obsgrp = as.character(),
                       stringsAsFactors = FALSE)
  
  # MEAN DAILY FLOW - qdaysz & qdaysn ----
  qData <- read.csv('C:/siletz/calib/gge.csv', stringsAsFactors = FALSE)
  
  # OUTPUT GAGE NA DATA FILE FOR MODEL DATA AUDITING
  dlyNA <- data.frame(date = as.character(),
                      gage = as.character(),
                      indx = as.numeric(),
                      stringsAsFactors = FALSE)
  
  gage <- c('Slz', 'Sun')
  
  for (i in 1 : 2) {
    
    indx <- which(is.na(qData[, i + 1]))
    
    tmp <- data.frame(date = qData[indx, 1],
                      gage = gage[i],
                      indx = indx,
                      stringsAsFactors = FALSE)
    
    dlyNA <- rbind(dlyNA, tmp)
    
  }
  
  # write.csv(dlyNA, 'C:/siletz/pest/dlyNA.csv', row.names = FALSE)
  
  # Set date values
  qData$Mn <- month(qData$Date)
  
  qData$Yr <- year(qData$Date)
  
  # Dummy month date
  qData$Mndt <- as.Date(paste0(qData$Yr, '-', ifelse(qData$Mn < 10, 0, ''), 
                               qData$Mn, '-01'), '%Y-%m-%d')
  
  # hydro year
  qData$HY <- ifelse(qData$Mn >= 10, qData$Yr + 1, qData$Yr)
  
  # Names
  qData$nmSlz <- paste0('qdaysz',
                        ifelse(year(qData$Date) - 2000 < 10, 0, ''),
                        year(qData$Date) - 2000,
                        ifelse(month(qData$Date) < 10, 0, ''), month(qData$Date),
                        ifelse(day(qData$Date) < 10, 0, ''), day(qData$Date))
  
  qData$nmSun <- paste0('qdaysn',
                        ifelse(year(qData$Date) - 2000 < 10, 0, ''),
                        year(qData$Date) - 2000,
                        ifelse(month(qData$Date) < 10, 0, ''), month(qData$Date),
                        ifelse(day(qData$Date) < 10, 0, ''), day(qData$Date))
  
  # Log transform daily flows
  qData$qlgSlz <- log10(qData$qSlz)
  
  qData$qlgSun <- log10(qData$qSun)
  
  # DAILY WEIGHTS
  dlywgt <- function(q, a, b) {10^b * q^a}

  sun2SlzFactor <- mean(qData$qSun / qData$qSlz, na.rm = TRUE)
  
  qData$wgtSlz <- sapply(qData$qlgSlz, dlywgt,
                         regs[["qday"]][["coefficients"]][["logval"]],
                         regs[["qday"]][["coefficients"]][["(Intercept)"]])
  
  qData$wgtSun <- sapply(qData$qlgSun, dlywgt,
                         regs[["qday"]][["coefficients"]][["logval"]],
                         regs[["qday"]][["coefficients"]][["(Intercept)"]]) *
                  sun2SlzFactor
                  
  
  qData[(is.nan(qData$wgtSun) | !is.finite(qData$wgtSun)), 13] <- NA
  
  # Still need to fill some Sunshine NAs
  qData$wgtSun <- ifelse(is.na(qData$wgtSun), qData$wgtSlz * sun2SlzFactor,
                         qData$wgtSun)

  # DAILY OBSERVATION NAMES
  qData$grpSz <- 'qdaysz'
  
  qData$grpSn <- 'qdaysn'
  
  # Add the data for Siletz log daily flows
  tmp <- qData[, c(8, 10, 12, 14)]
  
  names(tmp) <- c('obsNme', 'value', 'weigth', 'obsgrp')
  
  obsDat <- rbind(obsDat, tmp)
  
  # Add the data for Sunshine log daily flows
  tmp <- qData[, c(9, 11, 13, 15)]
  
  names(tmp) <- c('obsNme', 'value', 'weigth', 'obsgrp')
  
  obsDat <- rbind(obsDat, tmp)
  
  # MONTHLY VOLUME - vmonsz & vmonsn ----
  datMnt <- aggregate(qData[, 2 : 3], by = list(qData$Mndt), sum,
                      na.rm = TRUE)
  
  colnames(datMnt)[1] <- 'Date'
  
  # Convert to 1000 x AF (1 cfs = 1.98347 AF/day)
  datMnt[, 2 : 3] <- datMnt[, 2 : 3] * 1.98347 / 1000
  
  # Set Sunshine monthly totals to 0
  datMnt$qSun <- ifelse(datMnt$qSun == 0, NA, datMnt$qSun)
  
  datMnt$Mn <- month(datMnt$Date)
  
  datMnt$nmSlz <- paste0('vmonsz', year(datMnt$Date),
                         ifelse(month(datMnt$Date) < 10, 0, ''),
                         month(datMnt$Date))
  
  datMnt$nmSun <- paste0('vmonsn', year(datMnt$Date),
                         ifelse(month(datMnt$Date) < 10, 0, ''),
                         month(datMnt$Date))
  
  datMnt$wgt <- (regs[["weights"]][["x"]][7] + regs[["weights"]][["x"]][9]) /
                 (nrow(datMnt) * 2)
  
  datMnt$grpSz <- 'vmonsz'
  
  datMnt$grpSn <- 'vmonsn'
  
  # Add the data for Siletz monthly flow volumnes
  tmp <- datMnt[, c(5, 2, 7, 8)]
  
  names(tmp) <- c('obsNme', 'value', 'weigth', 'obsgrp')
  
  obsDat <- rbind(obsDat, tmp)
  
  # Add the data for Sunshine monthly flow volumnes
  tmp <- datMnt[, c(6, 3, 7, 9)]
  
  names(tmp) <- c('obsNme', 'value', 'weigth', 'obsgrp')
  
  obsDat <- rbind(obsDat, tmp)
  
  # DRY SEASON VOLUME (Jul-Sep) - vdrysz & vdrysn ----
  datDry <- datMnt[(datMnt$Mn >= 7 & datMnt$Mn <= 9), ]
  
  datDry$Yr <- year(datDry$Date)
  
  datDrySlz <- aggregate(datDry$qSlz, by = list(datDry$Yr), 'sum')
  
  datDrySun <- aggregate(datDry$qSun, by = list(datDry$Yr), 'sum')
  
  datDry <- merge(datDrySlz, datDrySun, by.x = 'Group.1', by.y = 'Group.1')
  
  names(datDry) <- c('Date', 'Slz', 'Sun')
  
  datDry[12, 3] <- NA
  
  datDry$nmSlz <- paste0('vdrysz', datDry$Date)
  
  datDry$nmSun <- paste0('vdrysn', datDry$Date)
  
  datDry$wgt <- regs[["weights"]][["x"]][7] / (2 * nrow(datDry))
  
  datDry$grpSz <- 'vdrysz'
  
  datDry$grpSn <- 'vdrysn'
  
  # Add the data for Siletz dry season flow volumnes
  tmp <- datDry[, c(4, 2, 6, 7)]
  
  tmp[, 3] <- tmp[, 3] * 1 # Obj fun weight factor for Sunshine dry season
  
  names(tmp) <- c('obsNme', 'value', 'weigth', 'obsgrp')
  
  obsDat <- rbind(obsDat, tmp)
  
  # Add the data for Sunshine dry season flow volumnes
  tmp <- datDry[, c(5, 3, 6, 8)]
  
  tmp[, 3] <- tmp[, 3] * 1 # Obj fun weight factor for Sunshine dry season
  
  names(tmp) <- c('obsNme', 'value', 'weigth', 'obsgrp')
  
  obsDat <- rbind(obsDat, tmp)
  
  # ANNUAL VOLUME - vannsz & vannsn ----
  datYr <- aggregate(qData[, 2 : 3], by = list(qData$HY), sum,
                     na.rm = TRUE)
  
  colnames(datYr)[1] <- 'Date'
  
  # remove Sunshine gage 2008, 2009, 2015 & 2016
  datYr[(datYr$Date == 2008 |datYr$Date == 2009 |
           datYr$Date == 2015 | datYr$Date == 2016), 3] <- NA
  
  # Convert to 1000 x AF (1 cfs = 1.98347 AF/day)
  datYr[, 2 : 3] <- datYr[, 2 : 3] * 1.98347 / 1000
  
  datYr$nmSlz <- paste0('vannsz', datYr$Date)
  
  datYr$nmSun <- paste0('vannsn', datYr$Date)
  
  datYr$wgt <- regs[["weights"]][["x"]][6] / (2 * nrow(datYr))
  
  datYr$grpSz <- 'vannsz'
  
  datYr$grpSn <- 'vannsn'
  
  # Add the data for Siletz annual flow volumnes
  tmp <- datYr[, c(4, 2, 6, 7)]
  
  names(tmp) <- c('obsNme', 'value', 'weigth', 'obsgrp')
  
  obsDat <- rbind(obsDat, tmp)
  
  # Add the data for Sunshine annual flow volumnes
  tmp <- datYr[, c(5, 3, 6, 8)]
  
  names(tmp) <- c('obsNme', 'value', 'weigth', 'obsgrp')
  
  obsDat <- rbind(obsDat, tmp)
  
  # USE PEAK STORM FLOWS INSTEAD OF 10% EXCEEDENCE FLOW VOLUME ----
  # Storm (Upper 10% flows)
  q10Slz <- quantile(qData$qSlz, 0.90, na.rm = TRUE)

  q10Sun <- quantile(qData$qSun, 0.90, na.rm = TRUE)

  q10Slz <- qData[qData$qSlz >= q10Slz, c(1 : 2)]

  q10Sun <- qData[qData$qSun >= q10Sun, c(1, 3)]

  q10Slz <- q10Slz[complete.cases(q10Slz), ]

  q10Sun <- q10Sun[complete.cases(q10Sun), ]

  # Convert to 1000 x ac-ft
  q10Slz[, 2] <- q10Slz[, 2] * 1.98347 / 1000

  q10Sun[, 2] <- q10Sun[, 2] * 1.98347 / 1000

  q10Tot <- data.frame('vol10pct' = c(sum(q10Slz[, 2], na.rm = TRUE),
                                      sum(q10Sun[, 2], na.rm = TRUE)),
                       'v10pnm' = c('v10psz001', 'v10psn001'),
                       'v10grp' = c('v10psz', 'v10psn'))

  # Increase weight of top 5% -- Multiply by XX
  q10Tot$wgt <- 0.0 * (regs[["weights"]][["x"]][8] / 2)

  tmp <- q10Tot[, c(2, 1, 4, 3)]

  names(tmp) <- c('obsNme', 'value', 'weigth', 'obsgrp')

  obsDat <- rbind(obsDat, tmp)

  # FLOW DURATION CURVES, fdcqsz & fdcqsn ----
  flwDur = data.frame('PCT' = seq(from = 0, to = 100, by = 1) / 100)
  
  flwDur$revPCT <- seq(from = 100, to = 0, by = -1) / 100
  
  # Calculate percentiles
  flwDur$qSlz = quantile(qData$qSlz, flwDur$PCT, na.rm = TRUE)
  
  flwDur$qSun = quantile(qData$qSun, flwDur$PCT, na.rm = TRUE)
  
  flwDur$slzNme <- paste0('fdcqsz', ifelse(flwDur$revPCT < 0.10, '00',
                                           ifelse(flwDur$revPCT < 1, '0', '')),
                          flwDur$revPCT * 100)
  
  flwDur$sunNme <- paste0('fdcqsn', ifelse(flwDur$revPCT < 0.10, '00',
                                           ifelse(flwDur$revPCT < 1, '0', '')),
                          flwDur$revPCT * 100)
  
  flwDur$wgt <- regs[['weights']][3, 2] / (nrow(flwDur) * 2)
  
  flwDur$grpSz <- 'fdcqsz'
  
  flwDur$grpSn <- 'fdcqsn'
  
  # Add the data for Siletz flow duration curve percentiles
  tmp <- flwDur[, c(5, 3, 7, 8)]
  
  names(tmp) <- c('obsNme', 'value', 'weigth', 'obsgrp')
  
  obsDat <- rbind(obsDat, tmp)
  
  # Add the data for Siletz flow duration curve percentiles
  tmp <- flwDur[, c(6, 4, 7, 9)]
  
  names(tmp) <- c('obsNme', 'value', 'weigth', 'obsgrp')
  
  obsDat <- rbind(obsDat, tmp)
  
  # PEAK STORM FLOWS - qstmsz & qstmsn ----
  qStm <- read.csv("C:/siletz/pest/stmObs.csv", stringsAsFactors = FALSE)
  
  names(qStm) <- c('obsNme', 'value', 'weigth', 'obsgrp')
  
  obsDat <- rbind(obsDat, qStm)

  # TIDY UP DATA AND OUTPUT TO CSV ----
  # Find index and name of NAs
  nas <- data.frame(naNme = obsDat[which(is.na(obsDat[, 2])), 1],
                    naInx = which(is.na(obsDat[, 2])))
  
  # Remove NA rows
  obsDat <- obsDat[-nas$naInx, ]
  
  # Write to csv
  write.csv(obsDat, 'C:/siletz/pest/pest_obsdat.csv', row.names = FALSE)
  
  # write.csv(nas, 'C:/siletz/nas.csv', row.names = FALSE)

}


