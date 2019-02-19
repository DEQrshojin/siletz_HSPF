#! C:/Program Files/R/R-3.5.2/bin Rscript

# FUNCTION TO WRITE MODEL OUTPUT TO PEST OUT FILE
# Ryan Shojinaga, Water Quality Analyst, NRS3, Oregon DEQ
# shojinaga.ryan@deq.state.or.us, 503-229-5777

suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))
suppressMessages(library(reshape2))

options(warn = -1)

filPath <- 'C:/siletz/'

# Read the counter
countFil = file(paste0(filPath, 'count.txt'))

n = as.numeric(readLines(countFil))

# IMPORT AND PREP MODEL DATA ----
# Create data frame for pest control file observation data
modDat <- data.frame(obsNme = as.character(),
                     value = as.numeric(),
                     stringsAsFactors = FALSE)

# Load flow data and NA indeces
qData <- read.csv('C:/siletz/calib/qmod.csv', stringsAsFactors = FALSE)

qData$Date <- as.POSIXct(qData$Date, '%Y-%m-%d %H:%M:%S',
                         tz = 'America/Los_Angeles')

nas <- read.csv('C:/siletz/pest/nas.csv', stringsAsFactors = FALSE)

# Remove the last entry (2017-10-01 00:00)
qData <- qData[-nrow(qData), ]

# MEAN DAILY FLOWS - qdaysz & qdaysn ----
qData$Date <- as.Date(qData$Date, '%Y-%m-%d %H:%M:%S',
                      tz = 'America/Los_Angeles')

qData <- melt(qData, id.vars = 'Date', value.name = 'qcfs',
              variable.name = 'Basin')

qData <- dcast(qData, Date ~ Basin, fun.aggregate = mean, value.var = 'qcfs')

qData <- qData[complete.cases(qData), ]

qData <- qData[, c(1, 3, 2)]

names(qData) <- c('Date', 'qSlz', 'qSun')

# Make model data NAs from gage data NAs
dlyNA <- read.csv('C:/siletz/pest/dlyNA.csv', stringsAsFactors = FALSE)

gage <- c('Slz', 'Sun')

for (i in 1 : 2) {
  
  NAs <- dlyNA[dlyNA$gage == gage[i], 3]
  
  qData[NAs, i + 1] <- NA
  
}

# SET DATE COMPONENTS
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

qData$qlgSlz <- log10(qData$qSlz)

qData$qlgSun <- log10(qData$qSun)

# Add the data for Siletz log daily flows
tmp <- qData[, c(8, 10)]

names(tmp) <- c('obsNme', 'value')

modDat <- rbind(modDat, tmp)

# Add the data for Sunshine log daily flows
tmp <- qData[, c(9, 11)]

names(tmp) <- c('obsNme', 'value')

modDat <- rbind(modDat, tmp)

# MONTHLY VOLUMES - vmonsz & vmonsn ----
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

# Add the data for Siletz monthly flow volumnes
tmp <- datMnt[, c(5, 2)]

names(tmp) <- c('obsNme', 'value')

modDat <- rbind(modDat, tmp)

# Add the data for Sunshine monthly flow volumnes
tmp <- datMnt[, c(6, 3)]

names(tmp) <- c('obsNme', 'value')

modDat <- rbind(modDat, tmp)

# DRY SEASON VOLUMES (Jul-Sep) - vdrysz & vdrysn ----
datDry <- datMnt[(datMnt$Mn >= 7 & datMnt$Mn <= 9), ]

datDry$Yr <- year(datDry$Date)

datDrySlz <- aggregate(datDry$qSlz, by = list(datDry$Yr), 'sum')

datDrySun <- aggregate(datDry$qSun, by = list(datDry$Yr), 'sum')

datDry <- merge(datDrySlz, datDrySun, by.x = 'Group.1', by.y = 'Group.1')

names(datDry) <- c('Date', 'Slz', 'Sun')

datDry[12, 3] <- NA

datDry$nmSlz <- paste0('vdrysz', datDry$Date)

datDry$nmSun <- paste0('vdrysn', datDry$Date)

# Add the data for Siletz dry season flow volumnes
tmp <- datDry[, c(4, 2)]

names(tmp) <- c('obsNme', 'value')

modDat <- rbind(modDat, tmp)

# Add the data for Sunshine dry season flow volumnes
tmp <- datDry[, c(5, 3)]

names(tmp) <- c('obsNme', 'value')

modDat <- rbind(modDat, tmp)

# ANNUAL VOLUMES - vannsz & vannsn ----
datYr <- aggregate(qData[, 2 : 3], by = list(qData$HY), sum,
                   na.rm = TRUE)

colnames(datYr)[1] <- 'Date'

# Convert to 1000 x AF (1 cfs = 1.98347 AF/day)
datYr[, 2 : 3] <- datYr[, 2 : 3] * 1.98347 / 1000

datYr$nmSlz <- paste0('vannsz', datYr$Date)

datYr$nmSun <- paste0('vannsn', datYr$Date)

# Add the data for Siletz annual flow volumnes
tmp <- datYr[, c(4, 2)]

names(tmp) <- c('obsNme', 'value')

modDat <- rbind(modDat, tmp)

# Add the data for Sunshine annual flow volumnes
tmp <- datYr[, c(5, 3)]

names(tmp) <- c('obsNme', 'value')

modDat <- rbind(modDat, tmp)

# 5th PERCENTILE EXCEEDENCE VOLUMES - v05psz & v05psn ----
# Storm (Upper 5% flows)
q05Slz <- quantile(qData$qSlz, 0.95, na.rm = TRUE)

q05Sun <- quantile(qData$qSun, 0.95, na.rm = TRUE)

q05Slz <- qData[qData$qSlz >= q05Slz, c(1 : 2)]

q05Sun <- qData[qData$qSun >= q05Sun, c(1, 3)]

q05Slz <- q05Slz[complete.cases(q05Slz), ]

q05Sun <- q05Sun[complete.cases(q05Sun), ]

# Convert to 1000 x ac-ft  
q05Slz[, 2] <- q05Slz[, 2] * 1.98347 / 1000

q05Sun[, 2] <- q05Sun[, 2] * 1.98347 / 1000

q05Tot <- data.frame('vol05pct' = c(sum(q05Slz[, 2], na.rm = TRUE),
                                    sum(q05Sun[, 2], na.rm = TRUE)),
                     'v05pnm' = c('v05psz001', 'v05psn001'),
                     'v05grp' = c('v05psz', 'v05psn'))

tmp <- q05Tot[, c(2, 1)]

names(tmp) <- c('obsNme', 'value')

modDat <- rbind(modDat, tmp)

# FLOW DURATION CURVES - fdcqsz & fdcqsn ----
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

# Add the data for Siletz flow duration curve percentiles
tmp <- flwDur[, c(5, 3)]

names(tmp) <- c('obsNme', 'value')

modDat <- rbind(modDat, tmp)

# Add the data for Siletz flow duration curve percentiles
tmp <- flwDur[, c(6, 4)]

names(tmp) <- c('obsNme', 'value')

modDat <- rbind(modDat, tmp)

# TIDY UP DATA AND OUTPUT TO MODEL.OUT ----
# Find index and name of NAs
modDat <- modDat[-nas$naInx, ]

modDat$length <- 20 - sapply(modDat$obsNme, nchar) - 1

modDat$length <- paste0('%', modDat$length, 's')

modDat$pre <- sapply(modDat$length, sprintf, ' ')

modDat$prelen <- sapply(modDat$pre, nchar)

modDat$line <- paste0(' ', modDat$obsNme, modDat$pre, modDat$value)

write.table(modDat$line, 'C:/siletz/model.out', row.names = FALSE,
            col.names = FALSE, quote = FALSE)

source('C:/siletz/scripts/R/move_hspf_files.R')

# Move the output files to storage folders
move_hspf_files(filPath, n)

# Update the run number and write back to the file
n = n + 1

writeLines(as.character(n), countFil)

close(countFil)