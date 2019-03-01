# LOAD, MERGE, AND PROCESS DATA ----
path <- paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/',
               'Middle_Siletz_River_1710020405/001_data/flow_data/')
  
qSlz <- readLines(paste0(path, 'USGS_Sta5500_Sil_InstQ_1086_0618.txt'))

# qSun <- readLines(paste0(path, 'OWRD_Sta4350_Sun_MeanQ_1072_1018.txt'))

qSlz <- data.frame(do.call("rbind", strsplit(qSlz, '\t')),
                   stringsAsFactors = FALSE)

# qSun <- data.frame(do.call("rbind", strsplit(qSun, '\t')), 
#                    stringsAsFactors = FALSE)

qSlz <- qSlz[, c(3, 5)]

# qSun <- qSun[, c(2, 3)]

# qSun <- qSun[-1, ]

names(qSlz) <- c('Date', 'Qcfs')

qSlz$Date <- as.POSIXct(qSlz$Date, '%Y-%m-%d %H:%M',
                        tz = 'America/Los_Angeles')

# qSun$Date <- as.Date(qSun$Date, '%m-%d-%Y')

qSlz$Qcfs <- as.numeric(qSlz$Qcfs)

# qSun$Qcfs <- as.numeric(qSun$Qcfs)

qSlz$Date2 <- as.Date(qSlz$Date)

qSlzDly <- aggregate(qSlz$Qcfs, by = list(qSlz$Date2), FUN = 'mean',
                     na.rm = TRUE)

colnames(qSlzDly)[1] <- 'Date'

ts <- as.Date(c('1990-10-01', '2018-09-30'), '%Y-%m-%d')

ts <- data.frame('Date' = seq(ts[1], ts[2], 1), stringsAsFactors = FALSE)

# snEx <- as.Date(c('2007-11-25', '2009-08-19', '2015-08-31', '2016-06-07'),
#                 '%Y-%m-%d')
# 
# qSun <- qSun[which((qSun$Date >= ts[1, ] & qSun$Date <= snEx[1]) |
#                    (qSun$Date >= snEx[2] & qSun$Date <= snEx[3]) |
#                    (qSun$Date >= snEx[4] & qSun$Date <= ts[nrow(ts), ])), ]

qData <- merge(ts, qSlzDly, by.x = 'Date', by.y = 'Date',
               all.x = TRUE, all.y = FALSE)

# qData <- merge(qData, qSun, by.x = 'Date', by.y = 'Date',
#                all.x = TRUE, all.y = FALSE)

names(qData) <- c('Date', 'qSlz')

write.csv(qData, file = 'D:/siletz_tmp/scratch/wq/slz_qDaily.csv',
          row.names = FALSE)

