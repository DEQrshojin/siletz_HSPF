# Script to extract peak storm flow dates
library(ggplot2)
library(lubridate)

qData <- read.csv('C:/siletz/calib/gge.csv', stringsAsFactors = FALSE)

qData$Date <- as.Date(qData$Date, '%Y-%m-%d')

pct <- 0.25

n = 150

# SELECTED STORMS FOR SILETZ ----
qSlz <- qData[, 1 : 2]

slzStm <- storm_dates(qSlz, pct, n)

qSlz <- add_date_info(qSlz)

slzStm <- add_date_info(slzStm)

pltSlz <- ggplot(data = qSlz, aes(x = doy, y = q), group = hy) + geom_line() + 
          geom_point(data = slzStm, aes(x = doy, y = q), size = 2, color = 2) +
          scale_y_log10() + facet_wrap(~hy, ncol = 3)

ggsave('selected_storms_siletz.png', plot = pltSlz, path = 'C:/siletz/calib',
       width = 15, height = 10, units = 'in', dpi = 300)

# SELECTED STORMS FOR SUNSHINE ----
qSun <- qData[, c(1, 3)]

sunStm <- storm_dates(qSun, pct, n)

qSun <- add_date_info(qSun)

sunStm <- add_date_info(sunStm)

pltSun <- ggplot(data = qSun, aes(x = doy, y = q), group = hy) + geom_line() + 
  geom_point(data = sunStm, aes(x = doy, y = q), size = 2, color = 2) +
  scale_y_log10() + facet_wrap(~hy, ncol = 3)

ggsave('selected_storms_sunshine.png', plot = pltSun, path = 'C:/siletz/calib',
       width = 15, height = 10, units = 'in', dpi = 300)

# output to csv ----
# add observation name
slzStm$name <- paste0('qstmsz',
                      ifelse(year(slzStm$dt) - 2000 < 10, 0, ''),
                      year(slzStm$dt) - 2000,
                      ifelse(month(slzStm$dt) < 10, 0, ''), month(slzStm$dt),
                      ifelse(day(slzStm$dt) < 10, 0, ''), day(slzStm$dt))
  
sunStm$name <- paste0('qstmsn',
                      ifelse(year(sunStm$dt) - 2000 < 10, 0, ''),
                      year(sunStm$dt) - 2000,
                      ifelse(month(sunStm$dt) < 10, 0, ''), month(sunStm$dt),
                      ifelse(day(sunStm$dt) < 10, 0, ''), day(sunStm$dt))

# add weight y = 0.9453 / x, based on Big Elk Creek storm weighting
slzStm$weight <- 0.9453 / slzStm$q

sunStm$weight <- 0.9453 / sunStm$q * 0.03 # scale to area contribution

# add group
slzStm$group <- 'qstmsz'

sunStm$group <- 'qstmsn'

# subset on required components (name, value, weight, group)
dfOut <- rbind(slzStm, sunStm)

dfOut <- dfOut[, c(5, 2, 6, 7)]

# output to csv
write.csv(dfOut, file = 'C:/siletz/pest/stmObs.csv', quote = FALSE,
          row.names = FALSE)

# Auxillary functions ----
storm_dates <- function(df, pct, n) {
  
  names(df) <- c('dt', 'q')
                 
  df$roc <- 0

  for (i in 1 : nrow(df)) {
    
    if (i == nrow(df)) {
      
      df[i, 3] <- 0
      
    } else {
      
      df[i, 3] <- df[i + 1, 2] - df[i, 2]
      
    }
  }
  
  # Top 10% = 121 storm events
  qStrPct <- quantile(df[df$roc > 0, 3], pct, na.rm = TRUE)
  
  strInd <- which(df$roc > qStrPct) + 1
  
  qStm <- df[strInd, ]
  
  qStm <- qStm[complete.cases(qStm), ]
  
  if (n > nrow(qStm)) {n = nrow(qStm)}
  
  # RANDOMLY SELECT SUBSET
  randStm <- sample(c(1 : nrow(qStm)), n, replace = FALSE)
  
  qStm <- qStm[randStm, 1 : 2]
  
  names(qStm) <- c('Date', 'Qcfs')
  
  # Reorder the DF by date
  qStm <- qStm[order(qStm$Date), ]

  return(qStm)

}

add_date_info <- function(df) {
  
  names(df) <- c('dt', 'q')
  
  df$yr <- year(df$dt) # Add calendar year
  
  df$hy <- ifelse(month(df$dt) >= 10, df$yr + 1, df$yr) # Add hydro year
  
  df$phy <- df$hy - 1
  
  df$mn <- month(df$dt)
  
  df$doy <- 0 # Add day of year
  
  df$boy <- paste0(df$phy, '-10-01')
  
  df$boy <- as.Date(df$boy, '%Y-%m-%d')
  
  df$doy <- as.numeric(df$dt - df$boy) + 1
  
  df <- df[, -c(3, 5, 6, 8)]
  
  return(df)
  
}
