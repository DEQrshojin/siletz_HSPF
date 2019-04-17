seasonal_wq_conc <- function(wqDF = NULL, par = NULL, lo = 0.10, hi = 0.90,
                             ts = 'none') {
  
  suppressMessages(library(TSA)); suppressMessages(library(ggplot2));
  suppressMessages(library(dplyr)); suppressMessages(library(lubridate))
  suppressMessages(library(reshape2))

  sapply(paste0('C:/Users/rshojin/Desktop/006_scripts/github/General/',
                c('day_of_hydro_year.R', 'hydro_year.R')), source)

  # ORGANIZE DATA ----
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
  
    plt <- ggplot(mdDF, aes(x = doy)) +
           geom_line(aes(y = ifw), color = 'darkblue') +
           geom_line(aes(y = agw), color = 'darkred') +
           geom_point(data = wqDF, aes(x = doy, y = C_mgL), size = 1.2, shape = 2,
                      stroke = 1.2, color = 'darkred', fill = 'yellow')
  
    ggsave(paste0('seasonal_', par, '.png'), plot = plt, width = 10,
           height = 7.5, path = 'D:/siletz/calib/wq/seasonal', units = 'in',
           dpi = 300)
  
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
           height = 7.5, path = 'D:/siletz/calib/wq/seasonal', units = 'in',
           dpi = 300)
    
  }

  return(fit)
  
}
