seasonal_wq_conc <- function(wqDF = NULL, par = NULL, trms = 1) {

  # Creates a harmonic function fit of water quality data to seasonal variation
  
  library(TSA); library(ggplot2); library(dplyr)
  
  sapply(paste0('C:/Users/rshojin/Desktop/006_scripts/github/General/',
                c('day_of_hydro_year.R', 'hydro_year.R')), source)
  
  names(wqDF) <- c('Date', 'C_mgL')
  
  # Create the data frame with the function parameters (periodicity)
  wqDF <- wqDF %>%
          mutate(hyr = hydro_year(Date), doy = day_of_hydro_year(Date)) %>%
          mutate(dys = ifelse((hyr %% 4) == 0, 366, 365), yr = hyr - hyr[1]) %>%
          mutate(per = yr + round(doy / dys, 3))

  # Fit the data to harmonic function
  if (is.null(trms) | trms == 1) {
    
    fit <- lm(C_mgL ~ sin(2 * pi * per) + cos(2 * pi * per), data = wqDF)
    
  } else if (trms == 2) {
    
    fit <- lm(C_mgL ~ sin(2 * pi * per) + cos(2 * pi * per) + 
                      sin(4 * pi * per) + cos(4 * pi * per), data = wqDF)
    
  } else if (trms == 3) {
    
    fit <- lm(C_mgL ~ sin(2 * pi * per) + cos(2 * pi * per) + 
                      sin(4 * pi * per) + cos(4 * pi * per) + 
                      sin(6 * pi * per) + cos(6 * pi * per), data = wqDF)
    
  } else {
    
    print('please specify number of terms for the function (1, 2, or 3)')
    
    return(NULL)
    
  }

  # Create the functional parameters (periodicity) for the fitted data
  mdDF <- data.frame(Date = seq(wqDF$Date[1], wqDF$Date[nrow(wqDF)], 1))
  
  mdDF <- mdDF %>%
          mutate(hyr = hydro_year(Date), doy = day_of_hydro_year(Date)) %>%
          mutate(dys = ifelse((hyr %% 4) == 0, 366, 365), yr = hyr - hyr[1]) %>%
          mutate(per = yr + round(doy / dys, 3))
  
  # Create a time series of the modeled data for plotting
  mdDF$fit <- fit$coefficients[2] * (sin(2 * pi * mdDF$per)) + 
              fit$coefficients[3] * (cos(2 * pi * mdDF$per)) + 
              fit$coefficients[1]
  
  if (trms == 2) {
    
    mdDF$fit <- mdDF$fit + fit$coefficients[4] * (sin(4 * pi * mdDF$per)) + 
                           fit$coefficients[5] * (cos(4 * pi * mdDF$per)) 

  } else if (trms == 3) {
    
    mdDF$fit <- mdDF$fit + fit$coefficients[4] * (sin(4 * pi * mdDF$per)) + 
                           fit$coefficients[5] * (cos(4 * pi * mdDF$per)) +
                           fit$coefficients[6] * (cos(5 * pi * mdDF$per)) +
                           fit$coefficients[7] * (cos(5 * pi * mdDF$per))

  }
  
  # Plot and save the data
  plt <- ggplot() +
         geom_point(data = wqDF, aes(x = doy, y = C_mgL), size = 1.2, shape = 2,
                    stroke = 1.2, color = 'darkred', fill = 'yellow') +
         # geom_line(data = wqDF, aes(x = doy, y = C_mgL, color = hyr, linetype = 'dashed')) + 
         geom_line(data = mdDF, aes(x = doy, y = fit), size = 1.2, color = 'darkblue') +
         scale_x_continuous(breaks = seq(1, 366, 15))
  
  ggsave(paste0('seasonal_', par, '.png'), plot = plt, width = 10, height = 7.5,
         path = 'D:/siletz/calib/wq/seasonal', units = 'in', dpi = 300)
  
  # Isolate the return variables
  return(fit)

}