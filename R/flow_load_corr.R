flow_load_corr <- function(pars, datM, n) {
  
  library('ggplot2')
  library('stats')
  
  plain <- function(x) {format(x, scientific = FALSE, trim = TRUE)}
  
  # ALL DATA ----
  plt <- ggplot(data = datM) +
    geom_point(aes(x = QM, y = LM), size = 0.25, color = 'darkblue') +
    geom_point(aes(x = QO, y = LO), size = 1.2, shape = 23, color = 'darkred',
               stroke = 1.0, fill = 'yellow') +
    xlab('Flow (cfs)') + ylab('Load (ton/day)') + 
    scale_x_log10(labels = plain) + scale_y_log10(labels = plain)
  
  ggsave(paste0(pars, '_flow_v_load_', n, '.png'), plot = plt, width = 10, height = 7.5, 
         path = 'D:/siletz/calib/wq/plots', units = 'in', dpi = 300)
  
  plt <- ggplot(data = datM) +
    geom_point(aes(x = QM, y = CM), size = 0.25, color = 'darkblue') +
    geom_point(aes(x = QO, y = CO), size = 1.2, shape = 23, color = 'darkred',
               stroke = 1.0, fill = 'yellow') +
    xlab('Flow (cfs)') + ylab('Concentration (ton/day)') + 
    scale_x_log10(labels = plain) + scale_y_log10(labels = plain)
  
  ggsave(paste0(pars, '_flow_v_conc_', n, '.png'), plot = plt, width = 10, height = 7.5, 
         path = 'D:/siletz/calib/wq/plots', units = 'in', dpi = 300)
  
  # PAIRED DATA ----
  datMCC <- datM[complete.cases(datM), 3 : 8]
  
  maxL <- 10^floor(log10(max(max(datMCC$LO), max(datMCC$LM))))
  
  maxC <- 10^floor(log10(max(max(datMCC$CO), max(datMCC$CM))))

  L121 <- data.frame(x = c(0, maxL),
                     y = c(0, maxL))
  
  C121 <- data.frame(x = c(0, maxC),
                     y = c(0, maxC))
  
  plt <- ggplot(data = datMCC) +
    geom_point(aes(x = LO, y = LM), size = 1.2, shape = 23, color = 'darkred',
               stroke = 1.0, fill = 'yellow') +
    geom_line(data = L121, aes(x = x, y = y)) + 
    xlab('Observed Loads (tons/day)') + ylab('Modeled Loads (ton/day)') + 
    scale_x_log10(labels = plain) + scale_y_log10(labels = plain)

  ggsave(paste0(pars, '_obsmod_loads_', n, '.png'), plot = plt, width = 10,
         height = 7.5, path = 'D:/siletz/calib/wq/plots', units = 'in', dpi = 300)
  
  plt <- ggplot(data = datMCC) +
    geom_point(aes(x = CO, y = CM), size = 1.2, shape = 23, color = 'darkred',
               stroke = 1.0, fill = 'yellow') +
    geom_line(data = C121, aes(x = x, y = y)) + 
    xlab('Observed Loads (tons/day)') + ylab('Modeled Concentrations (mg/L)') + 
    scale_x_log10(labels = plain) + scale_y_log10(labels = plain)
  
  ggsave(paste0(pars, '_obsmod_concs_', n, '.png'), plot = plt, width = 10,
         height = 7.5, path = 'D:/siletz/calib/wq/plots', units = 'in', dpi = 300)
  
}
