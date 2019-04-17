qlc_corr <- function(pars, datM, n) {
  
  suppressMessages(library('ggplot2'))
  suppressMessages(library('stats'))
  suppressMessages(library('dplyr'))
  
  plain <- function(x) {format(x, scientific = FALSE, trim = TRUE)}
  
  # ALL DATA ----
  plt <- ggplot(data = datM) +
         geom_point(aes(x = QM, y = LM), size = 0.25, color = 'darkblue') +
         geom_point(aes(x = QO, y = LO), size = 1.2, shape = 23, color = 'darkred',
                    stroke = 1.0, fill = 'yellow') +
         xlab('Flow (cfs)') + ylab('Load (ton/day)') + 
         scale_x_log10(labels = plain, limits = c(30, 10000)) +
         scale_y_log10(labels = plain, limits = c(1e-3, 2.0)) + theme_bw()
         # scale_y_continuous(labels = plain) + theme_bw()
    
  ggsave(paste0(pars, '_flow_v_load_', n, '.png'), plot = plt, width = 12,
         height = 6, path = 'D:/siletz/calib/wq/plots', units = 'in', dpi = 300)
  
  plt <- ggplot(data = datM) +
         geom_point(aes(x = QM, y = CM), size = 0.25, color = 'darkblue') +
         geom_point(aes(x = QO, y = CO), size = 1.2, shape = 23, color = 'darkred',
                    stroke = 1.0, fill = 'yellow') +
         xlab('Flow (cfs)') + ylab('Concentration (mg/L)') + 
         scale_x_log10(labels = plain, limits = c(30, 10000)) +
         scale_y_log10(labels = plain, limits = c(0.002, 0.20)) + theme_bw()
         # scale_y_continuous(labels = plain) + theme_bw()
  
  ggsave(paste0(pars, '_flow_v_conc_', n, '.png'), plot = plt, width = 12,
         height = 6, path = 'D:/siletz/calib/wq/plots', units = 'in', dpi = 300)
  
  # PAIRED DATA ----
  datMCC <- datM[complete.cases(datM), 3 : 8]
  
  # Linear regressions
  fL <- summary(lm(LM ~ LO, data = datMCC)); fC <- summary(lm(CM ~ CO, data = datMCC))
  
  # Paired load/conc, Adj R2, median residual
  qlcCorr <- list(Loads = c(fL$coefficients[2, 1], fL$adj.r.squared, median(fL$residuals)),
                  Concs = c(fC$coefficients[2, 1], fC$adj.r.squared, median(fC$residuals)))
  

  # maxL <- floor(max(max(datMCC$LO), max(datMCC$LM)))
  # maxC <- floor(max(max(datMCC$CO), max(datMCC$CM)))
  
  maxL <- 10^ceiling(log10(max(max(datMCC$LO), max(datMCC$LM))))

  maxC <- 10^ceiling(log10(max(max(datMCC$CO), max(datMCC$CM))))

  L121 <- data.frame(x = c(0, maxL),
                     y = c(0, maxL))
  
  C121 <- data.frame(x = c(0, maxC),
                     y = c(0, maxC))
  
  plt <- ggplot(data = datMCC) +
         geom_point(aes(x = LO, y = LM), size = 1.2, shape = 23, color = 'darkred',
                    stroke = 1.0, fill = 'yellow') +
         geom_line(data = L121, aes(x = x, y = y)) + 
         xlab('Observed Loads (tons/day)') + ylab('Modeled Loads (ton/day)') + 
         scale_x_log10(labels = plain, limits = c(1e-4, 10.0)) +
         scale_y_log10(labels = plain, limits = c(1e-4, 10.0)) + theme_bw()
         # scale_y_continuous(labels = plain) + theme_bw()

  ggsave(paste0(pars, '_obsmod_loads_', n, '.png'), plot = plt, width = 6,
         height = 6, path = 'D:/siletz/calib/wq/plots', units = 'in', dpi = 300)
  
  plt <- ggplot(data = datMCC) +
         geom_point(aes(x = CO, y = CM), size = 1.2, shape = 23, color = 'darkred',
                    stroke = 1.0, fill = 'yellow') +
         geom_line(data = C121, aes(x = x, y = y)) + 
         xlab('Observed Concentrations (mg/L)') + ylab('Modeled Concentrations (mg/L)') + 
         scale_x_log10(labels = plain, limits = c(1e-3, 1.0)) +
         scale_y_log10(labels = plain, limits = c(1e-3, 1.0)) + theme_bw()
         # scale_y_continuous(labels = plain) + theme_bw()
  
  ggsave(paste0(pars, '_obsmod_concs_', n, '.png'), plot = plt, width = 6,
         height = 6, path = 'D:/siletz/calib/wq/plots', units = 'in', dpi = 300)

  return(qlcCorr)

}
