rm(list = ls()); cat('\014')

# LIBRARIES, SCRIPTS, OPTIONS 
options(stringsAsFactors = FALSE, warn = -1)
suppressMessages(library('lubridate')); suppressMessages(library('ggplot2'))
suppressMessages(library('reshape2'))

# ANALYZE ERROR ----
data <- read.csv('C:/siletz_tmdl/05_misc/data/watershed_model_WQ_error_data.csv')

data$dte <- as.POSIXct(data$dte, '%m/%d/%Y', tz = 'America/Los_Angeles')

data$doy <- yday(data$dte)

data$mnth <- month(data$dte)

# Model underprediction = Positive residual 
data$err <- data$obs - data$mod

cwDt <- yday(as.POSIXct(c('2017-06-16', '2017-08-31'), '%Y-%m-%d',
                        tz = 'America/Los_Angeles'))

data$seas <- ifelse(data$doy >= cwDt[1] & data$doy <= cwDt[2],
                    'Cold-water', 'Spawning')

data$par <- factor(data$par, levels(factor(data$par))[c(2, 6, 1, 4, 5, 3)])

# Go with 90th percentile of the seasonal data
pars <- unique(data$par)

for (i in 1 : length(pars)) {

  tmp1 <- data[which(data$par == pars[i] & data$mnth %in% 7 : 10), ]

  tmp2 <- data.frame(par = pars[i],
                     cw  = quantile(x = tmp1$err[which(tmp1$seas == 'Cold-water')], 0.9),
                     sp  = quantile(x = tmp1$err[which(tmp1$seas == 'Spawning')], 0.9),
                     stringsAsFactors = F)

  if (i == 1) {resd <- tmp2} else {resd <- rbind(resd, tmp2)}

}

# Season, error, par
resd <- melt(resd, id.vars = 'par', variable.name = 'seas', value.name = 'err')

resd$seas <- ifelse(resd$seas == 'cw', 'Cold-water', 'Spawning')

resd$lbl <- paste0('90th %-ile\nresidual:\n', round(resd$err, 3), ' mg/L')

pl <- ggplot(data[which(data$mnth %in% 7 : 10), ],
             aes(x = seas, y = err, group = seas)) +
      geom_boxplot() + geom_jitter() +
      geom_point(data = resd, aes(x = seas, y = err), size = 1.5, shape = 23,
                 color = 'blue', fill = 'yellow', stroke = 1.2) +
      geom_text(data = resd, aes(x = seas, y = err, label = lbl),
                size = 3, color = 'blue', hjust = 0, vjust = 0.5, 
                nudge_x = 0.05) +
      ylab('Nutrient residuals (mg/L)') + theme_classic() +
      facet_wrap(.~ par, ncol = 2, scales = 'free_y') +
      geom_hline(yintercept = 0, linetype = 'dashed', size = 0.6) +
      theme(axis.title.x = element_blank())
  

ggsave(filename = 'C:/siletz_misc/memo/figures/v2/fig_05_nutrient_residuals.png',
       plot = pl, width = 11, height = 8.5, dpi = 300, units = 'in')

#

# SCRATCH ----
# suppressMessages(library('reshape2')); suppressMessages(library('dplyr'))
# source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_wq_run_hspf.R')
# source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_utilities_hspf.R')
# source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_wq_calib_hspf.R')

# PROCESS DATA FOR ANALYSIS 
# # COUNTER 
# ctrF <- read_ctrF_H(); fNme <- ctrF$name
# 
# # READ IN WQ CONTROL FILE VARIABLES (also writes to wq parm csv) 
# wqFl <- paste0('C:/siletz_tmdl/03_models/01_hspf/wq_pars/wq_pars_',
#                c('NOx', 'NH3', 'TKN', 'TP', 'PO4', 'OrC'), '.csv')
# 
# for (i in 1 : 6) {
#   
#   v <- read_wq_pars(cf = wqFl[i], filN = fNme, writeCsv = F)
#   
#   pars = v$pars
#   
#   stns = v$stns
# 
#   # LOAD WQ OBS DATA - Model flows/loads/conc 
#   rchQLC <- readRDS(paste0('C:/siletz_tmdl/02_outputs/01_hspf/base_rchQLC_',
#                            pars, '.RData')) 
# 
#   # aggregate concentrations in to daily mean values 
#   datM <- aggregate(rchQLC[[4]][, c('Date', 'Bas14')],
#                     by = list(floor_date(rchQLC[[4]]$Date, 'day')),
#                     FUN = 'mean')
# 
#   wqDt <- read.csv(paste0('C:/siletz_tmdl/01_inputs/01_hspf/amb_wq/', pars, '_',
#                           stns, '.csv'), stringsAsFactors = FALSE)
#   
#   wqDt$Date <- as.POSIXct(wqDt$Date, '%Y-%m-%d', tz = 'America/Los_Angeles')
# 
#   # Merge model and observation data
#   datM <- merge(wqDt[, c(1, 2)], datM[, c(1, 3)], by.x = 'Date', by.y = 'Group.1',
#                 all.x = F, all.y = F)
#   
#   datM$par <- pars
#   
#   if (i == 1) {data <- datM} else {data <- rbind(data, datM)}
# 
# }
# 
# names(data) <- c('dte', 'obs', 'mod', 'par')
# 
# write.csv(data, 'C:/siletz_tmdl/05_misc/data/watershed_model_WQ_error_data.csv')


# pl <- ggplot(data, aes(doy, err)) + ylab('Residuals (mg/L)') + theme_classic() +
#       facet_wrap(.~ par, ncol = 2, scales = 'free_y') + 
#       geom_point(color = 'darkred', fill = 'yellow',
#                  size = 1.2, shape = 23, stroke = 1.0) +
#       geom_hline(yintercept = 0, linetype = 'dashed', size = 0.6) +
#       scale_x_continuous(breaks = c(1, 32, 60, 91, 121, 152, 182,
#                                     213, 244, 274, 305, 335),
#                          labels = c('1' = 'J', '32' = 'F', '60' = 'M',
#                                     '91' = 'A', '121' = 'M', '152' = 'J',
#                                     '182' = 'J', '213' = 'A', '244' = 'S',
#                                     '274' = 'O', '305' = 'N', '335' = 'D'))
# 
# ggsave(filename = 'C:/siletz_tmdl/05_misc/data/watershed_model_WQ_residuals_DOY.png',
#        plot = pl, width = 11, height = 8.5, dpi = 300, units = 'in')
# 
# pl <- ggplot(data, aes(x = mnth, y = err, group = mnth)) + ylab('Residuals (mg/L)') +
#       theme_classic() + facet_wrap(.~ par, ncol = 2, scales = 'free_y') + 
#       geom_boxplot() + geom_jitter() + 
#       geom_hline(yintercept = 0, linetype = 'dashed', size = 0.6) +
#       scale_x_continuous(breaks = c(1 : 12),
#                          labels = c('J', 'F', 'M', 'A', 'M', 'J',
#                                     'J', 'A', 'S', 'O', 'N', 'D'))
# 
# ggsave(filename = 'C:/siletz_tmdl/05_misc/data/watershed_model_WQ_residuals_boxpl_mnth.png',
#        plot = pl, width = 11, height = 8.5, dpi = 300, units = 'in')

# data$pctR <- 100 * (data$err / data$obs)

# Go with 90th percentile of the seasonal data
# pars <- unique(data$par)
# 
# for (i in 1 : length(pars)) {
#   
#   tmp1 <- data[which(data$par == pars[i] & data$mnth %in% 7 : 10), ]
#   
#   tmp2 <- data.frame(par = pars[i],
#                      cw  = quantile(x = tmp1$pctR[which(tmp1$seas == 'cw')], 0.9),
#                      sp  = quantile(x = tmp1$pctR[which(tmp1$seas == 'sp')], 0.9),
#                      stringsAsFactors = F)
#   
#   if (i == 1) {resd <- tmp2} else {resd <- rbind(resd, tmp2)}
#   
# }
# 
# write.csv(resd, 'C:/siletz_tmdl/05_misc/data/watershed_model_90th_pct_residual_pct_diff.csv', row.names = F)
