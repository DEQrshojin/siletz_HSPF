#! d:/Program Files/R/R-3.5.2/bin Rscript

# FUNCTION TO ANALYZE HSPF SEDIMENT TRANSPORT
# Ryan Shojinaga, Water Quality Analyst, NRS3, Oregon DEQ
# shojinaga.ryan@deq.state.or.us, 503-229-5777

suppressMessages(library(ggplot2))
suppressMessages(library(reshape2))
suppressMessages(library(scales))
suppressMessages(library(stats))

options(scipen = -1, stringsAsFactors = FALSE, warn = -1)

for (q in 1) {

  filPath <- 'D:/siletz/'
  
  # Read counter
  countFil = file('D:/siletz/count.txt')
  
  n = as.numeric(readLines(countFil))

  # Read in data
  wqObs <- read.csv('D:/siletz_tmp/scratch/wq/sediment/sediment_STA10391.csv',
                    stringsAsFactors = FALSE)
  
  wqMod <- read.csv('D:/siletz/calib/siletz_out_sed.csv',
                    stringsAsFactors = FALSE)
  
  # Dates
  wqObs$date <- as.Date(wqObs$date, '%Y-%m-%d')
  
  colnames(wqMod)[1] = 'datetime'
  
  wqMod$datetime <- as.POSIXct(wqMod$datetime, '%Y-%m-%d %H:%M:%S',
                               tz = 'America/Los_Angeles')
  
  wqMod$date <- as.Date(wqMod$datetime, '%Y-%m-%d %H:%M:%S',
                        tz = 'America/Los_Angeles')

  # Isolate the results from Basin 14 (col# 15, 32, 49)
  # wqMod <- wqMod[-nrow(wqMod), c(1, 5, 2, 3, 4)]
  wqMod <- wqMod[-nrow(wqMod), c(1, 53, 15, 32, 49)]
  
  # Convert flow to cfs
  wqMod$BAS_14_RO <- wqMod$BAS_14_RO * 35.314666721
  
  names(wqMod) <- c('datetime', 'date', 'q_cfs', 'tss_mgL', 'sst_t')
  
  # Aggregate to dailys
  a <- aggregate(wqMod$q_cfs, by = list(wqMod$date), FUN = 'mean')
  b <- aggregate(wqMod$tss_mgL, by = list(wqMod$date), FUN = 'mean')
  c <- aggregate(wqMod$sst_t, by = list(wqMod$date), FUN = 'mean')
  
  wqMod <- data.frame(date = a$Group.1, qcfs = a$x, tssc = b$x, sedb = c$x,
                      stringsAsFactors = FALSE)
  
  wqObs <- wqObs[, c(1 : 3, 17, 10, 18, 19)]
  
  # Combine the model and observations
  wqAll <- merge(wqObs, wqMod,
                 by.x = 'date', by.y = 'date',
                 all.x = TRUE, all.y = FALSE)
  
  wqAll <- wqAll[complete.cases(wqAll), ]
  
  names(wqAll) <- c('date', 'uid', 'dt', 'cobs', 'opr', 'qobs', 'lobs',
                    'qmod', 'cmod', 'bmod')
  
  # Convert model flows and concentrations to daily loads
  wqAll$lmod <- wqAll$cmod * wqAll$qmod * 28.317 * 86400 * 1.10e-9
  
  wqAll <- wqAll[, c(2, 1,  # UID and date
                     4, 9,  # Concentration
                     7, 11, # Loads
                     6, 8)] # Flows
  
  # Make into long
  wqLng <- melt(wqAll, id.vars = 'date',
                measure.vars = c('cobs', 'cmod', 'lobs', 'lmod', 'qobs', 'qmod'),
                variable.name = 'src', value.name = 'val')
  
  vars <- names(wqAll[, 3 : length(wqAll)])
  
  vars <- data.frame(nme = c('Concentration', 'Load', 'Daily Flow'),
                     obs = c(vars[1], vars[3], vars[5]),
                     mod = c(vars[2], vars[4], vars[5]),
                     unt = c('mg/L', 'tons/day', 'cfs'),
                     lm1 = c(0.1, 0.1, 10),
                     lm2 = c(500, 10000, 10000),
                     stringsAsFactors = FALSE)
  
  # linear model
  for (i in 1 : nrow(vars)) {
  
    temp <- wqAll[, c(2, 2 * i + 1, 2 * i + 2)]
    
    names(temp) <- c('date', 'obsv', 'modl')
    
    temp$lobs <- log10(temp$obsv)
    
    temp$lmdl <- log10(temp$modl)
    
    idln <- data.frame(x = 0 : vars[i, 6],
                       y = 0 : vars[i, 6])
  
    regs <- list('ntrns' = summary(lm(modl ~ obsv, temp)),
                 'ltrns' = summary(lm(lmdl ~ lobs, temp)))
    
    fun <- list(function(x, m, b) {y = m * x + b},
                function(x, m, b) {y = 10^b * x^m})
  
    rgss <- eqns <- list()
    
    for (j in 1 : 2) {
      
      rgss[[j]] <- data.frame(x = 0 : vars[i, 6],
                              y = fun[[j]](0 : vars[i, 6], regs[[j]][[4]][2],
                                           regs[[j]][[4]][1]))
  
    }
    
    eqns[[1]] <- paste0('mod = ', round(regs[[1]][[4]][2], 2),
                        ' * obs + ', round(regs[[1]][[4]][1], 2),
                        ', R2 = ', round(regs[[1]][[8]], 2))
    
    eqns[[2]] <- paste0('mod = 10^', round(regs[[j]][[4]][1], 2),
                        ' * obs^', round(regs[[j]][[4]][2], 2),
                        ', R2 = ', round(regs[[j]][[8]], 2))
  
    plts <- ggplot(data = temp, aes(x = obsv, y = modl)) +
            geom_point(size = 2, color = 'darkblue') +
            scale_x_continuous(limits = c(vars[i, 5], vars[i, 6]), labels = comma) +
            scale_y_continuous(limits = c(vars[i, 5], vars[i, 6]), labels = comma) + 
            geom_line(data = idln, aes(x = x, y = y), color = 'black', size = 1.1) +
            geom_line(data = rgss[[1]], aes(x = x, y = y), color = 'darkblue', linetype = 2) +
            geom_line(data = rgss[[2]], aes(x = x, y = y), color = 'darkred', linetype = 4) +
            labs(x = paste('Observed', vars[i, 1], vars[i, 4]),
                 y = paste('Modeled', vars[i, 1], vars[i, 4])) +
            annotate('text', x = 0.65 * vars[i, 6], y = 0.15 * vars[i, 6],
                     label = eqns[[1]], hjust = 0) + 
            annotate('text', x = 0.65 * vars[i, 6], y = 0.10 * vars[i, 6],
                     label = eqns[[2]], hjust = 0)
    
    ggsave(paste0('sed_corr_', vars[i, 1], '_', n, '.png'),
           plot = plts, path = 'D:/siletz/calib/plots', width = 10, height = 10,
           units = 'in', dpi = 300)
    
  }

  # Move the output files to storage folders
  source('D:/siletz/scripts/R/move_hspf_files.R')
  
  move_hspf_files(filPath, n)
  
  # Update counter
  n = n + 1
  
  writeLines(as.character(n), countFil)
  
  close(countFil)

}
