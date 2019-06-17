
#_______________________________________________________________________________
calib_FDC = function(ggeDat, mdlDat, pltPath, site, n) {
  
  library(ggplot2)
  library(reshape2)
  library(hydroGOF)
  
  # Initialize percentages vector
  flwDur = data.frame('PCT' = seq(from = 0, to = 100, by = 1) / 100)
  
  flwDur$revPCT <- seq(from = 100, to = 0, by = -1) / 100
  
  # Calculate percentiles
  flwDur$MDL_Q = quantile(mdlDat, flwDur$PCT, na.rm = TRUE)
  
  flwDur$GGE_Q = quantile(ggeDat, flwDur$PCT, na.rm = TRUE)
  
  # FOR NOW USE RMSE FOR COMPARISON OF FDC
  fdcNSE = NSE(flwDur$MDL_Q, flwDur$GGE_Q, na.rm = TRUE, FUN = log)
  
  # Reshape for graphing
  flwDurP = melt(flwDur, id.vars = 'revPCT')
  
  flwDurP = flwDurP[flwDurP$variable != 'PCT', ]
  
  fdcPlot = ggplot(data = flwDurP) + 
            geom_line(aes(x = revPCT, y = value, group = variable, color = variable),
                      size = 0.75) + xlab("Probability of Exceedence") +
            scale_color_manual(values = c('darkred', 'darkblue'),
                               labels = c('Model data', 'Gage data')) +
            ylab("Flow (cfs)") + theme_bw() + scale_y_log10(labels = comma) +
            guides(color = guide_legend(title = 'Flow data source')) + 
            theme(legend.position = c(0.75, 0.10)) +
            scale_x_continuous(labels = c('0' = '0', '0.25' = '25', '0.5' = '50',
                                          '0.75' = '75', '100' = '100'))
  
  ggsave(filename = paste0('fdc_plot_', site, '_', n, '.png'), plot = fdcPlot,
         path = pltPath, width = 10, height = 6.5, dpi = 300, units = 'in')
  
  return(fdcNSE)
  
}

#_______________________________________________________________________________
move_hspf_files <- function(filePath, n) {
  
  # SET FILES ----
  outFils <- list()
  
  outFils <- list(modFile = paste0(filPath, 'siletz', n),
                  outFile = paste0(filPath, 'siletz.out'),
                  echFile = paste0(filPath, 'siletz', n, '.ech'),
                  uciFile = paste0(filPath, 'siletz', n, '.uci'),
                  wdmFile = paste0(filPath, 'siletz', n, '_out.wdm'))
  
  # SET PATHS ----
  newFils <- list()
  
  newFils <- list(modFile = paste0(filPath, 'sprcd/mdl/siletz', n),
                  outFile = paste0(filPath, 'siletz', n, '.out'),
                  echFile = paste0(filPath, 'sprcd/out/siletz', n, '.ech'),
                  uciFile = paste0(filPath, 'sprcd/uci/siletz', n, '.uci'),
                  wdmFile = paste0(filPath, 'sprcd/wdm/siletz', n, '_out.wdm'))
  
  newOutFile = paste0(filPath, 'sprcd/out/siletz', n, '.out')
  
  # SHIP 'EM OUT!
  for (i in 1 : length(outFils)) {file.rename(outFils[[i]], newFils[[i]])}
  
  # NOW MOVE .OUT FILE (FIRST TIME IT WAS JUST RENAMED)
  file.rename(newFils[[2]], newOutFile)
  
}
