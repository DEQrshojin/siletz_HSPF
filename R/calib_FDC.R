calib_FDC = function(ggeDat, mdlDat, pltPath, site, n) {
  
  library(ggplot2)
  library(reshape2)

  # Initialize percentages vector
  flwDur = data.frame('PCT' = seq(from = 0, to = 100, by = 1) / 100)
  
  flwDur$revPCT <- seq(from = 100, to = 0, by = -1) / 100
  
  # Calculate percentiles
  flwDur$MDL_Q = quantile(mdlDat, flwDur$PCT, na.rm = TRUE)
  
  flwDur$GGE_Q = quantile(ggeDat, flwDur$PCT, na.rm = TRUE)
  
  # Set the limits for the y axis
  durLims <- log10(c(min(flwDur[, 3 : 4], na.rm = TRUE),
                     max(flwDur[, 3 : 4], na.rm = TRUE)))
  
  durLims <- c(floor(durLims[1]), ceiling(durLims[2]))
  
  durLims <- 10^durLims
  
  # FOR NOW USE RMSE FOR COMPARISON OF FDC
  fdcRMSE = rmse(flwDur$GGE_Q, flwDur$MDL_Q, na.rm = TRUE) /
            (quantile(flwDur$GGE_Q, 1.00, na.rm = TRUE) -
            quantile(flwDur$MDL_Q, 0.00, na.rm = TRUE))

  # Reshape for graphing
  flwDurP = melt(flwDur, id.vars = 'revPCT')
  
  flwDurP = flwDurP[flwDurP$variable != 'PCT', ]
  
  fdcPlot = ggplot(data = flwDurP) +
            geom_line(aes(x = revPCT, y = value,
                          group = variable,
                          color = variable),
                      size = 1.1) + xlab("Probability of Exceedence") +
            scale_x_continuous(labels = c('0' = '0', '0.25' = '25',
                                          '0.5' = '50', '0.75' = '75',
                                          '100' = '100')) + 
            ylab("Flow (cfs)") + theme_bw() +
            scale_y_log10(limits = durLims, labels = comma) +
            theme(legend.position = c(0.2, 0.8),
                  panel.grid.minor = element_blank(),
                  axis.text.x = element_text(size = 13),
                  axis.title.y = element_text(size = 13),
                  axis.text.y = element_text(size = 13),
                  plot.title = element_text(size = 13,
                                            hjust = 0.5)) +
            annotate("text", x = 0.75, y = 50, size = 10,
                     label = paste0('RUN ', n), hjust = 0)
  
  ggsave(filename = paste0('fdc_plot_', site, '_', n, '.png'), plot = fdcPlot,
         path = pltPath, width = 15, height = 10, dpi = 300, units = 'in')
  
  return(fdcRMSE)
  
}
