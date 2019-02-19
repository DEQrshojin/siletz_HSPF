analyze_bec_objfun <- function(pstFil) {
  
  pest <- readLines(pstFil)
  
  pest <- pest[61 : 7120]
  
  pest <- data.frame(do.call('rbind', strsplit(pest, '\\s+')),
                     stringsAsFactors = FALSE)
  
  pest <- pest[, -1]
  
  names(pest) <- c('obsNme', 'value', 'weight', 'obsgrp')
  
  pest$value <- as.numeric(pest$value)
  
  pest$weight <- as.numeric(pest$weight)
  
  pest$obsgrp <- as.factor(pest$obsgrp)
  
  x <- aggregate(pest$weight, by = list(pest$obsgrp), FUN = 'sum')
  
  pest$logval <- log10(pest$value)
  
  pest$logwgt <- log10(pest$weight)
  
  library(stats)
  
  indx <- list('qday' = which(pest[, 4] == 'mlog'),
               'peak' = which(pest[, 4] == 'mpeak'),
               'mann' = which(pest[, 4] == 'mvol_ann'),
               'msmr' = which(pest[, 4] == 'mvol_smr'),
               'mstm' = which(pest[, 4] == 'mvol_stm'),
               'mwtr' = which(pest[, 4] == 'mvol_wtr'))
  
  regs <- list('weights' = x,
               'counts' = indx,
               'qday' = lm(logwgt ~ logval, pest, indx[['qday']]),
               'peak' = lm(logwgt ~ logval, pest, indx[['peak']]),
               'vann' = lm(logwgt ~ logval, pest, indx[['mann']]),
               'vsmr' = lm(logwgt ~ logval, pest, indx[['msmr']]),
               'vstm' = lm(logwgt ~ logval, pest, indx[['mstm']]),
               'vwtr' = lm(logwgt ~ logval, pest, indx[['mwtr']]))
  
  return(regs)
  
}

