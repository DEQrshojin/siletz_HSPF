source('C:/siletz_tmdl/04_scripts/01_hspf/02_R/fnct_utilities_hspf.R')

qOut <- proc_flow_4_wq(hydros = 'C:/siletz_tmdl/02_outputs/01_hspf/',
                       oFil   = 'C:/siletz_tmdl/02_outputs/01_hspf/base_qOut.RData',
                       nFil   = 'base')
