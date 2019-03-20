get_sparrow_loads <- function(cmd, par) {

  dir <- paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxygen/Middle',
                '_Siletz_River_1710020405/001_data/SPARROW/')

  # Loads File
  if (par == 'TSS') {

    lds <- readLines(paste0(dir, 'sir20165079_TSS_NHDV2_predict_data.txt'))

  } else if (par == 'TN' | par == 'TP') {
    
    lds <- readLines(paste0(dir, 'sir20135103_Nutrients_PNW_SPARROW_NHD_predic',
                            't_data.txt'))
    
  } else {stop('Please specify a valid WQ parameter (TSS, TN, or TP).')}

  if (par == 'TSS') {lds <- gsub('[,\\\"]', '', lds)} # Remove ' and , in TSS

  # Remove lines with preceding #
  lds <- lds[-grep('#', lds)]

  # Remove lines before the COMID
  lds <- lds[-(1 : (grep('COMID', lds)) - 1)]

  lds <- data.frame(do.call('rbind', strsplit(lds, '\t')),
                    stringsAsFactors = F)  
  
  names(lds) <- lds[1, ]

  lds <- lds[-1, ]

  lds <- as.data.frame(lapply(lds, FUN = as.numeric))

  lds <- lds[lds$COMID %in% cmd, ]
  
  if (par == 'TSS') {
    
    lds <- lds[, c(1, 3)]
    
  } else if (par == 'TN') {
    
    lds <- lds[c(2, 1), c(1, 2)]
    
  } else if (par == 'TP') {
    
    lds <- lds[c(2, 1), c(1, 12)]
    
  }
  
  names(lds)[2] <- paste0('L_', par, '_kgyr')

  return(lds)
  
}
  