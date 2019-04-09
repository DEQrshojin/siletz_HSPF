write_wq_2_pars <- function(pars = 'nox', suro = 1.00,
                            ifwa = 0.250, ifwb = -0.10, ifwc = 1.000,
                            agwa = 0.025, agwb = -0.01, agwc = 0.050,
                            shft = 0, tmat = 1) {
  
  # Read the counter from file to get the run number ----
  countFil = file('D:/siletz/wqct.txt')
  
  n = as.numeric(readLines(countFil)) - 1
  
  close(countFil)
  
  # Create the data frame of WQ parameters ----
  df <- data.frame(HRU = c('FOR', 'DEV', 'GRS', 'CUL', 'IMPRV'),
                   SURO = c(rep(suro, 5)),  # Surface flow runoff
                   IFWA = c(rep(ifwa, 5)),  # Interflow A coeff (sin)
                   IFWB = c(rep(ifwb, 5)),  # Interflow B coeff (cos)
                   IFWC = c(rep(ifwc, 5)),  # Interflow average concentration
                   AGWA = c(rep(agwa, 5)),  # Baseflow A coeff (sin)
                   AGWB = c(rep(agwb, 5)),  # Baseflow B coeff (sin)
                   AGWC = c(rep(agwc, 5)),  # Baseflow average concentration
                   SHFT = c(rep(shft, 5)))  # Phase shift

  # Adjust parameters if specified ----
  if (length(tmat) != 1) {
    
    for (i in 1 : (nrow(df) - 1)) {
    
      df[i + 1, c(2, 5, 8)] <- tmat[i] * df[i + 1, c(2, 5, 8)]
    
    }
  }
  
  # Rename and move old WQ parm file to \calib ----
  base <- 'D:/siletz/'
  
  fils <- c(paste0(base, c(paste0('emcdwc_', pars, '.csv'),
                           paste0('emcdwc_', pars, '_', n, '.csv'),
                           paste0('calib/parms/wq/emcdwc_', pars, '_', n, '.csv'))))
                           
  file.rename(fils[1], fils[2]) # RENAME
  
  file.rename(fils[2], fils[3]) # MOVE

  # Write new parameters to .csv ----
  write.csv(df, paste0('D:/siletz/emcdwc_', pars, '.csv'), row.names = F,
            quote = F)
  
  return(df)
 
}

