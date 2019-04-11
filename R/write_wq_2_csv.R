write_wq_2_csv <- function(pars = NULL,  tmat = 1.000, SURO = 1.5, 
                           IFWC = 0.502, IFW1 = 0.230, IFW2 = -0.10, IFW3 = 0.087,
                           IFW4 = -0.02, IFW5 = 0.022, IFW6 = -0.02,
                           AGWC = 0.296, AGW1 = 0.188, AGW2 = -0.08, AGW3 = 0.048,
                           AGW4 = -0.07, AGW5 = 0.019, AGW6 = -0.02) {
  
  # Read the counter from file to get the run number ----
  countFil = file('D:/siletz/wqct.txt')
  
  n = as.numeric(readLines(countFil)) - 1
  
  close(countFil)
  
  # Create the data frame of WQ parameters ----
  df <- data.frame(HRU = c('FOR', 'DEV', 'GRS', 'CUL', 'IMPRV'),
                   SURO = c(rep(SURO, 5)),
                   IFWC = c(rep(IFWC, 5)),
                   IFW1 = c(rep(IFW1, 5)),
                   IFW2 = c(rep(IFW2, 5)),
                   IFW3 = c(rep(IFW3, 5)),
                   IFW4 = c(rep(IFW4, 5)),
                   IFW5 = c(rep(IFW5, 5)),
                   IFW6 = c(rep(IFW6, 5)),
                   AGWC = c(rep(AGWC, 5)),
                   AGW1 = c(rep(AGW1, 5)),
                   AGW2 = c(rep(AGW2, 5)),
                   AGW3 = c(rep(AGW3, 5)),
                   AGW4 = c(rep(AGW4, 5)),
                   AGW5 = c(rep(AGW5, 5)),
                   AGW6 = c(rep(AGW6, 5)))

  # Adjust parameters if specified ----
  if (length(tmat) != 1) {
    
    for (i in 1 : (nrow(df) - 1)) {
    
      df[i + 1, c(2, 3, 10)] <- tmat[i] * df[i + 1, c(2, 3, 10)]
    
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

