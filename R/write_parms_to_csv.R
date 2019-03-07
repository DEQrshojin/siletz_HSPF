# FUNCTION TO MODIFY HSPF MODEL PARAMETERS FROM CSV
# Ryan Shojinaga, Water Quality Analyst, NRS3, Oregon DEQ
# shojinaga.ryan@deq.state.or.us, 503-229-5777

# WRITE THE PARAMETERS TO THE CSVS ----
write_parms_to_csv <- function(comp) {
  
  path <- 'D:/siletz/calib/'
  
  # Read the counter from file to get the run number ----
  countFil = file('D:/siletz/count.txt')
  
  n = as.numeric(readLines(countFil))
  
  close(countFil)
  
  # SAVE THE PREVIOUS VERSIONS IN ANOTHER FOLDER
  move_parm_files(n)
  
  # WRITE PWAT PARAMETERS ----
  write.csv(comp[['mtrx']][['pwat']], file = paste0(path, 'pwat.csv'),
            row.names = FALSE)
  
  # WRITE MONTHLY INTERCEPTION PARAMETERS ----
  write.csv(comp[['mtrx']][['mint']], file = paste0(path, 'mint.csv'),
            row.names = FALSE)
  
  # WRITE MONTHLY LOWER ZONE ET PARAMETERS ----
  write.csv(comp[['mtrx']][['mlze']], file = paste0(path, 'lzet.csv'),
            row.names = FALSE)
  
  # WRITE ROUTING PARAMETERS ----
  write.csv(comp[['mtrx']][['rtks']], file = paste0(path, 'rtks.csv'),
            row.names = FALSE)
  
  write.csv(comp[['mtrx']][['sprp']], file = paste0(path, 'sprp.csv'),
            row.names = FALSE)
  
  write.csv(comp[['mtrx']][['sprr']], file = paste0(path, 'sprr.csv'),
            row.names = FALSE)
  
}

move_parm_files <- function(n) {
  
  # SET FILES ----
  aFils <- c('pwat', 'mint', 'lzet', 'rtks', 'sprp', 'sprr')
  
  bFils <- paste0('D:/siletz/calib/', aFils, '.csv')          # Old names
  
  cFils <- paste0('D:/siletz/calib/', aFils, n - 1, '.csv')       # new names 
  
  dFils <- paste0('D:/siletz/calib/parms/', aFils, n - 1, '.csv') # new locations
  
  # RENAME
  for (i in 1 : length(aFils)) {file.rename(bFils[i], cFils[i])}
  
  # MOVE
  for (i in 1 : length(aFils)) {file.rename(cFils[i], dFils[i])}

}
