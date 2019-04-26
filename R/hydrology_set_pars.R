
#_______________________________________________________________________________
read_pars <- function(parFil) {
  
  parm <- strsplit(readLines(parFil), ',')
  
  # REMOVE COMMENT, BLANK LINES, AND EMPTY SPACES ----
  parm[grep('#{3}', parm)] <- NULL
  
  parm[which(parm == ' ')] <- NULL
  
  parm <- parm[lapply(parm, length) > 0]
  
  # PROCESS SCALAR OR VECTOR PARAMETERS ----
  for (i in 1 : length(parm)) {
    
    names(parm)[i] = parm[[i]][1] 
    
    # Remove the first list item
    if (parm[[i]][1] == 'HRUS' | parm[[i]][1] == 'MON_VARS') {
      
      parm[[i]] <- parm[[i]][2 : length(parm[[i]])]
      
    } else {
      
      parm[[i]] <- as.numeric(parm[[i]][2 : length(parm[[i]])])
      
    }
  }
  
  mnth <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
            'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
  
  # MONTHY VALUES
  for (par in parm[['MON_VARS']]) {
    
    parm <- monthly_pars(parm, par, mnth)  
    
  }
  
  return(parm)
  
}

#_______________________________________________________________________________
initialize_components <- function(parm, basn, indx) {
  
  # Synopsis ----
  
  
  # INITIALIZE THE DATA FRAMES ----
  prNm <- c('LZSN', 'INFL', 'LSUR', 'AGWR', 'DPFR', 'BSFR', 'AGWE',
            'UZSN', 'INFW', 'INTR')
  
  mnth <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
            'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
  
  pwat <- data.frame(matrix(data = 0, nrow = nrow(indx), ncol = length(prNm),
                            byrow = FALSE, dimnames = list(NULL, prNm)),
                     stringsAsFactors = FALSE)
  
  # Create for MON-INTERCEP ----
  mint <- data.frame(matrix(data = 0, nrow = nrow(indx), ncol = length(mnth),
                            byrow = FALSE, dimnames = list(NULL, mnth)),
                     stringsAsFactors = FALSE)
  
  # Create for MON-LZETP ----
  mlze <- data.frame(matrix(data = 0, nrow = nrow(indx), ncol = length(mnth),
                            byrow = FALSE, dimnames = list(NULL, mnth)),
                     stringsAsFactors = FALSE)
  
  # Create for HYDR-PARM2; Routing coefficient ----
  rtks <- rep(1, basn) # 1 because you'll specify a scalar multipler
  
  # Indeces for Individual HRU Infiltration Rates ----
  hrus <- unique(indx$HRU)
  
  indx2 <- list()
  
  for (i in 1 : length(hrus)) {
    
    indx2[[hrus[i]]] <- grep(hrus[i], indx$HRU)
    
  }
  
  # Create the list of components ----
  comp <- list(indx = indx2, mtrx = list(pwat = pwat, mint = mint, mlze = mlze,
                                         rtks = rtks))
  
  # Check for sediment (SDSW = 1)
  if (parm[['SDSW']] == 1) {
    
    s1Nm <- c('KRER', 'JRER', 'AFFX', 'COVR', 'KSER', 'JSER', 'DETS')
    
    s2Nm <- c('DB50', 'PORE', 'WSND', 'KSND', 'XSND', 'WSLT', 'TAUDS', 'TAUSS',
              'MSLT', 'WCLY', 'TAUDC', 'TAUSC', 'MCLY')
    
    # For PERLND parameters
    sprp <- data.frame(matrix(data = 0, nrow = nrow(indx), ncol = length(s1Nm),
                              byrow = FALSE, dimnames = list(NULL, s1Nm)),
                       stringsAsFactors = FALSE)
    
    # For RCHRES parameters
    sprr <- data.frame(matrix(data = 0, nrow = basn, ncol = length(s2Nm),
                              byrow = FALSE, dimnames = list(NULL, s2Nm)),
                       stringsAsFactors = FALSE)
    
    comp[['mtrx']][['sprp']] = sprp
    
    comp[['mtrx']][['sprr']] = sprr
    
  }
  
  return(comp)
  
}

#_______________________________________________________________________________
monthly_pars <- function(parm, prNm, mnth) {
  
  # Find multiplier index
  mltInd <- grep(paste0(prNm, 'mult'), names(parm))
  
  # Find HRU-specific lines
  hruInd <- grep(paste0(prNm, '-'), names(parm))
  
  prmNms <- names(parm)[hruInd]
  
  # Create data frame item for the parm list
  parm[[prNm]] <- data.frame(matrix(data = do.call(rbind, parm[hruInd]),
                                    nrow = length(hruInd),
                                    ncol = length(mnth),
                                    byrow = FALSE,
                                    dimnames = list(prmNms, mnth)),
                             stringsAsFactors = FALSE)
  
  parm[[prNm]] <- parm[[prNm]] * parm[[mltInd]]
  
  delLst <- paste0(prNm, '-|', prNm, 'm')
  
  parm[grep(delLst, names(parm))] <- NULL
  
  return(parm)
  
}

#_______________________________________________________________________________
populate_pars <- function(parm, comp) {
  
  # FIND THE INDECES OF THE COMPONENT LIST ELEMENTS
  a <- grep('mtrx', names(comp))           # matrix indeces 
  b <- grep('indx', names(comp))           # index indeces 
  c <- grep('pwat', names(comp[['mtrx']])) # pwat matrix indeces
  d <- grep('mint', names(comp[['mtrx']])) # mon intercep matrix indeces
  e <- grep('mlze', names(comp[['mtrx']])) # mon lz et matrix indeces
  f <- grep('sprp', names(comp[['mtrx']])) # perlnd sediment parameters
  g <- grep('sprr', names(comp[['mtrx']])) # rchres sediment parameters
  aa <- which(names(parm) == 'MINT')       # Monthly MINT parameter index
  bb <- which(names(parm) == 'MINTapp')    # MINT application indeces
  cc <- which(names(parm) == 'MLZE')       # Monthly MLZE parameter index
  dd <- which(names(parm) == 'MLZEapp')    # MLZE application indeces
  
  # POPULATE THE PWAT-PARMS & SEDIMENT FOR GLOBAL SCALARS ----
  glbP <- names(comp[[a]][[c]])
  
  if(length(f) != 0) {glsp <- names(comp[[a]][[f]])
                      glsr <- names(comp[[a]][[g]])}
  
  for (n in 1 : length(glbP)) { # GLOBAL PWAT-PARMS
    
    if (glbP[n] != 'INFL' & glbP[n] != 'UZSN' & glbP[n] != 'INFW') {
      
      comp[[a]][[c]][, n] <- parm[[glbP[n]]]
      
    }
  }
  
  if (parm[['SDSW']] == 1) { # GLOBAL SED-PARMS
    
    for (n in 1 : length(glsp)) {
      
      if (glsp[n] != 'COVR') {comp[[a]][[f]][, n] <- parm[[glsp[n]]]}
      
    }
  }
  
  # POPULATE HRU-SPECIFIC PARAMETERS ----
  # pwat of matrix with indeces for each hru = parameter with index of that HRU
  hrus <- unique(parm[['HRUS']])
  
  for (i in 1 : length(hrus)) {
    
    # Infiltration rate
    comp[[a]][[c]][comp[[b]][[hrus[i]]], 2] <- parm[['INFL']][i]
    
    # Upper zone storage
    comp[[a]][[c]][comp[[b]][[hrus[i]]], 8] <- parm[['UZSN']][i]
    
    # Interflow inflow parameter
    comp[[a]][[c]][comp[[b]][[hrus[i]]], 9] <- parm[['INFW']][i]
    
    # Land surface shield
    if(length(f) != 0) {comp[[a]][[f]][comp[[b]][[hrus[i]]], 4] <-
                        parm[['COVR']][i]}
    
    # MONTHLY INTERCEPTION -- ALREADY MODIFIED BY MULTIPLIER
    comp[[a]][[d]][comp[[b]][[hrus[i]]], ] <- parm[[aa]][parm[[bb]][i], ]
    
    # MONTHLY LOWER-ZONE ET -- ALREADY MODIFIED BY MULTIPLIER
    comp[[a]][[e]][comp[[b]][[hrus[i]]], ] <- parm[[cc]][parm[[dd]][i], ]
    
  }
  
  # POPULATE THE REACH ROUTING (KS) PARAMETER
  comp[["mtrx"]][["rtks"]] <- comp[["mtrx"]][["rtks"]] * parm[["HYKS"]]
  
  # GLOBAL REACH SEDIMENT PARS
  if (parm[['SDSW']] == 1) { 
    
    for (n in 1 : length(glsr)) {
      
      comp[[a]][[g]][, n] <- parm[[glsr[n]]]
      
    }
  }
  
  return(comp)
  
}

#_______________________________________________________________________________
write_pars_to_csv <- function(comp) {
  
  path <- 'D:/siletz/calib/'
  
  # Read the counter from file to get the run number ----
  countFil = file('D:/siletz/count.txt')
  
  n = as.numeric(readLines(countFil))
  
  close(countFil)
  
  # SAVE THE PREVIOUS VERSIONS IN ANOTHER FOLDER
  move_par_files(n)
  
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

#_______________________________________________________________________________
move_par_files <- function(n) {
  
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
