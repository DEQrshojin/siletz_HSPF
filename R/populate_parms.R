# TAKE THE PAR VALUES AND IMBED IN THE COMPONENTS LIST ----
populate_parms <- function(parm, comp) {
  
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
  
  glsp <- names(comp[[a]][[f]])
  
  glsr <- names(comp[[a]][[g]])

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
    comp[[a]][[f]][comp[[b]][[hrus[i]]], 4] <- parm[['COVR']][i]
    
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