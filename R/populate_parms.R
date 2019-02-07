# TAKE THE PAR VALUES AND IMBED IN THE COMPONENTS LIST ----
populate_parms <- function(parm, comp) {
  
  # FIND THE INDECES OF THE COMPONENT LIST ELEMENTS
  a <- grep('mtrx', names(comp))           # matrix indeces 
  b <- grep('indx', names(comp))           # index indeces 
  c <- grep('pwat', names(comp[['mtrx']])) # pwat matrix indeces
  d <- grep('mint', names(comp[['mtrx']])) # mon intercep matrix indeces
  e <- grep('mlze', names(comp[['mtrx']])) # mon lz et matrix indeces
  aa <- which(names(parm) == 'MINT')       # Monthly MINT parameter index
  bb <- which(names(parm) == 'MINTapp')    # MINT application indeces
  cc <- which(names(parm) == 'MLZE')       # Monthly MLZE parameter index
  dd <- which(names(parm) == 'MLZEapp')    # MLZE application indeces

  # POPULATE THE PWAT-PARMS FOR SCALARS ----
  comp[[a]][[c]]$LZSN <- parm[['LZSN']]
  comp[[a]][[c]]$LSUR <- parm[['LSUR']]
  comp[[a]][[c]]$AGWR <- parm[['AGWR']]
  comp[[a]][[c]]$DPFR <- parm[['DPFR']]
  comp[[a]][[c]]$BSFR <- parm[['BSFR']]
  comp[[a]][[c]]$AGWE <- parm[['AGWE']]
  comp[[a]][[c]]$INTR <- parm[['INTR']]

  # POPULATE INFILTRATION, INTERFLOW AND UZ STORAGE FOR EACH HRU ----
  # pwat of matrix with indeces for each hru = parameter with index of that HRU
  hrus <- unique(parm[['HRUS']])
  
  for (i in 1 : length(hrus)) {
    
    # Infiltration rate
    comp[[a]][[c]][comp[[b]][[hrus[i]]], 2] <- parm[['INFL']][i]
    
    # Upper zone storage
    comp[[a]][[c]][comp[[b]][[hrus[i]]], 8] <- parm[['UZSN']][i]
    
    # Interflow inflow parameter
    comp[[a]][[c]][comp[[b]][[hrus[i]]], 9] <- parm[['INFW']][i]
    
    # MONTHLY INTERCEPTION -- ALREADY MODIFIED BY MULTIPLIER
    comp[[a]][[d]][comp[[b]][[hrus[i]]], ] <- parm[[aa]][parm[[bb]][i], ]

    # MONTHLY LOWER-ZONE ET -- ALREADY MODIFIED BY MULTIPLIER
    comp[[a]][[e]][comp[[b]][[hrus[i]]], ] <- parm[[cc]][parm[[dd]][i], ]

  }
  
  # POPULATE THE REACH ROUTING (KS) PARAMETER
  comp[["mtrx"]][["rtks"]] <- comp[["mtrx"]][["rtks"]] * parm[["HYKS"]]
  
  return(comp)

}

