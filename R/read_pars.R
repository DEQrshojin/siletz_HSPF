read_pars <- function(parFil) {
  
  parm <- strsplit(readLines(parFil), ',')

  # REMOVE COMMENT AND BLANK LINES ----
  parm[grep('#{3}', parm)] <- NULL
  
  parm <- parm[lapply(parm, length) > 0]
  
  # PROCESS SCALAR OR VECTOR PARAMETERS ----
  for (i in 1 : length(parm)) {
    
    names(parm)[i] = parm[[i]][1] 
      
    # Remove the first list item
    if (parm[[i]][1] == 'HRUS') {
        
      parm[[i]] <- parm[[i]][2 : length(parm[[i]])]
        
    } else {
        
      parm[[i]] <- as.numeric(parm[[i]][2 : length(parm[[i]])])
        
    }
  }
  
  # MONTHY INTERCEPTION ----
  mnth <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
            'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')
  
  mintInd <- grep('MINT-', names(parm))
  
  mintNms <- names(parm)[mintInd]
  
  parm[['MINT']] <- data.frame(matrix(data = do.call(rbind, parm[mintInd]),
                                      nrow = length(mintInd),
                                      ncol = length(mnth),
                                      byrow = FALSE,
                                      dimnames = list(mintNms, mnth)),
                               stringsAsFactors = FALSE)
  
  parm[['MINT']] <- parm[['MINT']] * parm[['MINTmult']]
  
  # MONTHY LOWER ZONE ET PARAMETERS ----
  mlzeInd <- grep('MLZE-', names(parm))
  
  mlzeNms <- names(parm)[mlzeInd]
  
  parm[['MLZE']] <- data.frame(matrix(data = do.call(rbind, parm[mlzeInd]),
                                      nrow = length(mlzeInd),
                                      ncol = length(mnth),
                                      byrow = FALSE,
                                      dimnames = list(mlzeNms, mnth)),
                               stringsAsFactors = FALSE)
  
  parm[['MLZE']] <- parm[['MLZE']] * parm[['MLZEmult']]
  
  # DELETE THE MINT AND MLZE INTERIMS ----
  parm[grep('MINT-|MLZE-|MINTm|MLZEm', names(parm))] <- NULL
  
  return(parm)

}

