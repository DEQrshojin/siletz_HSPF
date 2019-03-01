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
