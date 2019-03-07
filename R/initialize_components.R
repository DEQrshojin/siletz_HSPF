# COMPONENT INITIALIZATION ----
initialize_components <- function(parm, basn, indx) {

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
  
  # Create for HYDR-PARM2 ----
  rtks <- rep(1, basn) # 1 because you'll specify a scalar multipler
  
  # Indeces for Individual HRU Infiltration Rates ----
  hrus <- unique(indx$HRU)
  
  indx2 <- list()
  
  for (i in 1 : length(hrus)) {
    
    indx2[[hrus[i]]] <- grep(hrus[i], indx$HRU)
    
  }
  
  # Create the list of components ----
  comp <- list(indx = indx2,
               mtrx = list(pwat = pwat,
                           mint = mint,
                           mlze = mlze,
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