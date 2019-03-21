proc_network_linkage = function(shpFile) {
  
  library(raster)
  
  shpFile = shapefile(shpFile)
  
  lnks = data.frame(cbind('Basn' = as.numeric(shpFile@data[["HSPF_Bas"]]),
                          'DSBs' = as.numeric(shpFile@data[["DS_Basin"]])),
                    stringsAsFactors = FALSE)

  # List of each basin & vector of upstream basin(s); not cumulative, just the
  # immediately upstream basin(s) for upstream inflow inputs
  # Basins with no upstream basins (headwaters) have a value of 0
  
  usBas = rep(list(0), nrow(lnks)) # Upstream basins
  
  twBas = unique(lnks$DSBs) # Tailwater basins
  
  twBas = twBas[which(twBas != 0)] # remove zeros (0 = watershed outlet)
  
  for (basin in twBas) {
  
    usBas[[basin]] = lnks[which(lnks$DSBs == basin), 1]
  
  }
  
  # Make a vector of the basin processing order
  hwBas = lnks$Basn # All headwater basins
  
  hwBas = hwBas[!(hwBas %in% twBas)] # Which basins are not a tailwater basin
  
  # First order basins (headwaters)
  nthOrdBas = list()
  
  nthOrdBas[[1]] = procOrd = hwBas # initialize processing order
  
  n = 2 # Counter for indexing the loop; n = 1 addressed with headwaters
  
  # subsequent order basins
  repeat {
    
    nthOrdBas[[n]] = lnks[which(lnks$Basn %in% nthOrdBas[[n - 1]]), 2]
    
    nthOrdBas[[n]] = unique(nthOrdBas[[n]]) # remove duplicates
    
    nthOrdBas[[n]] = nthOrdBas[[n]][which(nthOrdBas[[n]] != 0)] # remove zeros
    
    if (length(nthOrdBas[[n]]) == 0) { # exit when the nthOrdBas has no elements
      
      break
      
    }
    
    procOrd = c(procOrd, nthOrdBas[[n]])
    
    n = n + 1      
  }
  
  # Now, keep the LAST indexed instance of dup basins. The first column is the
  # process order, the second is the basin to process in that order
  tmpOrd = data.frame(cbind('ORD' = as.vector(tapply(seq_along(procOrd),
                                                     procOrd, max)),
                            'BAS' = 1 : nrow(lnks)), stringsAsFactors = FALSE)
  
  tmpOrd = tmpOrd[order(tmpOrd$ORD), ]
  
  tmpOrd$ORD = 1 : nrow(lnks)
  
  rownames(tmpOrd) = 1 : nrow(lnks)
  
  procLnks = list('pOrd' = tmpOrd, 'cBas' = usBas)
  
  return(procLnks)
  
}