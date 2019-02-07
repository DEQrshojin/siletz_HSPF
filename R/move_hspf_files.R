move_hspf_files <- function(filePath, n) {
  
  # SET FILES ----
  outFils <- list()
  
  outFils <- list(modFile = paste0(filPath, 'siletz', n),
                  outFile = paste0(filPath, 'siletz.out'),
                  echFile = paste0(filPath, 'siletz', n, '.ech'),
                  uciFile = paste0(filPath, 'siletz', n, '.uci'),
                  wdmFile = paste0(filPath, 'siletz', n, '_out.wdm'))
  
  # SET PATHS ----
  newFils <- list()
  
  newFils <- list(modFile = paste0(filPath, 'sprcd/mdl/siletz', n),
                  outFile = paste0(filPath, 'siletz', n, '.out'),
                  echFile = paste0(filPath, 'sprcd/out/siletz', n, '.ech'),
                  uciFile = paste0(filPath, 'sprcd/uci/siletz', n, '.uci'),
                  wdmFile = paste0(filPath, 'sprcd/wdm/siletz', n, '_out.wdm'))
  
  newOutFile = paste0(filPath, 'sprcd/out/siletz', n, '.out')
  
  # SHIP 'EM OUT!
  for (i in 1 : length(outFils)) {file.rename(outFils[[i]], newFils[[i]])}
  
  # NOW MOVE .OUT FILE (FIRST TIME IT WAS JUST RENAMED)
  file.rename(newFils[[2]], newOutFile)

}
