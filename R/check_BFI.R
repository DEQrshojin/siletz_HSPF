# CHECK BASEFLOW INDEX --------------------------------------------------------
qTotLat <- qlcLat[["flow"]]

qTotROC <- qTotLat

qTotROC[, 2 : length(qTotROC)] <- 0

for (i in 2 : length(qTotROC)) {
  
  qTotROC[, i] <- qlcROC[["flow"]][[i - 1]]$AGWO
  
}

BFI <- sum(rowSums(qTotROC[, 2 : length(qTotROC)])) / 
  sum(rowSums(qTotLat[, 2 : length(qTotLat)]))
