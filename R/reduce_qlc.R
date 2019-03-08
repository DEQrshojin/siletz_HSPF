reduce_qlc <- function(strDte = NULL, endDte = NULL, df2Red = NULL) {
  
  ts <- as.POSIXct(c(strDte, endDte), '%Y-%m-%d',
                   tz = 'America/Los_Angeles')

  ts <- seq(ts[1], ts[2], 3600)

  names(df2Red)[1] <- 'Date'

  redDF <- df2Red[df2Red$Date %in% ts, ]
  
  return(redDF)

}
