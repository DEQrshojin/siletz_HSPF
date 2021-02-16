rm(list = ls()); cat('\014')

# FUNCTION TO DETECT THE NUMBER OF DECIMALS IN NUMBERS
nDec <- function(x) {
  stopifnot(class(x)=="numeric")
  x <- sub("0+$","",x)
  x <- sub("^.+[.]","",x)
  nchar(x)
}

# FOR POTENTIAL EVAPOTRANSPIRATION
# path <- 'C:/siletz_tmdl/01_inputs/01_hspf/pet'
#
# data <- read.csv(paste0(path, '/pet_base.csv'))

# FOR PRECIPITATION
path <- 'C:/siletz_tmdl/01_inputs/01_hspf/pcp'

data <- read.csv(paste0(path, '/wdm_blank_input.csv'))

# data <- read.csv(paste0(path, '/pcp_base.csv'))

basn <- names(data)[2 : length(data)]

subs <- data.frame(frst = c(7, 1, 4, 12), last = c(10, 2, 5, 13))

dtes <- list()

# Extract year, month, day, hour from dates
for (i in 1 : nrow(subs)) {
  dtes[[i]] <- as.numeric(substr(data$Date, subs$frst[i], subs$last[i]))
}

names(dtes) <- c('year', 'mnth', 'day', 'hour')

spc4 <- '    '; spc6 <- '      '

# PASTE TOGETHER DATES IN THE IMPORT FORMAT
text <- paste0(spc4, dtes$year,                                  # 1-8:   Year
               spc6, ifelse(dtes$mnth < 10, ' ', ''), dtes$mnth, # 9-16:  Month
               spc6, ifelse(dtes$day  < 10, ' ', ''), dtes$day,  # 17-24: Day
               spc6, ifelse(dtes$hour < 10, ' ', ''), dtes$hour) # 25-32: Hour

# CREATE BLANK OUTPUT OBJECT (WRITE TO TEXT)
outP <- rep(NA, length(text))

# ITERATE THROUGH EACH PRECIP COLUMN (BASINS) AND CONCATENATE EACH LINE
for (i in 2 : length(data)) {   # EACH BASIN

  # TRIM THE PRECIP DATA To NO MORE THAN 7 DIGITS
  rdcn <- nDec(data[, i]) - ifelse(nchar(data[, i]) > 7, nchar(data[, i]) - 7, 0)

  data[, i] <- round(data[, i], rdcn)

  for (j in 1 : length(outP)) { # EACH LINE

    spcs <- rep(' ', 8 - nchar(data[j, i])) # THE NUMBER OF SPACES PADDING

    blnk <- NULL

    for (k in 1 : length(spcs)) {blnk <- paste0(blnk, spcs[k])}

    outP[j] <- paste0(text[j], blnk, data[j, i])                  # 33-40: Value

  }

  # FOR POTENTIAL EVAPOTRANSPIRATION
  # write.table(outP, quote = F, row.names = F, col.names = F,
  #             file = paste0(path, '/pet_', basn[i - 1], '.txt'))

  # FOR PRECIPITATION
  write.table(outP, quote = F, row.names = F, col.names = F,
              file = paste0(path, '/pcp_', basn[i - 1], '.txt'))

}

