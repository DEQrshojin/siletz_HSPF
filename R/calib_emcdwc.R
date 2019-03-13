library('reshape2')
library('ggplot2')

# Load data ----
rchQLC <- readRDS('D:/siletz/calib/wq/rchQLC.RData') # Model flows/loads/conc

wqData <- read.csv('D:/siletz/calib/wq/sediment_STA10391.csv',
                   stringsAsFactors = FALSE)

# Filter data ----
wqData$date <- as.Date(wqData$date, '%Y-%m-%d')

basin <- 'Bas14'

col <- which(names(rchQLC$reach_flows) == basin)

# trim observations to dates of model data
modDts <- as.Date(c(min(rchQLC[[1]]$Date), max(rchQLC[[1]]$Date)),
                  '%Y-%m-%d %H:%M:%S', tz = 'America/Los_Angeles')

wqData <- wqData[which(wqData$date >= modDts[1] & wqData$date <= modDts[2]), ]

# Model data 2 daily ----
# model data
aggFun <- c('mean', 'sum', 'mean')

dlyQLC <- list()

for (i in 1 : length(rchQLC)) {

  dlyQLC[[i]] <- rchQLC[[i]][, c(1, col)]
  
  dlyQLC[[i]]$Date2 <- as.Date(dlyQLC[[i]]$Date, '%Y-%m-%d %H:%M:%S',
                               tz = 'America/Los_Angeles')
    
  dlyQLC[[i]] <- aggregate(dlyQLC[[i]][, 2],
                           by = list(dlyQLC[[i]]$Date2),
                           FUN = aggFun[i])
  
  names(dlyQLC)[i] <- names(rchQLC)[i]
  
  names(dlyQLC[[i]]) <- c('Date', basin)

}

# Extract flows/loads/concentrations
datM <- data.frame(dlyQLC[['reach_flows']],
                   LM = dlyQLC[['reach_loads']]$Bas14,
                   CM = dlyQLC[['reach_conc']]$Bas14)

names(datM)[2] <- 'QM'

datM$LM <- datM$LM / 1000 # Convert from kg to tons (metric)

datM$QM <- datM$QM * 35.314666213 # Convert flows to cfs

# Merge model and observation data
datM <- merge(datM, wqData, by.x = 'Date', by.y = 'date', all.x = TRUE)

names(datM)[5 : 8] = c('CO', 'opr', 'QO', 'LO')

datM <- datM[, c(1, 2, 7, 3, 8, 4, 5, 6)]

datCC <- datM[complete.cases(datM), ]

plain <- function(x) {format(x, scientific = FALSE, trim = TRUE)}

pltL <- ggplot(data = datM, aes(x = Date)) +
        geom_line(aes(y = LM), color = 'darkred', size = 1.2) + 
        geom_point(aes(y = LO), size = 2, shape = 23, color = 'darkblue',
                   stroke = 1.2, fill = 'yellow') + 
        scale_y_log10(labels = plain)

ggsave('loads.png', plot = pltL, path = 'D:/siletz/calib/wq', width = 15,
       height = 10, units = 'in', dpi = 300)

pltC <- ggplot(data = datM, aes(x = Date)) +
        geom_line(aes(y = CM), color = 'darkred', size = 1.2) + 
        geom_point(aes(y = CO), size = 2, shape = 23, color = 'darkblue',
                   stroke = 1.2, fill = 'yellow') + 
        scale_y_log10(labels = plain)

ggsave('conc.png', plot = pltC, path = 'D:/siletz/calib/wq', width = 15,
       height = 10, units = 'in', dpi = 300)


pltQ <- ggplot(data = datM, aes(x = Date)) + 
  geom_line(aes(y = QM), color = 'darkred', size = 1.2) + 
  geom_point(aes(y = QO), size = 2, shape = 23, color = 'darkblue',
             stroke = 1.2, fill = 'yellow') +
  scale_y_log10(labels = plain)

ggsave('flow.png', plot = pltQ, path = 'D:/siletz/calib/wq', width = 15,
       height = 10, units = 'in', dpi = 300)



                                                             

