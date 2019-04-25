# Use upper case for PAR where needed (i.e., TP, NOx, PO4)

check_2017_wq <- function(par, n) {

  library(dplyr); library(reshape2); library(ggplot2)
  
  # IMPORT AMBIENT WATER QUALITY DATA ----
  wqAll <- readRDS(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxy',
                          'gen/Middle_Siletz_River_1710020405/001_data/wq_data',
                          '/awqms_WQ_data.Rdata'))
  
  dates <- seq(as.Date('2017-07-01', '%Y-%m-%d'),
               as.Date('2017-10-01', '%Y-%m-%d'), 1)
  
  # JUST WORRY ABOUT NITRATE, TP and PO4 for the moment
  wqDt <- wqAll[[1]] %>% 
          mutate(date = as.Date(dt), vnd = ifelse(opr == '<', val / 2, val)) %>%
          filter(dql %in% c('DQL=A', 'DQL=B'), date %in% dates)
  
  wqDt <- wqDt[, c(16, 3, 5, 17, 9, 2)]

  wqDt <- wqDt[which(wqDt$new %in% par & wqDt$date %in% dates), ]
      
  wqLC <- read.csv(paste0('//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Dissolved Oxy',
                          'gen/Middle_Siletz_River_1710020405/001_data/wq_data',
                          '/Monitoring 2017/LSWCD/CCAL_SILETZ_092017_092617_RMS.csv'),
                   stringsAsFactors = F)
  
  wqLC$date <- as.Date(wqLC$date, '%Y-%m-%d')
  
  wqLC$dt <- as.POSIXct(wqLC$dt, '%Y-%m-%d %H:%M', tz = 'America/Los_Angeles')
  
  wqLC <- wqLC[wqLC$new %in% par, ]
  
  wqDt$src <- 'ORDEQ'
  
  if (nrow(wqLC) !=0) {wqLC$src <- 'LSWCD'; wqDt <- rbind(wqDt, wqLC)}

  mtch <- data.frame(site = c('10391-ORDEQ', '37396-ORDEQ', '38918-ORDEQ',
                              '11246-ORDEQ', '29287-ORDEQ', '38919-ORDEQ',
                              '38930-ORDEQ', '36367-ORDEQ', '38928-ORDEQ', '38929-ORDEQ'),
                     basn = c(14, 6, 11, 7, 13, 13, 10, 15, 8, 10),
                     cols = c(14, 6, 11, 7, 13, 13, 10, 15, 8, 10) + 1,
                     stringsAsFactors = FALSE)
  
  mdDt <- readRDS(paste0('D:/siletz/calib/wq/rchQLC.RData'))
  
  mdDt <- mdDt[['reach_conc']] %>% mutate(Date2 = as.Date(Date)) %>%
          filter(Date2 %in% dates)
  
  mdDt <- aggregate(mdDt[2 : (length(mdDt) - 1)], by = list(mdDt$Date2), FUN = mean)
  
  mdDt <- mdDt[, c(1, mtch$cols)]
  
  names(mdDt) <- c('Date', mtch$site)
  
  mdDt <- melt(mdDt, id.vars = 'Date', value.name = 'C_mgL', variable.name = 'stn')
  
  plt <- ggplot(mdDt, aes(x = Date, y = C_mgL)) +
         geom_line(color = 'darkblue', size = 0.5) +
         geom_point(data = wqDt, aes(x = date, y = vnd), size = 1.2, shape = 23,
                    color = 'darkred', stroke = 1.0, fill = 'yellow') +
         facet_wrap(~stn, ncol = 4)
  
  ggsave(paste0(par, '_2017_conc_ts_', n, '.png'), plot = plt, dpi = 300, units = "in",
         path = 'D:/siletz/calib/wq/plots', width = 10, height = 7.5)

}


