# libraries
library(dplyr)
library(tidyverse)
library(tidyterra)
library(terra)
library(here)

# Globals
wk.dir = here()
fires <- c('riverside', 'beachiecreek', 'lionshead', 'holidayfarm', 'archiecreek')
fire.lookup <- c('riverside'='riverside', 'beachiecreek'='beachie_creek', 'lionshead'='lionshead', 'holidayfarm'='holiday_farm', 'archiecreek'='archie_creek')
fire.lookup.cap <- c('riverside'='Riverside', 'beachiecreek'='BeachieCreek', 'lionshead'='Lionshead', 'holidayfarm'='HolidayFarm', 'archiecreek'='ArchieCreek')

purrr::map(fires, function(f) {
  print(f)

  # Open masked distance rasters
  d.1 <- rast(paste(wk.dir, f, '_2020_dist_masked.tif', sep=''))
  d.2 <- rast(paste(wk.dir, f, '_2021_dist_masked.tif', sep=''))
  d.3 <- rast(paste(wk.dir, f, '_2022_dist_masked.tif', sep=''))
  d.4 <- rast(paste(wk.dir, f, '_2023_dist_masked.tif', sep=''))
  
  # open severity raster, then mask distance rasters
  sev <- rast(paste(fire.lookup.cap[f], '_SevMask_2020.tif', sep='')) %>% resample(d.1, method='near')
  names(sev) <- 'sev'
  
  d.1 <- mask(d.1, sev, maskvalue=2)
  d.2 <- mask(d.2, sev, maskvalue=2)
  d.3 <- mask(d.3, sev, maskvalue=2)
  d.4 <- mask(d.4, sev, maskvalue=2)
  
  gc()
  
  # stats
  # Distance
  names(d.1) <- 'dist'
  df.d.1 <- d.1 %>% as.data.frame(na.rm=TRUE)
  df.d.1$year <- 2020
  
  names(d.2) <- 'dist'
  df.d.2 <- d.2 %>% as.data.frame(na.rm=TRUE)
  df.d.2$year <- 2021
  
  names(d.3) <- 'dist'
  df.d.3 <- d.3 %>% as.data.frame(na.rm=TRUE)
  df.d.3$year <- 2022
  
  names(d.4) <- 'dist'
  df.d.4 <- d.4 %>% as.data.frame(na.rm=TRUE)
  df.d.4$year <- 2023
  
  remove(d.1, d.2, d.3, d.4)
  gc()
  
  df.dist <- bind_rows(df.d.1, df.d.2, df.d.3, df.d.4) %>%
    mutate(distclass = cut(dist, breaks=c(-1,200,400,Inf), labels=c('0-200','200-400','>400')))
  
  remove(df.d.1, df.d.2, df.d.3, df.d.4)
  
  d.stats <- df.dist %>%
    group_by(distclass, year) %>% 
    arrange(year, .by_group = TRUE) %>%
    summarise(n=n()) %>%
    mutate(area = n*9*0.0001) %>%
    mutate(pct_change = (area-lag(area))/lag(area) * 100)
  
  write.csv(d.stats, paste('dist_stats_masked_', f, '_v2.csv', sep=''))
  
  gc()
  
  remove(df.dist, d.stats)
  gc()
  
  ###########################
  
  # Open masked distance rasters
  d2wd.1 <- rast(paste(wk.dir, f, '_2020_d2wd_masked.tif', sep=''))
  d2wd.2 <- rast(paste(wk.dir, f, '_2021_d2wd_masked.tif', sep=''))
  d2wd.3 <- rast(paste(wk.dir, f, '_2022_d2wd_masked.tif', sep=''))
  d2wd.4 <- rast(paste(wk.dir, f, '_2023_d2wd_masked.tif', sep=''))
  
  # open severity raster, then mask distance rasters
  sev <- rast(paste(fire.lookup.cap[f], '_SevMask_2020.tif', sep='')) %>% resample(d2wd.1, method='near')
  names(sev) <- 'sev'
  
  d2wd.1 <- mask(d2wd.1, sev, maskvalue=2)
  d2wd.2 <- mask(d2wd.2, sev, maskvalue=2)
  d2wd.3 <- mask(d2wd.3, sev, maskvalue=2)
  d2wd.4 <- mask(d2wd.4, sev, maskvalue=2)
  
  gc()
  
  # stats
  # Distance
  names(d2wd.1) <- 'd2wd'
  df.d2wd.1 <- d2wd.1 %>% as.data.frame(na.rm=TRUE)
  df.d2wd.1$year <- 2020
  
  names(d2wd.2) <- 'd2wd'
  df.d2wd.2 <- d2wd.2 %>% as.data.frame(na.rm=TRUE)
  df.d2wd.2$year <- 2021
  
  names(d2wd.3) <- 'd2wd'
  df.d2wd.3 <- d2wd.3 %>% as.data.frame(na.rm=TRUE)
  df.d2wd.3$year <- 2022
  
  names(d2wd.4) <- 'd2wd'
  df.d2wd.4 <- d2wd.4 %>% as.data.frame(na.rm=TRUE)
  df.d2wd.4$year <- 2023
  
  remove(d2wd.1, d2wd.2, d2wd.3, d2wd.4)
  gc()
  
  df.d2wd <- bind_rows(df.d2wd.1, df.d2wd.2, df.d2wd.3, df.d2wd.4) %>%
    mutate(d2wdclass = cut(d2wd, breaks=c(-1,0.082,0.271,1), labels=c('0-0.082','0.082-0.271','0.271-1')))
  
  remove(df.d2wd.1, df.d2wd.2, df.d2wd.3, df.d2wd.4)
  
  d2wd.stats <- df.d2wd %>%
    group_by(d2wdclass, year) %>% 
    arrange(year, .by_group = TRUE) %>%
    summarise(n=n()) %>%
    mutate(area = n*9*0.0001) %>%
    mutate(pct_change = (area-lag(area))/lag(area) * 100)
  
  write.csv(d2wd.stats, paste('d2wd_stats_masked_', f, '_v2.csv', sep=''))
  
  gc()
  
  remove(df.d2wd, d2wd.stats)
  gc()
  
})




