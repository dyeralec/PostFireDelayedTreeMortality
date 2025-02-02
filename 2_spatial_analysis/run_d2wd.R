# libraries
library(dplyr)
library(tidyverse)
library(tidyterra)
library(terra)

# settings
fires <- c('riverside', 'beachiecreek', 'lionshead', 'holidayfarm', 'archiecreek')

fire.lookup <- c('riverside'='riverside', 'beachiecreek'='beachie_creek', 'lionshead'='lionshead', 'holidayfarm'='holiday_farm', 'archiecreek'='archie_creek')
fire.lookup.cap <- c('riverside'='Riverside', 'beachiecreek'='BeachieCreek', 'lionshead'='Lionshead', 'holidayfarm'='HolidayFarm', 'archiecreek'='ArchieCreek')

df.d2wd <- purrr::map(fires, function(f) {
  
  print(f)
  
  d2wd.1 <- rast(paste(f, '_2020_d2wd_masked.tif', sep=''))
  d2wd.2 <- rast(paste(f, '_2021_d2wd_masked.tif', sep=''))
  d2wd.3 <- rast(paste(f, '_2022_d2wd_masked.tif', sep=''))
  d2wd.4 <- rast(paste(f, '_2023_d2wd_masked.tif', sep=''))
  
  names(d2wd.1) <- 'd2wd.1'
  names(d2wd.2) <- 'd2wd.2'
  names(d2wd.3) <- 'd2wd.3'
  names(d2wd.4) <- 'd2wd.4'
  
  sev <- rast(paste(fire.lookup.cap[f], '_SevMask_2020.tif', sep='')) %>% resample(d2wd.1, method='near')
  names(sev) <- 'sev'
  
  gc()
  
  d2wd.1 <- mask(d2wd.1, sev, maskvalue=2)
  d2wd.2 <- mask(d2wd.2, sev, maskvalue=2)
  d2wd.3 <- mask(d2wd.3, sev, maskvalue=2)
  d2wd.4 <- mask(d2wd.4, sev, maskvalue=2)
  
  gc()
  
  df.d2wd <- c(
    as.data.frame(d2wd.1, na.rm=TRUE),
    as.data.frame(d2wd.2, na.rm=TRUE),
    as.data.frame(d2wd.3, na.rm=TRUE),
    as.data.frame(d2wd.4, na.rm=TRUE)
  ) %>% bind_cols()
  
  df.d2wd$fire <- f
  
  remove(d2wd.1, d2wd.2, d2wd.3, d2wd.4, sev)
  gc()
  
  return(df.d2wd)
  
}) %>% bind_rows

# Save to file
save(df.d2wd, file='d2wd_highsev_v2.RData')