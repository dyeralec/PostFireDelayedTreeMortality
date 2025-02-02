# libraries
library(dplyr)
library(tidyverse)
library(tidyterra)
library(terra)

# settings
fires <- c('riverside', 'beachiecreek', 'lionshead', 'holidayfarm', 'archiecreek')

fire.lookup <- c('riverside'='riverside', 'beachiecreek'='beachie_creek', 'lionshead'='lionshead', 'holidayfarm'='holiday_farm', 'archiecreek'='archie_creek')
fire.lookup.cap <- c('riverside'='Riverside', 'beachiecreek'='BeachieCreek', 'lionshead'='Lionshead', 'holidayfarm'='HolidayFarm', 'archiecreek'='ArchieCreek')

df.dist <- purrr::map(fires, function(f) {
  
  print(f)
  
  d.1 <- rast(paste(f, '_2020_dist_masked.tif', sep=''))
  d.2 <- rast(paste(f, '_2021_dist_masked.tif', sep=''))
  d.3 <- rast(paste(f, '_2022_dist_masked.tif', sep=''))
  d.4 <- rast(paste(f, '_2023_dist_masked.tif', sep=''))
  
  names(d.1) <- 'd.1'
  names(d.2) <- 'd.2'
  names(d.3) <- 'd.3'
  names(d.4) <- 'd.4'
  
  sev <- rast(paste(fire.lookup.cap[f], '_SevMask_2020.tif', sep='')) %>% resample(d.1, method='near')
  names(sev) <- 'sev'
  
  gc()
  
  d.1 <- mask(d.1, sev, maskvalue=2)
  d.2 <- mask(d.2, sev, maskvalue=2)
  d.3 <- mask(d.3, sev, maskvalue=2)
  d.4 <- mask(d.4, sev, maskvalue=2)
  
  gc()
  
  df <- c(
    as.data.frame(d.1, na.rm=TRUE),
    as.data.frame(d.2, na.rm=TRUE),
    as.data.frame(d.3, na.rm=TRUE),
    as.data.frame(d.4, na.rm=TRUE)
  ) %>% bind_cols()
  
  df$fire <- f
  
  remove(d.1, d.2, d.3, d.4, sev)
  gc()
  
  return(df)
  
}) %>% bind_rows

# Save to file
save(df.dist, file='dist_highsev_v2.RData')