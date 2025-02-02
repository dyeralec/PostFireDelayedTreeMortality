# libraries
library(terra)
library(dplyr)
library(furrr)
library(stringr)

setwd('..04_SpatialAnalysis/Isolation_HPC')

# globals
ncores <- 2
set.seed(5)
w <- 801
out.dir <- '..isolation/run4/'

# functions
## mosaic raster
combine_raster_chunks<- function(x, buffer){
  for (i in 1:length(x)) {
    if(i!=1){x[[i]]<- x[[i]][(buffer+1):nrow(x[[i]]), , , drop=FALSE]} #Trim top of chunk
    if(i!=length(x)){x[[i]]<- x[[i]][1:(nrow(x[[i]])-buffer), , , drop=FALSE]} #Trim bottom of chunk
  }
  out<- do.call(merge, x)
  return(out)
}
# create burn severity mask
create_sev_mask <- function(fc.s, data.mask=NA) {
  # aggregate to 30m and calculate percent of area that is not forest cover
  fc.s.30 <- 1 - terra::aggregate(fc.s, fact=10, fun=mean, na.rm=TRUE)
  # classify into high severity (1) and low/moderate severity (0) using 75% cut off
  fc.s.sev <- classify(fc.s.30, c(0,0.75,1), right=NA, include.lowest=TRUE)
  # disaggregate back to 3m resolution
  fc.s.sev.3 <- terra::disagg(fc.s.sev, fact=10, method='near') %>% 
    # resample back to original grid
    resample(fc.s, method='near') %>%
    # convert to numeric
    catalyze()
  
  gc()
  
  return(fc.s.sev.3)
}

################################################################################

# list of rasters to loop over
rasters <- c(
  'riverside_2020.tif', 'riverside_2021.tif', 'riverside_2022.tif', 'riverside_2023.tif', 
  'beachiecreek_2020.tif', 'beachiecreek_2021.tif', 'beachiecreek_2022.tif', 'beachiecreek_2023.tif',
  'lionshead_2020.tif', 'lionshead_2021.tif', 'lionshead_2022.tif', 'lionshead_2023.tif', 
  'holidayfarm_2020.tif', 'holidayfarm_2021.tif', 'holidayfarm_2022.tif', 'holidayfarm_2023.tif', 
  'archiecreek_2020.tif', 'archiecreek_2021.tif', 'archiecreek_2022.tif', 'archiecreek_2023.tif'
)

for (ras in rasters) {
  print(ras)
  
  # load data
  fc <- rast(ras)
  
  # create severity mask
  sev.mask <- create_sev_mask(fc)
  
  # prepare forest cover
  m <- sev.mask %>% subst(2,NA) %>% buffer(400, background=2) %>% subst(1, 0)
  fc.s <- ifel(m == 0, fc, NA)
  fc.s.d <- ifel(m == 0, fc, NA) %>% subst(0, NA) %>% mask(fc, maskvalue=NA, updatevalue=0)
  gc()
  
  ## generate the weights matrices using these steps
  ## 1. make a raster to use in distance function. this one applies to the 1 m tree data.
  m1t <- rast(xmin=0, xmax=w*3, ymin=0, ymax=w*3, crs=crs(fc.s), nrows=w, ncols=w)
  m1t[ceiling(w/2), ceiling(w/2)] <- 1 ## all values are na except the center cell = 1
  ## 2. now run the raster distance function
  m1td <- distance(m1t)
  ## 3. then convert the raster to a matrix and use the values in the distance, distance^2 calc
  wts1 <- 1/(as.matrix(m1td, wide=T) + 1) ## matrix w euclid dist
  wts2 <- 1/(as.matrix(m1td^2, wide=T) + 1) ## matrix with dist ^2
  
  # data-prep
  # Create breaks for chunking raster
  buffer<- (w-1)/2 # Buffer for focal window
  breaks<- data.frame(write_start = ceiling(seq(1, nrow(fc.s)+1, length.out = ncores+1)))
  breaks<- breaks %>% mutate(write_end=lead(write_start, n=1)-1)
  breaks<- breaks %>% mutate(chunk_start=write_start - buffer)
  breaks<- breaks %>% mutate(chunk_end = write_end + buffer)
  breaks<- breaks[-nrow(breaks),]
  breaks$chunk_end[breaks$chunk_end > nrow(fc.s)]<- nrow(fc.s)
  breaks$chunk_start[breaks$chunk_start < 1]<- 1
  
  # Put raster chunks in list
  r_list<- vector(mode = "list", length = ncores)
  for (i in 1:ncores) {
    r_list[[i]]<- wrap(fc.s[breaks$chunk_start[i]:breaks$chunk_end[i], ,drop=FALSE])
  } #SpatRasters need to be wrapped before sending out to different cores
  
  focal_parallel<- function(x, ...){
    wrap(focal(unwrap(x),...)) #unwrap for processing and then wrap output
  }
  
  # run isolation
  # Distance
  print('Distance')
  t.d <- system.time({ ## begin time keeping loop.
    d <- distance(fc.s.d) %>% mask(fc)
    writeRaster(d, paste(out.dir, str_replace(ras, '.tif', '_dist.tif'), sep=''), overwrite=TRUE) # save
  }) # end loop
  
  print(t.d)
  
  # D2WD
  print('D2WD')
  t.d2wd <- system.time ( {
    plan(strategy = "multisession", workers=ncores) # Set up parallel
    out_list <- future_map(r_list, .f = focal_parallel, fun= "sum", w=wts2, na.rm=TRUE, na.policy='omit')
    plan(strategy = "sequential")
    out_list <- lapply(out_list, unwrap) # unwrap chunks
    d2wd <- combine_raster_chunks(out_list, buffer) # merge chunks into single raster
    writeRaster(d2wd, paste(out.dir, str_replace(ras, '.tif', '_d2wd.tif'), sep=''), overwrite=TRUE) # save
    gc()
  })
  
  print(t.d2wd)
  
  remove(fc, fc.s, sev.mask, d, dwd, d2wd, r_list)
  gc()
  
}
