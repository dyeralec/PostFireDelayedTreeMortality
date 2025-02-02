# Image Functions
## Created by: Alec Dyer

# Functions for rescaling layers/stacks
## https://gist.github.com/scbrown86/795b51aaa8421439c75deb92f155906d
## Cleaned up/edited the source code for sdvmspecies library (https://cran.r-project.org/web/packages/sdmvspecies/index.html)

rescaleLayer <- function(raster.layer, new.min, new.max) {
  # new.min <- 0; new.max <- 1 ## change for different output range (e.g. 0-255)
  min.value <- cellStats(raster.layer, min, na.rm = TRUE)
  max.value <- cellStats(raster.layer, max, na.rm = TRUE)
  if (min.value == max.value) {
    stop("min.value and max.value are the same. Cannot rescale a layer with no variation.")
  }
  raster.layer <- new.min + (raster.layer - min.value) * ((new.max - new.min) / (max.value - min.value))
  return(raster.layer)
}

rescaleStack <- function(raster.stack, new.min, new.max) {
  raster.name <- names(raster.stack)
  raster.stack.list <- lapply(X = raster.name, FUN = function(name, raster.stack) {
    return(rescaleLayer(raster.stack[[name]], new.min, new.max))}, raster.stack)
  result.stack <- stack(raster.stack.list)
  names(result.stack) <- raster.name
  return(result.stack)
}

rescale <- function(raster.object, new.min=0, new.max=1) {
  if (!(class(raster.object) %in% c("RasterLayer", "RasterStack"))) {
    stop("raster.object needs to be a RasterLayer or RasterStack object!\nraster.object is a ", class(raster.object)[1])
  }
  if (class(raster.object) %in% "RasterLayer") {
    raster.object <- rescaleLayer(raster.object, new.min, new.max)
  } else {
    raster.object <- rescaleStack(raster.object, new.min, new.max)
  }
  return(raster.object)
}

# Adjusted for terra
rescaleLayer_terra <- function(raster.layer, new.min, new.max) {
  # new.min <- 0; new.max <- 1 ## change for different output range (e.g. 0-255)
  min.value <- minmax(raster.layer)[1]
  max.value <- minmax(raster.layer)[2]
  if (min.value == max.value) {
    stop("min.value and max.value are the same. Cannot rescale a layer with no variation.")
  }
  raster.layer <- new.min + (raster.layer - min.value) * ((new.max - new.min) / (max.value - min.value))
  return(raster.layer)
}

rescaleStack_terra <- function(raster.stack, new.min, new.max) {
  raster.name <- names(raster.stack)
  raster.stack.list <- lapply(X = raster.name, FUN = function(name, raster.stack) {
    return(rescaleLayer_terra(raster.stack[[name]], new.min, new.max))}, raster.stack)
  result.stack <- rast(raster.stack.list)
  names(result.stack) <- raster.name
  return(result.stack)
}

rescaleTerra <- function(raster.object, new.min=0, new.max=1) {
  if (!(class(raster.object) %in% c("SpatRaster"))) {
    stop("raster.object needs to be a SpatRaster object!\nraster.object is a ", class(raster.object)[1])
  }
  if (nlyr(raster.object) > 1) {
    raster.object <- rescaleStack_terra(raster.object, new.min, new.max)
  } else {
    raster.object <- rescaleLayer_terra(raster.object, new.min, new.max)
  }
  return(raster.object)
}

# Indices
## NDVI
calc_ndvi <- function(x) {
  (x[4] - x[1]) / (x[4] +  x[1])
}

# Green Normalized Difference Moisture Index (GNDMI)
calc_gndmi <- function(x) {
  (x[4] - x[2]) / (x[4] +  x[2])
}

## Red-Green Index
calc_rgi <- function(x) {
  (x[1] - x[2]) / (x[1] +  x[2])
}

## Brightness - 3 band
tcb <- function(x) {
  mean(x[1:3])
}

# False-color brighness using red, blue, and near-infrared
calc_fcb <- function(x) {
  mean(x[c(1,3,4)])
}

# Greyscale - weights determined from ArcGIS Pro's Greyscale formula
greyscale <- function(x) {
  ((x[1]*0.299) + (x[2]*0.587) + (x[3]*0.114)) / (0.299 + 0.587 + 0.114)
}


## Local Minima
## Rodman et al., 2019
calc_local_minima <- function(r) {
  # window sizes
  w <- c(3, 5, 7, 9, 11, 13, 15) 
  
  # calculate brightness
  r.tcb <- calc(r, tcb)
  
  # iterate over windows and create raster stack with means
  st.mean <- stack()
  for (s in w) {
    # focal function to apply sliding window
    tcb.w <- focal(r.tcb, w=matrix(1, ncol=s, nrow=s), fun=mean)
    # add to stack
    st.mean <- stack(st.mean, tcb.w)
  }
  
  # iterate over windows and create raster stack with standard deviation
  st.sd <- stack()
  for (s in w) {
    # focal function to apply sliding window
    tcb.w <- focal(r.tcb, w=matrix(1, ncol=s, nrow=s), fun=sd)
    # add to stack
    st.sd <- stack(st.sd, tcb.w)
  }
  
  # binary classification of dark (1) or bright (0) for each window size
  # where the brightness is less than two standard deviations below the mean
  dark <- function(x) {
    ifelse(x[1] < (x[2] - x[3]*2), 1, 0)
  }
  st.dark <- stack()
  for (i in 1:length(w)) {
    r.dark <- calc(stack(r.tcb, st.mean[[i]], st.sd[[i]]), fun=dark)
    st.dark <- stack(st.dark, r.dark)
  }
  
  # sum all bands in st.dark to raster with values ranging from
  # 0 (lightest areas) to 7 (darkest areas)
  local.minima <- calc(st.dark, sum)
  
  return(local.minima)
}

# Local Minima for Terra SpatRaster
calc_local_minima_terra <- function(r) {
  # load libraries
  library(sf)
  library(terra)
  library(tidyverse)
  
  ## Correcting an error check issue with some of the polygons
  sf_use_s2(FALSE)
  
  # window sizes
  w <- c(3, 5, 7, 9, 11, 13, 15) 
  
  # calculate brightness
  r.tcb <- app(r, tcb)
  
  st.mean <- lapply(1:length(w), function(x){
    # focal function to apply sliding window
    return(focal(r.tcb, w=w[x], fun=mean))
  })
  
  # iterate over windows and create layered SpatRaster with standard deviation
  st.sd <- lapply(1:length(w), function(x){
    # focal function to apply sliding window
    return(focal(r.tcb, w=w[x], fun=sd))
  })
  
  # binary classification of dark (1) or bright (0) for each window size
  # where the brightness is less than two standard deviations below the mean
  dark <- function(x) {
    # x[1] is brightness -- the mean value across R,G,B bands
    # x[2] is mean brightness for the window size
    # x[3] is the standard deviation in brightness for the window size
    ifelse(x[1] < (x[2] - (x[3]*2)), 1, 0)
  }
  st.dark <- rast(lapply(1:length(w), function(x){
    return(app(rast(list(r.tcb, st.mean[[x]], st.sd[[x]])), fun=dark))
  }))
  
  # sum all bands in st.dark to raster with values ranging from
  # 0 (lightest areas) to 7 (darkest areas)
  local.minima <- app(st.dark, sum)
  
  return(local.minima)
}

# Local Minima for Terra SpatRaster with Parallel Processing
calc_local_minima_terra_parallel <- function(r, n.cores=1) {
  library(parallel)
  library(terra)
  
  # window sizes
  w <- c(3, 5, 7, 9, 11, 13, 15) # , 17, 19, 21, 23, 35, 27, 29, 31 
  
  # calculate brightness
  b <- wrap(terra::app(r, tcb))
  
  # make cluster
  cl <- makeCluster(n.cores)
  # export needed data to clusters
  clusterExport(cl, varlist = c('b', 'w'), envir=environment())
  # open libraries in clusters
  clusterEvalQ(cl, {
    library(terra)
    library(parallel)
  })
  
  st.dark <- parLapply(cl, 1:length(w), function(i) {
    
    b <- rast(b)
    
    # means
    st.mean <- terra::focal(b, w=w[i], fun=mean)
    
    # standard deviation
    st.sd <- terra::focal(b, w=w[i], fun=sd)
    
    # binary classification of dark (1) or bright (0) for each window size
    # where the brightness is less than two standard deviations below the mean
    dark <- function(x) {
      ifelse(x[1] < (x[2] - (x[3]*2)), 1, 0)
    }

    r.dark <- terra::app(rast(list(b, st.mean, st.sd)), fun=dark)

    return(wrap(r.dark))
    
  })

  stopCluster(cl)
  
  st.dark <- lapply(st.dark, unwrap)

  st.dark <- rast(st.dark)
  
  # sum all bands in st.dark to raster with values ranging from
  # 0 (lightest areas) to 7 (darkest areas)
  local.minima <- terra::app(st.dark, sum)
  
  return(local.minima)
  
}

## Sum of Standard Deviations
## Rodman et al., 2019
calc_sum_sd <- function(r) {
  # window sizes
  w <- c(3, 5, 7, 9, 11, 13, 15) 
  
  # calculate tasseled cap brightness
  r.tcb <- calc(r, tcb)
  
  # iterate over windows and create raster stack with standard deviation
  st.sd <- stack()
  for (s in w) {
    # focal function to apply sliding window
    tcb.w <- focal(r.tcb, w=matrix(1/(s*s), ncol=s, nrow=s), fun=sd)
    # add to stack
    st.sd <- stack(st.sd, tcb.w)
  }
  
  # sum all bands in st.sd
  sum.sd <- calc(st.sd, sum)
  
  return(sum.sd) 
}

calc_sum_sd_terra <- function(r) {
  # load libraries
  library(sf)
  library(terra)
  library(tidyverse)
  
  # Correcting an error check issue with some of the polygons
  sf_use_s2(FALSE)
  
  # window sizes
  w <- c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 35, 27, 29, 31)
  
  # calculate tasseled cap brightness
  r.tcb <- app(r, tcb)
  
  # iterate over windows and create layered SpatRaster with standard deviation
  st.sd <- lapply(1:length(w), function(x){
    # focal function to apply sliding window
    return(focal(r.tcb, w=w[x], fun=sd))
  })
  
  st.sd <- rast(st.sd)
  
  # sum all bands in st.sd
  sum.sd <- app(st.sd, sum)
  
  return(sum.sd) 
}

calc_sum_sd_terra_parallel <- function(r, n.cores) {
  library(parallel)
  library(terra)
  
  # window sizes
  w <- c(3, 5, 7, 9, 11, 13, 15) # , 17, 19, 21, 23, 35, 27, 29, 31 
  
  # calculate brightness
  b <- wrap(terra::app(r, tcb))
  
  # make cluster
  cl <- makeCluster(n.cores)
  # export needed data to clusters
  clusterExport(cl, varlist = c('b', 'w'), envir=environment())
  # open libraries in clusters
  clusterEvalQ(cl, {
    library(terra)
    library(parallel)
  })
  
  st.sd <- parLapply(cl, 1:length(w), function(i) {
    
    b <- rast(b)
    
    # standard deviation
    s <- terra::focal(b, w=w[i], fun=sd)
    
    return(wrap(s))
    
  })
  
  stopCluster(cl)
  
  st.sd <- lapply(st.sd, unwrap)
  
  st.sd <- rast(st.sd)
  
  # sum all bands
  sum.sd <- terra::app(st.sd, sum)
  
  return(sum.sd)
  
}

## Radiometric Normalization
## Relative Radiometric Normalization for Single Band
relNormalizeSB <- function(i_tbn, i_b, pts) {
  #extract Pseudo Invariant Feature (PIF) Points from base and target images
  base <- raster::extract(i_b, pts)
  target <- raster::extract(i_tbn, pts)
  #Apply linear regression on PIFs
  Regression <- lm( base ~ target);
  #Get coefficient from the linear regression results
  targetcoefficient <- summary(Regression)$coefficients[2,1];
  intercept <- summary(Regression)$coefficients[1,1];
  #Compute coefficients for target and intercept that minimizes error in targetcoefficient*i_tbn + intercept = i_b
  i_n <- i_tbn*targetcoefficient+intercept
  return (i_n)
}
## Relative Radiometric Normalization for Multi-Band
relNormalizeMB<- function(i_tbn, i_b, pts, n){
  rni <- relNormalizeSB(i_tbn[[1]], i_b[[1]], pts);
  # Iterate over the rest of the bands
  for (b in 2:n) {
    rni = addLayer(rni, relNormalizeSB(i_tbn[[b]], i_b[[b]], pts))
  }
  return (rni)
}

## Radiometric Normalization for Terra package
## Relative Radiometric Normalization for Single Band
relNormalizeSB_terra <- function(i_tbn, i_b, pts) {
  #extract Pseudo Invariant Feature (PIF) Points from base and target images
  base <- terra::extract(i_b, pts, ID=FALSE)
  names(base) <- 'base'
  target <- terra::extract(i_tbn, pts, ID=FALSE)
  names(target) <- 'target'
  df <- cbind(base, target)
  #Apply linear regression on PIFs
  Regression <- lm( base ~ target, df)
  #Get coefficient from the linear regression results
  targetcoefficient <- summary(Regression)$coefficients[2,1]
  intercept <- summary(Regression)$coefficients[1,1]
  #Compute coefficients for target and intercept that minimizes error in targetcoefficient*i_tbn + intercept = i_b
  i_n <- i_tbn*targetcoefficient+intercept
  return (i_n)
}
## Relative Radiometric Normalization for Multi-Band
relNormalizeMB_terra <- function(i_tbn, i_b, pts, n){
  my_function <- function(i, tbn, b, pts){
    gc()
    relNormalizeSB_terra(tbn[[i]], b[[i]], pts)
  }
  
  indices <- c(1,2,3,4)
  
  rni <- lapply(indices, my_function, tbn=i_tbn, b=i_b, pts=pts)
  
  gc()
  
  return(rni)
  
}
