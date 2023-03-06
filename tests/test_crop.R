# This is a test script for crop method
# 
# Author: Emanuele Cordano
###############################################################################
rm(list=ls())

library(rasterList)
library(lmom)



## TESTING R CODE: 
library(testthat)
context("Verify cropping a RasterList")

## UNDER DEVOLOPMENT


##precf <- system.file("map/precipitation.grd", package="rasterList")
precf <- system.file("map/Mekrou_precipitation.grd", package="rasterList")
prec <- stack(precf)
## Sample L-moments 

library(lmom)
samlmom <- stack(rasterList(prec,FUN=samlmu))
## Fitting a Random Probability Distribution: it is a 'rasterList' Object
fitdist <- rasterList(samlmom,FUN=pelgam)

##### ZOOM IN 
## set a mask 
mask <-raster( extent(fitdist)/4 )


fitdist_masked <- crop ( x = fitdist,y=mask)

####
prec_crop <-  crop(x= prec , y= mask )
samlmom_crop <- stack(rasterList(prec_crop,FUN=samlmu))
fitdist_crop <- rasterList(samlmom_crop,FUN=pelgam)

test_that(desc="Testing final Results",code=expect_equal(fitdist_crop@list,fitdist_masked@list, tolerance = .002, scale = 1))
test_that(desc="Testing final Results",code=expect_equal(fitdist_crop@extent,fitdist_masked@extent, tolerance = .002, scale = 1))
