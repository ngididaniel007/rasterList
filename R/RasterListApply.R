# TODO: Add comment
# 
# Author: cordaem
###############################################################################
NULL
#' 
#' A function for operations among  \code{\link{RasterList-class}} objects. 
#'
#' @param ... a set of arguments containg the \code{\link{RasterList-class}} objects whose lists are operated by \code{FUN}. 
#' @param FUN a function 
#' 
#' @export 
#' @return a \code{\link{RasterList-class}} object
#' 
#' @seealso \code{\link{mapply}},\code{\link{rasterList}}
#' 
#' @examples 
#' 
#' 
#' 
#' f <- system.file("external/test.grd", package="raster")
#' 
#' ra <- rasterList(f)
#' rb <- rasterList(f)
#' 
#' rm <- RasterListApply(x=ra,y=rb,z=10,FUN=function(x,y,z){x+y+z})
#' 
#' ### Fitting a probability distribution for precipitation 
#' ### in each cell with "lmon" package (L Moments) 
#' library(lmom)
#' 
#' 
#' # The package-provided datasets shall be only used as example datasets. 
#' precf <- system.file("map/Mekrou_precipitation.grd", package="rasterList") ## 
#' ## A resampled preciptation raster map based on CHIRS dataset:
#' ## Funk, Chris, Pete Peterson, Martin Landsfeld, Diego Pedreros, James Verdin, 
#' ## Shraddhanand Shukla, Gregory Husak, James Rowland, Laura Harrison, 
#' ## Andrew Hoell and Joel Michaelsen.
#' ## "The climate hazards infrared precipitation with stations -a new environmental 
#' ## record for monitoring extremes". Scientific Data 2, 150066. doi:10.1038/sdata.2015.66 2015. 
#' ## http://chg.geog.ucsb.edu/data/chirps/
#' ##
#' prec <- stack(precf)
#' ## Sample L-moments 
#' samlmom <- stack(rasterList(prec,FUN=samlmu))
#' ## Fitting a Random Probability Distribution: it is a 'rasterList' Object
#' fitdist <- rasterList(samlmom,FUN=pelgam)
#' 
#' ## KS TESTING
#' 
#' kstesting <- RasterListApply(x=rasterList(prec),y="cdfgam",para=fitdist,FUN=ks.test)
#' 
#' ## Mapping of p-value 
#' pval_ks <- raster(kstesting,FUN=function(x){x$p.value})

RasterListApply <- function(...,FUN=NULL) {
	
    ## TO DO ....
	l <- list(...)
	if (length(l)==1) l <- l[[1]]
	
	if (is.null(FUN)) FUN <- l$FUN 
	
	l$FUN <- FUN
	
	
	il <- which(sapply(X=l,FUN=function(x){class(x)=="RasterLayer"}))
	l[il] <- lapply(X=l[il],FUN=function(x){rasterList(stack(x))})
	
	
	iw <- which(sapply(X=l,FUN=is.RasterList))
	
	oo <- sapply(X=l[iw],FUN=function(x){length(x@list)})


	cond <- all(oo==oo[1])


	if (length(iw)<2) {
		
		stop("At least two arguments must be RasterList Objects")
		
	}
	
	if (cond==FALSE) {
		
		mm <- sapply(X=l[iw],FUN=function(x){x@name})
		mm <- paste(mm,collapse=" ")
		mm <- sprintf("Mismatch among rasterLists %s !!!",mm)
		stop(mm)
		
	}
	
	out <- l[[iw[1]]]
	
	l[iw] <- lapply(X=l[iw],FUN=function(x){x@list})
	l$SIMPLIFY <- FALSE
	out@list <- do.call(what=mapply,args=l)
	

	
	
	
	
	out <- rasterList(out)
	
	
	
	return(out)
	
	
	
	
	
}



