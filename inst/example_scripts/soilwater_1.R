#### /usr/bin/Rscript

# file soilwater_1.R
#
# This file  is an application of "rasterListFun" : how to cresate a raster of soil water content function. 
#
# author: Emanuele Cordano on 03-07-2017
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.

###############################################################################
library(soilwater)

library(sp)
library(rasterList)
library(soilwater)

##source('/STORAGE/projects/R-Packages/rasterList/R/is.RasterList.R') 
##source('/STORAGE/projects/R-Packages/rasterList/R/rasterListFun.R') 
##source('/STORAGE/projects/R-Packages/rasterList/R/RasterListApply.R') 
##source('/STORAGE/projects/R-Packages/rasterList/R/raster.R') 
set.seed(1234)
data(meuse.grid)
data(meuse)
coordinates(meuse.grid) <- ~x+y
coordinates(meuse) <- ~x+y
gridded(meuse.grid) <- TRUE


soilmap <- stack(meuse.grid)[['soil']]
elevmap <- rasterize(x=meuse,y=soilmap,field="elev",fun=mean)
soilpar <- read.table(system.file("external/soil_data.csv",package="soilwater"),stringsAsFactors=FALSE,header=TRUE,sep=",")
## From help(meuse,help_type="html")
##soil type according to the 1:50 000 soil map of the Netherlands. 
## 1 = Rd10A (Calcareous weakly-developed meadow soils, light sandy clay); 
## 2 = Rd90C/VII (Non-calcareous weakly-developed meadow soils, heavy sandy clay to light clay); 
## 3 = Bkd26/VII (Red Brick soil, fine-sandy, silty light clay)
soiltype_id <- c(1,2,3)
soiltype_name <- c("sandy clay","sandy clay","silty clay loam")

meuse.soilrasterlist <- rasterList(soilmap,FUN=function(i,soiltype_name,soilpar){
			
			o <- NULL
			if (!is.na(i)) {
				
				o <- soilpar[which(soilpar$type==soiltype_name[i]),]				
				type <- o[["type"]]
				o <- o[names(o)!="type"]
				o <- o[names(o)!="Ks_m_per_hour"]
				names(o)[names(o)=="Ks_m_per_sec"] <- "ks"
				names(o)[names(o)=="swc"] <- "theta_sat"
				names(o)[names(o)=="rwc"] <- "theta_res"
				attr(o,"type") <- type
				## add noise
				noise <- rnorm(length(o))
				o <- o*(1+0.005*noise)
				
			    o["m"] <- 1-1/o["n"]
				 
				
 			} else {
				
				o <- soilpar[which(soilpar$type==soiltype_name[1]),]
				type <- o[["type"]]
				o <- o[names(o)!="type"]
				o <- o[names(o)!="Ks_m_per_hour"]
				names(o)[names(o)=="Ks_m_per_sec"] <- "ks"
				names(o)[names(o)=="swc"] <- "theta_sat"
				names(o)[names(o)=="rwc"] <- "theta_res"
				o[] <- NA
			}
			
			return(o)
		},soiltype_name=soiltype_name,soilpar=soilpar)


meuse.swclist <- rasterList(meuse.soilrasterlist,FUN=function(x) {
			
			o <- NA
#			swc       rwc   alpha       n         m           ks
#			9 0.4295507 0.1093227 3.39387 1.39617 0.2837546 2.018317e-07
#			
			
			o <- function(psi,...,func="swc"){
				
				args <- c(list(psi=psi,...),as.list(x))
				oo <- do.call(args=args,what=get(func))
				return(oo)
			
			}
				
	
				
				
			
			
			
			return(o)
			
		})


### RasterList with soil water retenction curves (One for each cell!) 

swcfunr <- rasterListFun(meuse.swclist)

## RasterLayer of soil water content assuming a uniformly distrrubted pressure head 
psi <- -0.9
soil_water_content <- raster(swcfunr(psi))
plot(soil_water_content)

## RasterLayer of soil water content from  a generic map of soil water pressure head
psi <- 0.2-(elevmap-(5))
psi[] <- -0.9+0.1*rnorm(ncell(psi[])) ## Alternatively to the values of the previous line!
soil_water_content <- raster(swcfunr(psi))
plot(soil_water_content)

###
###
###
###

