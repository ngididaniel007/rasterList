## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo=TRUE,
  warning = FALSE
)
###knitr::opts_chunk$set(echo = TRUE)

## ----eval=TRUE,fig.width=7----------------------------------------------------
library(soilwater)
library(stringr)
soilparcsv <- system.file("external/soil_data.csv",package="soilwater")
soilpar <- read.table(soilparcsv,stringsAsFactors=FALSE,header=TRUE,sep=",")
soilpar$color <- str_sub(rainbow(nrow(soilpar)),end=7)  ## Only first 7 characters of HTML code is considered.

## ----eval=TRUE,fig.width=7----------------------------------------------------
library(rasterList)

## ----eval=TRUE,fig.width=7----------------------------------------------------
library(sp)
data(meuse)
help(meuse)
data(meuse.grid)
help(meuse.grid)

## ----eval=TRUE,fig.width=7----------------------------------------------------
soiltype_id <- c(1,2,3)
soiltype_name <- c("sandy clay","clay","silty clay loam")
## Then
soilpar_s <- soilpar[soilpar$type %in% soiltype_name,]
is <- order(soilpar_s[,1],by=soiltype_name)
soilpar_s <- soilpar_s[is,]
soilpar_s$id <- soiltype_id

## ----eval=TRUE, fig.width=7---------------------------------------------------
library(rasterList)
library(soilwater)
library(leaflet)
set.seed(1234)
data(meuse.grid)
data(meuse)
coordinates(meuse.grid) <- ~x+y
coordinates(meuse) <- ~x+y
gridded(meuse.grid) <- TRUE
### +init=epsg:28992
proj4string(meuse) <- CRS("+init=epsg:28992")
proj4string(meuse.grid) <- CRS("+init=epsg:28992")
## Not run: 
soilmap <- as.factor(stack(meuse.grid)[['soil']])
###elevmap <- rasterize(x=meuse,y=soilmap,field="elev",fun=mean)

ref_url <- "https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png"
ref_attr <- 'Map data: &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>, <a href="http://viewfinderpanoramas.org">SRTM</a> | Map style: &copy; <a href="https://opentopomap.org">OpenTopoMap</a> (<a href="https://creativecommons.org/licenses/by-sa/3.0/">CC-BY-SA</a>)'
opacity <- 0.6
color <- colorFactor(soilpar_s$color,level=soilpar_s$id)
labFormat <- function(x,values=soilpar_s$type){values[as.integer(x)]}

leaf1 <- leaflet() %>% addTiles(urlTemplate=ref_url,attribution=ref_attr) 
leaf2 <- leaf1 %>% addRasterImage(soilmap,opacity=opacity,color=color) %>% ##addLegend(position="bottomright",pal=color,values=soilpar_s$id,labels=soilpar_s$type,title="Soil Type",labFormat=labFormat)
addLegend(position="bottomright",colors=soilpar_s$color,labels=soilpar_s$type,title="Soil Type",opacity=opacity)

leaf2

## ----eval=TRUE,fig.width=7----------------------------------------------------

soil_parameters_f <- function (soiltype,sp=soilpar_s) {
  o <- sp[soiltype,c("swc","rwc","alpha","n","m")]
  names(o) <- c("theta_sat","theta_res","alpha","n","m")
  return(o)
  }
soil_parameters_rl <- rasterList(soilmap,FUN=soil_parameters_f)

## ----eval=TRUE,fig.width=7----------------------------------------------------
soil_parameters <- stack(soil_parameters_rl)
soil_parameters

## ----eval=TRUE,fig.width=7,warning=FALSE--------------------------------------
theta_sat <- soil_parameters[["theta_sat"]]
color <- colorNumeric("Greens",domain=theta_sat[])

leaf3 <- leaf1 %>% addRasterImage(theta_sat,color=color,opacity=opacity) %>% 
addLegend(position="bottomright",pal=color,values=theta_sat[],opacity=opacity,title="Porosity")

## ----eval=TRUE, fig.width=7---------------------------------------------------
lat <- 50.961532+c(0,0,0.02)
lon <-  5.724514+c(0,0.01,0.0323)
name <- c("A","B","C")

points <- data.frame(lon=lon,lat=lat,name=name)
print(points)
coordinates(points) <- ~lon+lat
proj4string(points) <- CRS("+proj=longlat  +ellps=WGS84") 
leaf3 %>% addMarkers(lng=points$lon,lat=points$lat,label=points$name)


## ----eval=TRUE,fig.width=7----------------------------------------------------

points$icell <- cellFromXY(soil_parameters,spTransform(points,projection(soil_parameters)))


## ----eval=TRUE,fig.width=7----------------------------------------------------
soil_parameters[points$icell]

## ----eval=TRUE,fig.width=7----------------------------------------------------
library(soilwater)

swc_func <- function(x,...) {
         o <- function(psi,y=x,...) {
         
              args <- c(list(psi,...),as.list(y))
              oo <- do.call(args=args,what=get("swc"))
         }     
        return(o)
}

## ----eval=TRUE,fig.width=7----------------------------------------------------
soilparA <- soil_parameters[points$icell[1]][1,]
swcA <- swc_func(soilparA)


## ----eval=TRUE,fig.width=7----------------------------------------------------
library(ggplot2)
psi <- seq(from=-5,to=1,by=0.25)
title <- "Soil Water Retention Curve at Point A"
xlab <- "Soil Water Pressure Head [m]"
ylab <- "Soil Water Content (ranging 0 to 1) "
gswA <- ggplot()+geom_line(aes(x=psi,y=swcA(psi)))+theme_bw()
gswA <- gswA+ggtitle(title)+xlab(xlab)+ylab(ylab)
gswA

## ----eval=TRUE,fig.width=7----------------------------------------------------
swc_rl <- rasterList(soil_parameters,FUN=swc_func)


## ----eval=TRUE,fig.width=7----------------------------------------------------
swc_rlf <- rasterListFun(swc_rl)

## ----eval=TRUE,fig.width=7----------------------------------------------------
psi <- c(0,-0.5,-1,-2)
names(psi) <- sprintf("psi= %1.1f m",psi)
soil_water_content <- stack(swc_rlf(psi))
names(soil_water_content) <- names(psi)
plot(soil_water_content,main=names(psi))

## ----eval=TRUE,fig.width=7,warings=TRUE---------------------------------------
region <- raster(swc_rlf(0))
mask <- !is.na(region)
region[] <- NA
region[points$icell[1]] <- 1
dist <- distance(region)
dist[mask==0] <- NA

mdist <- max(dist[],na.rm=TRUE)

psi_max <- 0 
psi_min <- -2
psi <- psi_max+(psi_min-psi_max)*(1-exp(-dist/mdist))
plot(psi)

## ----eval=TRUE,fig.width=7,waring=FALSE---------------------------------------

color <- colorNumeric("Blues",domain=psi[])

leaf_psi <- leaf1 %>% addRasterImage(psi,color=color,opacity=opacity) %>% 
addLegend(position="bottomright",pal=color,values=psi[],opacity=opacity,title="Psi") %>% addMarkers(lng=points$lon,lat=points$lat,label=points$name)

leaf_psi

## ----eval=TRUE,fig.width=7,warning=FALSE--------------------------------------
theta <- raster(swc_rlf(psi))
plot(theta)

## ----eval=TRUE,fig.width=7,warning=FALSE--------------------------------------

color <- colorNumeric("Blues",domain=theta[])

leaf_psi <- leaf1 %>% addRasterImage(theta,color=color,opacity=opacity) %>% 
addLegend(position="bottomright",pal=color,values=theta[],opacity=opacity,title="Psi") %>% addMarkers(lng=points$lon,lat=points$lat,label=points$name)

leaf_psi

## ----eval=TRUE,echo=TRUE------------------------------------------------------
library(rasterList)
##precf <- system.file("map/Mekrou_precipitation.grd", package="rasterList")
#precf <- system.file("map/cajamarca_monthly_precipitation.grd", package="rasterList")
##
precf0 <- '/home/ecor/local/rpackages/jrc/rasterList_doc/map/cajamarca_monthly_precipitation_vL.grd'
precf <- '/home/ecor/local/rpackages/jrc/rasterList/inst/map/cajamarca_monthly_precipitation_vL2.grd'

prec0 <- stack(precf0)
prec <- aggregate(prec0,fact=2,na.rm=TRUE,fun=mean,filename=precf,overwrite=TRUE)



## ----eval=TRUE,fig.width=7,warning=FALSE,message=FALSE------------------------
  
  ###  independent and identically distributed (i.i.d. 
  library(lubridate)
 
  dates <- as.Date(names(prec),format="X%Y.%m.%d")
  names(dates) <- names(prec)
  
  years <- year(dates)
  months <- month(dates)
  
  im <- which(months<8)
  years[im] <- years[im]-1

## ----eval=TRUE,fig.width=7----------------------------------------------------

lseason <- tapply(X=dates,FUN=length,INDEX=years)
years_c <- names(lseason)[which(lseason==12)]
ic <- which(years %in% years_c)
prec <- prec[[ic]]
years <- years[ic]
months <- months[ic]

## ----eval=TRUE,fig.width=7,warning=FALSE--------------------------------------
  ###
  prec_sum <- stackApply(prec,fun=sum,indices=years-years[1]+1)
  names(prec_sum) <- years[1]+as.numeric(str_replace_all(names(prec_sum),"index_",""))-1
  prec_sum
  

## ----eval=TRUE,fig.width=7,warning=FALSE--------------------------------------


library(leaflet)

ref_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}"
ref_attr <- "Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ, TomTom, Intermap, iPC, USGS, FAO, NPS, NRCAN, GeoBase, Kadaster NL, Ordnance Survey, Esri Japan, METI, Esri China (Hong Kong), and the GIS User Community"
leaf <- leaflet() %>% addTiles(urlTemplate=ref_url,attribution=ref_attr) 


opacity <- 0.6
r <- mean(prec_sum) ###prec_slm[["l_1"]]
#### Domain 
mm <- range(r[])
buffer <- 200
mm <- mm+c(-1,1)*buffer
minzero <- TRUE
if (minzero==TRUE) mm[1] <- 0 ## Precipitation cannot be negative
step <- buffer ###10^(ceiling(log(mm[2])/log(10))-2)
domain=seq(from=mm[1],to=mm[2]+step,by=step)

bins <- (length(domain))/2+1
color <- colorBin("YlGnBu", domain = domain,bin=bins)
values <- domain[domain<=max(r[])]

leaf_l_1 <- leaf %>% addRasterImage(r,opacity=opacity,color=color) %>% addLegend(position="bottomright",pal=color,values=values)
leaf_l_1


## ----eval=TRUE,fig.width=7----------------------------------------------------

pts <- data.frame(lat=c(-7.140278,-5.16,-1.55481,-3.784722),lon=c(-78.488889,-78.288889,-74.609001,-73.308333),name=c("Cajamarca","Imaza","Angoteros","Iquitos"),label=c("CJA","IMZ","ANG","IQT"),stringsAsFactors = FALSE)
pts <- pts[1:2,] ## only Cajamarca and Imaza
pts$icell <- cellFromXY(prec_sum,pts[c("lon","lat")])
head(pts)

leaf_l_1 %>% addMarkers(lng=pts$lon,lat=pts$lat,label=sprintf("%10.2f mm at %s",r[pts$icell], pts$label))

## ----eval=TRUE,fig.width=7----------------------------------------------------
library(trend)

sens.slope_ <- function(x,...) {
  condNA <- all(is.na(x))
  if (condNA) x[] <- -9999
  o <- sens.slope(x,...)
  if (condNA) o[] <- NA
  return(o)
}
prec_sum_sens_slope <- rasterList(prec_sum,FUN=sens.slope_)


## ----eval=TRUE,fig.width=7----------------------------------------------------


lsl <- as.list(prec_sum_sens_slope@list[pts$icell])
names(lsl) <- pts$label
lsl

## ----eval=TRUE,fig.width=7----------------------------------------------------
  
  library(gridExtra)
  library(ggplot2)
  getTrend <- function(x,signif=0.05) {
    
          pval <- x$p.value
          o <- x$estimates
          o[pval>signif] <- 0
          return(o)
  }     

  plotTrend <- function(x=prec_sum[pts$icell][1,],trend=NULL,time=1:length(x),signif=0.05,variable.name="VARNAME",title="Title",...) {
    if (is.null(trend)) trend <- sens.slope_(x,...)
    trend <- getTrend(x=trend,signif=signif)
    print(trend)
    xm <- mean(x)
    ylab <- sprintf("%s an. [mm] from mean",variable.name)
    mean <- sprintf("(mean: %5.2f mm)",xm)
    o <- ggplot()+geom_col(aes(x=time,y=x-xm))+theme_bw() 
    o <- o+xlab("Time [years]")+ylab(ylab)
    o <- o+ggtitle(paste0(title,"  ",mean))
    intercept <- -trend*time[1]
    o <- o+geom_abline(mapping=NULL,intercept=intercept,slope=trend)
    o <- o+theme(title =element_text(size=8))
    return(o)
    
    
  }
  
  time <- as.numeric(str_replace(names(prec_sum),"X",""))
  
  
  prec_sum_sens_slope_trend <- raster(prec_sum_sens_slope,FUN=getTrend)
  
  ptstrend <- list()
  for (i in 1:nrow(pts)){
    ptstrend[[i]] <- plotTrend(prec_sum[pts$icell][i,],variable.name="MAP",title=sprintf("%s (%s)",pts$name[i],pts$label[i]),time=time)
  }
  
  
  do.call(what=grid.arrange,args=ptstrend) 

## ----eval=TRUE,fig.width=7,warning=FALSE--------------------------------------
r <-  prec_sum_sens_slope_trend 
#### Domain 
mm <- range(r[],na.rm=TRUE)
buffer <- 1  #0.01
mm <- mm+c(-1,1)*buffer
minzero <- TRUE
if (minzero==TRUE) mm[1] <- 0 ## Precipitation cannot be negative
step <- buffer ###10^(ceiling(log(mm[2])/log(10))-2)
domain=seq(from=mm[1],to=mm[2]+step,by=step)

bins <- (length(domain))/2+1
color <- colorBin("YlGnBu", domain =domain,bin=bins)
values <- domain[domain<=max(r[])]

leaf_trend <- leaf %>% addRasterImage(r,opacity=opacity,color=color) %>% addLegend(position="bottomright",pal=color,values=values)

leaf_trend %>% addMarkers(lng=pts$lon,lat=pts$lat,label=sprintf("%10.2f mm/year at %s",r[pts$icell], pts$label))

## ----eval=TRUE,fig.width=7----------------------------------------------------

prec_rank <- rasterList(prec,FUN=function(x,years,xnum){
        
  o <- tapply(x,INDEX=years,FUN=function(x,n){x[order(x,decreasing=TRUE)][1:n]},n=xnum,simplify=TRUE)
  return(o)
  
},years=years,xnum=3)

prec_rank_mean <- stack(prec_rank,FUN=function(x,...){
  
  o <- sapply(X=x,FUN=mean,...)
  return(o)
},na.rm=FALSE)

## ----eval=TRUE,fig.width=7,warning=FALSE--------------------------------------
prec_rank_mean_sens_slope <- rasterList(prec_rank_mean,FUN=sens.slope_)
prec_rank_mean_sens_slope_trend <- raster(prec_rank_mean_sens_slope,FUN=getTrend)
ptstrend <- list()

 for (i in 1:nrow(pts)){
    ptstrend[[i]] <- plotTrend(prec_rank_mean[pts$icell][i,],variable.name="M3MP",title=sprintf("%s (%s)",pts$name[i],pts$label[i]),time=time)
    
  }
  do.call(what=grid.arrange,args=ptstrend) 

## ----eval=TRUE,fig.width=7,warning=FALSE,ercho=FALSE,message=FALSE------------
r <-  prec_rank_mean_sens_slope_trend
#### Domain 
mm <- range(r[],na.rm=TRUE)
buffer <- 1  #0.01
mm <- mm+c(-1,1)*buffer
minzero <- TRUE
if (minzero==TRUE) mm[1] <- 0 ## Precipitation cannot be negative
step <- buffer ###10^(ceiling(log(mm[2])/log(10))-2)
domain=seq(from=mm[1],to=mm[2]+step,by=step)

bins <- (length(domain))/2+1
color <- colorBin("YlGnBu", domain =domain,bin=bins)
values <- domain[domain<=max(r[])]

leaf_trend <- leaf %>% addRasterImage(r,opacity=opacity,color=color) %>% addLegend(position="bottomright",pal=color,values=values)

leaf_trend %>% addMarkers(lng=pts$lon,lat=pts$lat,label=sprintf("%10.2f mm/year at %s",r[pts$icell], pts$label))

## ----eval=TRUE,fig.width=7----------------------------------------------------
  
  years <- as.numeric(str_replace(names(prec_sum),"X",""))
  years_max <- years[length(years)]
  prec_sum_res <- prec_sum-stack(lapply(X=(years-years_max),FUN="*",prec_sum_sens_slope_trend))
  prec_rank_mean_res <- prec_rank_mean-stack(lapply(X=(years-years_max),FUN="*",prec_rank_mean_sens_slope_trend))

## ----eval=TRUE,fig.width=7----------------------------------------------------
  library(lmomPi)
  prec_m_ts <- prec_rank_mean_res[pts$icell[1]]
  lmoments <- samlmu(prec_m_ts,ratios=TRUE)
  para <- pel_lmom(lmoments,distrib=c("pe3","gev")) 
  para
  ks <- list()
  for (it in names(para)) {
    ks[[it]] <- ks.test(x=prec_m_ts,y="cdf",para=para[[it]])
  }
  ks

## ----eval=TRUE,fig.width=7,warning=FALSE--------------------------------------
samlmom <- stack(rasterList(prec_rank_mean_res,FUN=samlmu,ratios=TRUE))
## This is a correction
inas <- is.na(samlmom)
samlmom[inas] <- 0
prec_rank_mean_res[inas] <- -9999
distrib <- c("pe3","gev")
fitdist <- list()
kstesting <- list()
for (it in distrib) {
  fitdist[[it]] <- rasterList(samlmom,FUN=pel_lmom,distrib=it)
  kstesting[[it]] <- RasterListApply(x=rasterList(prec_rank_mean_res),y="cdf",para=fitdist[[it]],FUN=ks.test)
}
kstesting[["pe3"]]@list[[pts$icell[1]]]  


## ----eval=TRUE,fig.width=7----------------------------------------------------
  pval_ks <- list()
  pval_ks[["pe3"]] <- raster(kstesting[["pe3"]],FUN=function(x){x$p.value})
  plot(pval_ks[["pe3"]]>0.1)
  pval_ks[["pe3"]] 


## ----eval=TRUE,fig.width=7----------------------------------------------------
 
  pval_ks[["gev"]] <- raster(kstesting[["gev"]],FUN=function(x){x$p.value})
  plot(pval_ks[["gev"]]>0.1)
  pval_ks[["gev"]] 


## ----eval=TRUE,fig.width=7,warning=TRUE---------------------------------------
return_periods <- c(5,10,20,50,100) 
frq= 1/return_periods
names(frq) <- sprintf("T%d",return_periods)
percentiles <-  stack(fitdist[["pe3"]],FUN=function(p,frq) {
   o <- qua(f=frq,para=p)
   names(o) <- names(frq)
   
   return(o)
   },frq=frq)


## ----eval=TRUE,fig.width=7,warning=FALSE,ercho=FALSE,message=FALSE------------
r <-  percentiles[["T20"]]
rp <- r
r[r>1000] <- 1000
#### Domain 
mm <- range(r[],na.rm=TRUE)
buffer <- 20  #0.01
mm <- mm+c(-1,1)*buffer
minzero <- TRUE
if (minzero==TRUE) mm[1] <- 0 ## Precipitation cannot be negative
step <- buffer ###10^(ceiling(log(mm[2])/log(10))-2)
domain=seq(from=mm[1],to=mm[2]+step,by=step)

bins <- (length(domain))/2+1
color <- colorBin("YlGnBu", domain =domain,bin=bins)
values <- domain[domain<=max(r[])]

leaf_trp <- leaf %>% addRasterImage(r,opacity=opacity,color=color) %>% addLegend(position="bottomright",pal=color,values=values)

leaf_trp %>% addMarkers(lng=pts$lon,lat=pts$lat,label=sprintf("%10.2f mm at %s",rp[pts$icell], pts$label))


## ----generateBibliography,echo=FALSE,eval=TRUE,message=FALSE,warning=FALSE,print=FALSE,results="hide"----
require("knitcitations")
cleanbib()
options("citation_format" = "pandoc")
read.bibtex(file = "bibliography.bib")



