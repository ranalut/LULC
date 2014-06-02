# Packages
library(raster)
library(rgdal)
library(sp)
library(ncdf)
library(climates)

# Functions

# Workspace
drive <- 'd'
workspace <- paste(drive,':/ClimateData',sep='')

r.raster.temp <- 'C:/Users/cwilsey/AppData/Local/Temp/R_raster_cwilsey2/'

# Load pts.
cbc <- readOGR(dsn=workspace,layer='CBC_circles',encoding='ESRI Shapefile')
cat('cbc pts loaded\n')
# plot(cbc); print(dim(cbc))
# stop('cbw')

# Time Series
rasterOptions(tmpdir=r.raster.temp) 
# file.prefix <- c('/Maurer2010Historical/nldas_met_update.obs.monthly.')
# variables <- c('pr','tas','tasmax','tasmin')

hist.bioclim <- list()

load(paste(workspace,'/Maurer2010Historical/maurer.hist.cbc.pr.rdata',sep=''))
pr.data <- hist.clim
load(paste(workspace,'/Maurer2010Historical/maurer.hist.cbc.tas.rdata',sep=''))
tas.data <- hist.clim
load(paste(workspace,'/Maurer2010Historical/maurer.hist.cbc.tasmax.rdata',sep=''))
tasmax.data <- hist.clim
load(paste(workspace,'/Maurer2010Historical/maurer.hist.cbc.tasmin.rdata',sep=''))
tasmin.data <- hist.clim

for (i in 1966:2005)
{
	list.name <- paste('y',i,sep='')
		
	temp <- bioclim(
				tmin=tasmin.data[[list.name]][,-c(1,2)],
				tmax=tasmax.data[[list.name]][,-c(1,2)],
				prec=pr.data[[list.name]][,-c(1,2)],
				tmean=tas.data[[list.name]][,-c(1,2)],
				vois=1:19,
				period='month'
				)
	in.out <- cbc@data$abbrev%in%tasmin.data[[list.name]][,'abbrev']
	hist.bioclim[[list.name]] <- data.frame(abbrev=cbc@data$abbrev[in.out==TRUE],temp)
	
	# stop('cbw')
	save(hist.bioclim, file=paste(workspace,'/Maurer2010Historical/maurer.hist.cbc.bioclim.rdata',sep=''))
	
	# Remove all the temporary files for that year's calculations.  
	file.remove(dir(r.raster.temp,full.names=TRUE))
	# stop('cbw')
	cat('done',i,'\n')
}


