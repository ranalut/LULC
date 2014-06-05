# Packages
library(raster)
library(rgdal)
library(sp)

# Functions
source('layers.lulc.r')
# source('sample.lulc.brick.r')

# Workspace
drive <- 'd'
workspace <- paste(drive,':/LULC',sep='')
the.radii <- 12070 # c(12070,24140) # 12070 # 48280 # 24140
cell.size <- 250
ag.factors <- 1 # c(1,4) # 4 # NA
do.hist <-	'y'
sample.on <- 'n'
r.raster.temp <- 'C:/Users/cwilsey/AppData/Local/Temp/R_raster_cwilsey3/'

# Load and crop pts.
cbc <- readOGR(dsn=workspace,layer='CBC_circles_alb',encoding='ESRI Shapefile')
cat('cbc pts loaded\n')
# plot(cbc); print(dim(cbc))
# stop('cbw')

# Processes

for (k in 1)
{
	ag.factor <- ag.factors[k]
	cat('factor',ag.factor,'\n')
	
	# Historical Data
	if (do.hist=='y')
	{
		rasterOptions(tmpdir=r.raster.temp) 
		list.of.lists <- list(ag1=list(),ag2=list()) # Match to or longer than the.radii length.
		historical <- list()

		for (i in 2003:2005) # 93-97, 98-01, 02-05 #1992:2005
		{
			list.name <- paste('y',i,sep='')
			
			temp <- raster(paste(workspace,'/historical/conus_historical_y',i,'.img',sep=''))
			
			lulc.data <- layers.lulc(
				raster.in=temp,
				the.crop=NA,
				ag.fact=ag.factor,
				ag.fun=modal
				)
			
			writeRaster(lulc.data, paste(workspace, '/Historical/conus.lulc.brick.r', ag.factor*cell.size, 'm.y', i, '.tif', sep=''), overwrite=TRUE)
			
			if (sample.on=='y')
			{
				for (n in 1:length(the.radii))
				{
					the.radius <- the.radii[n]
					list.of.lists[[n]][[list.name]] <- extract(lulc.data, cbc, buffer=the.radius, fun=mean, na.rm=TRUE) # This automatically deals with NAs, unlike focal. # raster loaded... layerized... Error in apply(x, 2, fun2) : dim(X) must have a positive length
					historical <- list.of.lists[[n]]
					save(historical,file=paste(workspace,'/Historical/conus.hist.',cell.size*ag.factor,'m.cbc.r',the.radius,'m.rdata',sep=''))
				}
			}
			# Remove all the temporary files for that year's calculations.  
			file.remove(dir(r.raster.temp,full.names=TRUE))
			cat('done',i,'\n')
		}
	}
}

