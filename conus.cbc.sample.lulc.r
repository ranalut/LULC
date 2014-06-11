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
data.z <- 'z:/LULC'
the.radii <- 12070 # c(12070,24140) # 12070 # 48280 # 24140
cell.size <- 250
ag.factors <- 1 # c(1,4) # 4 # NA
do.hist <-	'y'
layerize.on <- 'n'
sample.on <- 'y'
r.raster.temp <- 'C:/Users/cwilsey/AppData/Local/Temp/R_raster_cwilsey3/'

# Load and crop pts.
cbc <- readOGR(dsn=data.z,layer='CBC_circles_alb',encoding='ESRI Shapefile')
cat('cbc pts loaded\n')
the.crop <- raster(paste(data.z,'/Historical/conus_historical_y1992.img',sep=''))
test <- extract(the.crop,cbc)
cbc <- cbc[is.na(test)==FALSE & test!=0,] 
# plot(cbc); stop('cbw')
cat('cbc pts loaded\n')

# Processes

for (k in 1)
{
	ag.factor <- ag.factors[k]
	cat('factor',ag.factor,'\n')
	
	# Historical Data
	if (do.hist=='y')
	{
		rasterOptions(tmpdir=r.raster.temp) 
		
		for (i in 2002:2005) # 92-96, 97-01, 02-05 #1992:2005
		{
			list.name <- paste('y',i,sep='')
			
			if (layerize.on=='y')
			{
				temp <- raster(paste(workspace,'/historical/conus_historical_y',i,'.img',sep=''))
				
				lulc.data <- layers.lulc(
					raster.in=temp,
					the.crop=NA,
					ag.fact=ag.factor,
					ag.fun=modal
					)
				
				writeRaster(lulc.data, paste(workspace, '/Historical/conus.lulc.brick.r', ag.factor*cell.size, 'm.y', i, '.tif', sep=''), overwrite=TRUE)
			}
			
			if (sample.on=='y')
			{
				lulc.data <- brick(paste(workspace, '/Historical/conus.lulc.brick.r', ag.factor*cell.size, 'm.y', i, '.tif', sep=''))
				cat('brick loaded...\n')
				
				for (n in 1:length(the.radii))
				{
					the.radius <- the.radii[n]
					historical <- extract(lulc.data, cbc, buffer=the.radius, fun=mean, na.rm=TRUE) # This automatically deals with NAs, unlike focal.
					# historical <- list.of.lists[[n]]
					save(historical,file=paste(workspace,'/Historical/conus.hist.',cell.size*ag.factor,'m.cbc.y',i,'.rdata',sep=''))
				}
			}
			# Remove all the temporary files for that year's calculations.  
			file.remove(dir(r.raster.temp,full.names=TRUE))
			cat('done',i,'\n')
			# stop('cbw')
		}
	}
}

