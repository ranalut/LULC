
library(raster)
library(rgdal)
library(sp)

cbc.lulc <- function(filename, the.pts, the.radius, the.crop) # Radius in map units of the CRS (m)
{
	# Load LULC data.
	the.data <- raster(filename)
	cat('raster loaded... ')
	
	# Crop data
	r.crop <- raster(the.crop)
	the.data <- crop(the.data, r.crop)
	
	# Create a binary raster brick with layerize.
	the.brick <- layerize(the.data) # This can take a long time.
	cat('layerized... ')
	
	# Sample at CBC points applying an appropriate buffer.
	the.output <- extract(the.brick, the.pts, buffer=the.radius, fun=mean, na.rm=TRUE)
	cat('extracted\n')
	
	return(the.output)
}

drive <- 'z'
workspace <- paste(drive,':/LULC',sep='')
cbc <- readOGR(dsn=workspace,layer='CBC_circles_alb',encoding='ESRI Shapefile')
cat('cbc pts loaded\n')

# Historical goes from 1992 to 2005
historical <- list()

for (i in 1992:2005)
{
	list.name <- paste('y',i,sep='')
	historical[[list.name]] <- cbc.lulc(
		filename=paste(workspace,'/Historical/conus_historical_y',i,'.img',sep=''),
		the.pts=cbc,
		the.radius=24140,
		the.crop=paste(workspace,'/gp_backcast_1938_1992/gp_lcyear_1992.tif',sep='')
		)
	cat('done',i,'\n')
}


# save(historical,file=paste(workspace,'/Historical/gp.hist.250m.cbc.r24140m.rdata',sep=''))

