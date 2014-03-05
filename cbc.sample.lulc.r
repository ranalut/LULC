# Packages
library(raster)
library(rgdal)
library(sp)

# Functions
source('layers.lulc.r')
source('sample.lulc.brick.r')

# Workspace
drive <- 'z' # This ran faster on the server than my computer.
workspace <- paste(drive,':/LULC',sep='')
the.radius <- 48280 # 24140
do.hist <-		'y'
do.backcast <-	'y'

# Load and crop pts.
cbc <- readOGR(dsn=workspace,layer='CBC_circles_alb',encoding='ESRI Shapefile')
cat('cbc pts loaded\n')
the.crop <- raster(paste(workspace,'/gp_backcast_1938_1992/gp_lcyear_1992.tif',sep=''))
test <- extract(the.crop,cbc)
cbc <- cbc[is.na(test)==FALSE & test!=0,]
# plot(cbc); print(dim(cbc))
# stop('cbw')

# Processes
# Historical Data
if (do.hist=='y')
{
	rasterOptions(tmpdir='C:/Users/cwilsey/AppData/Local/Temp/R_raster_cwilsey/') 
	historical <- list()

	for (i in 1992:2005)
	{
		list.name <- paste('y',i,sep='')
		
		lulc.data <- layers.lulc(
			file.in=paste(workspace,'/Historical/conus_historical_y',i,'.img',sep=''),
			the.crop=paste(workspace,'/gp_backcast_1938_1992/gp_lcyear_1992.tif',sep='')
			)
		
		historical[[list.name]] <- extract(lulc.data, cbc, buffer=the.radius, fun=mean, na.rm=TRUE)

		# Remove all the temporary files for that year's calculations.  
		file.remove(dir('c:/users/cwilsey/appdata/local/temp/r_raster_cwilsey',full.names=TRUE))
		cat('done',i,'\n')
		
		save(historical,file=paste(workspace,'/Historical/gp.hist.250m.cbc.r',the.radius,'m.rdata',sep=''))
	}
}

# Backcast Data
if (do.backcast=='y')
{
	rasterOptions(tmpdir='C:/Users/cwilsey/AppData/Local/Temp/R_raster_cwilsey/')
	gp.backcast <- list()

	for (i in 1938:1992)
	{
		list.name <- paste('y',i,sep='')
		
		lulc.data <- layers.lulc(
			file.in=paste(workspace,'/gp_backcast_1938_1992/gp_lcyear_',i,'.tif',sep=''),
			the.crop=NA
			)
		
		gp.backcast[[list.name]] <- extract(lulc.data, cbc, buffer=the.radius, fun=mean, na.rm=TRUE)

		# Remove all the temporary files for that year's calculations.  
		file.remove(dir('c:/users/cwilsey/appdata/local/temp/r_raster_cwilsey',full.names=TRUE))
		cat('done',i,'\n')
		
		save(gp.backcast,file=paste(workspace,'/gp_backcast_1938_1992/gp.backcast.250m.cbc.r',the.radius,'m.rdata',sep=''))
	}
}

