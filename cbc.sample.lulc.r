# Packages
library(raster)
library(rgdal)
library(sp)

# Functions
source('layers.lulc.r')
source('sample.lulc.brick.r')

# Workspace
drive <- 'z'
workspace <- paste(drive,':/LULC',sep='')

# Process
do.layers <- 'n'
do.sample <- 'y'

if (do.layers=='y')
{
	for (i in 1992:2005)
	{
		layers.lulc(
			file.in=paste(workspace,'/Historical/conus_historical_y',i,'.img',sep=''),
			the.crop=paste(workspace,'/gp_backcast_1938_1992/gp_lcyear_1992.tif',sep=''),
			rdata.out=paste(workspace,'/Historical/gp_hist_y',i,'.rdata',sep='')
			)
		cat('done',i,'\n')
	}
}

if (do.sample=='y')
{
	cbc <- readOGR(dsn=workspace,layer='CBC_circles_alb',encoding='ESRI Shapefile')
	cat('cbc pts loaded\n')
	the.crop <- paste(workspace,'/gp_backcast_1938_1992/gp_lcyear_1992.tif',sep='')
	
	stop('cbw')

	historical <- list()

	for (i in 1992:2005)
	{
		list.name <- paste('y',i,sep='')
		cbc.lulc(
			filename=paste(workspace,'/Historical/conus_historical_y',i,sep=''),
			the.pts=cbc,
			the.radius=24140,
			the.crop=paste(workspace,'/gp_backcast_1938_1992/gp_lcyear_1992.tif',sep='')
			)
		cat('done',i,'\n')
	}



	save(historical,file=paste(workspace,'/Historical/gp.hist.250m.cbc.r24140m.rdata',sep=''))
}
