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
the.radii <- c(12070,24140) # 12070 # 48280 # 24140
cell.size <- 250
ag.factors <- c(1,4) # 4 # NA
do.hist <-		'n'
do.backcast <-	'y'
r.raster.temp <- 'C:/Users/cwilsey/AppData/Local/Temp/R_raster_cwilsey/'

# Load and crop pts.
cbc <- readOGR(dsn=workspace,layer='CBC_circles_alb',encoding='ESRI Shapefile')
cat('cbc pts loaded\n')
the.crop <- raster(paste(workspace,'/gp_backcast_1938_1992/gp_lcyear_1992.tif',sep=''))
test <- extract(the.crop,cbc)
cbc <- cbc[is.na(test)==FALSE & test!=0,]
# plot(cbc); print(dim(cbc))
# stop('cbw')

# Processes
for (n in 1:2)
{
	for (k in 1:2)
	{
		the.radius <- the.radii[n]
		ag.factor <- ag.factors[k]
		
		# Historical Data
		if (do.hist=='y')
		{
			rasterOptions(tmpdir=r.raster.temp) 
			historical <- list()

			for (i in 1992:2005)
			{
				list.name <- paste('y',i,sep='')
				
				temp <- raster(paste(workspace,'/historical/gp_lcyear_',i,'.tif',sep=''))
				
				lulc.data <- layers.lulc(
					raster.in=temp,
					the.crop=paste(workspace,'/gp_backcast_1938_1992/gp_lcyear_1992.tif',sep=''),
					ag.fact=ag.factor, # NA if no aggregate
					ag.fun=modal # NA if no aggregate
					)
				
				writeRaster(lulc.data,paste(workspace,'/Historical/gp.lulc.brick.r',ag.factor*cell.size,'m.y',i,'.tif',sep=''), overwrite=TRUE)

				historical[[list.name]] <- extract(lulc.data, cbc, buffer=the.radius, fun=mean, na.rm=TRUE) # This automatically deals with NAs, unlike focal.
				
				save(historical,file=paste(workspace,'/Historical/gp.hist.',cell.size*ag.factor,'m.cbc.r',the.radius,'m.rdata',sep=''))
				
				# Remove all the temporary files for that year's calculations.  
				file.remove(dir(r.raster.temp,full.names=TRUE))
				cat('done',i,'\n')
			}
		}

		# Backcast Data
		if (do.backcast=='y')
		{
			rasterOptions(tmpdir=r.raster.temp)
			gp.backcast <- list()

			for (i in 1966:1992) # 1938:1992
			{
				list.name <- paste('y',i,sep='')
				
				temp <- raster(paste(workspace,'/gp_backcast_1938_1992/gp_lcyear_',i,'.tif',sep=''))
				temp[temp==0] <- NA
				plot(temp); stop()
				
				lulc.data <- layers.lulc(
					raster.in=temp,
					the.crop=paste(workspace,'/gp_backcast_1938_1992/gp_lcyear_1992.tif',sep=''),
					ag.fact=4, # NA if no aggregate
					ag.fun=modal # NA if no aggregate
					)
				
				writeRaster(lulc.data,paste(workspace,'/gp_backcast_1938_1992/gp.lulc.brick.r',ag.factor*cell.size,'m.y',i,'.tif',sep=''), overwrite=TRUE)
				
				gp.backcast[[list.name]] <- extract(lulc.data, cbc, buffer=the.radius, fun=mean, na.rm=TRUE)
				
				save(gp.backcast,file=paste(workspace,'/gp_backcast_1938_1992/gp.backcast.',cell.size*ag.factor,'m.cbc.r',the.radius,'m.rdata',sep=''))
				
				# Remove all the temporary files for that year's calculations.  
				file.remove(dir(r.raster.temp,full.names=TRUE))
				cat('done',i,'\n')
			}
		}
	}
}
