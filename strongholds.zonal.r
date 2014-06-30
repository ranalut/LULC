library(raster)
create.zones <-	'n'
do.present <- 	'n'
do.future <- 	'y'

if (create.zones=='y')
{
	# Preparation.  Create a strongholds cell index grid to define zones.
	# load strongholds grid
	strong <- raster('d:/strongholds/ensemble_te_in_bbs_cbc.img')
	plot(strong); cat('strongholds loaded...\n')

	# Crop and reproject Strongholds
	proj.lulc.extent <- projectExtent(lulc.brick,crs=crs(strong))
	crop.strong <- crop(strong, proj.lulc.extent, snap='out') # s.grid <- crop(strong, extent(-2357953,2282797,-1785520,1500000), snap='out')
	plot(crop.strong); cat('strongholds cropped to LULC\n') # ; stop('cbw')
	cat('crop.strong',dim(crop.strong),'\n', sep=' ')

	# Assign Zone #s
	na.grid <- as.matrix(crop.strong)
	zone.grid <- matrix(as.integer(seq(1,prod(dim(crop.strong)[1:2]),1)),nrow=dim(crop.strong)[1],ncol=dim(crop.strong)[2])
	zone.grid[is.na(na.grid)==TRUE] <- NA
	s.grid <- raster(zone.grid,template=crop.strong)
	plot(s.grid); cat('zonal grid\n')
	cat('s.grid',dim(s.grid),'\n', sep=' ')

	writeRaster(s.grid,'d:/strongholds/lulc.crop.strong.grid.tif',overwrite=TRUE)
	
	strong.mat <- matrix(seq(1,prod(dim(strong)[1:2]),1),nrow=dim(strong)[1],ncol=dim(strong)[2])
	lookup.table <- data.frame(strong=as.vector(strong.mat))
	
	# Rebuild to match strongholds grid
	zg2 <- cbind(matrix(NA,ncol=421,nrow=dim(zone.grid)[1]),zone.grid,matrix(NA,ncol=202,nrow=dim(zone.grid)[1]))
	zg2 <- rbind(matrix(NA,ncol=dim(zg2)[2],nrow=396),zg2,matrix(NA,ncol=dim(zg2)[2],nrow=4))
	
	lookup.table <- data.frame(lookup.table,zone=as.vector(zg2))
	write.csv(lookup.table, 'd:/strongholds/lookup.table.csv')
	
	stop('cbw')
}

# Reprojected strongholds grid
zones <- raster('d:/strongholds/zones_raster2.tif')

# Present
if(do.present=='y')
{
	# LULC data
	lulc.brick <- brick('d:/lulc/historical/conus.lulc.brick.r250m.y2005.tif')
	lookup.table <- read.csv('d:/strongholds/lookup.table.csv', header=TRUE, row.names=1)
	
	startTime <- Sys.time()
	zonal.mean.mat <- zonal(lulc.brick, zones, fun='mean', digits=4, na.rm=TRUE) # This took 4.45 hours.
	write.csv(zonal.mean.mat, 'd:/strongholds/lulc.strong.2005.csv')
	print(Sys.time() - startTime)

	output <- merge(lookup.table, zonal.mean.mat, all.x=TRUE)
	output <- output[order(output$strong),]
	output$score <- as.vector(as.matrix(strong))
	write.csv(output, 'd:/strongholds/lulc.strong.2005.full.csv')
}

# Future
if (do.future=='y')
{
	drive <- 'd'
	scenarios <- c('a1b','a2','b2')
	years <- c(2020,2050,2080)
	paths1 <- paste(drive,':/LULC/conus_',scenarios,'/conus_',scenarios,sep='')
	paths2 <- paste(rep(paths1,each=3),'_y',years,sep='')
	paths3 <- paste(paths2,'.tif',sep='')

	# for (i in 1:length(paths3))
	for (i in c(6,9,2,5,8,1,4,7)) # 3,
	{
		lulc.brick <- brick(paths3[i])
		lookup.table <- read.csv('d:/strongholds/lookup.table.csv', header=TRUE, row.names=1)
	
		startTime <- Sys.time()
		zonal.mean.mat <- zonal(lulc.brick, zones, fun='mean', digits=4, na.rm=TRUE) # This took 4.45 hours.
		write.csv(zonal.mean.mat, paste('d:/strongholds/lulc.strong.',rep(scenarios,each=3)[i],'.y',rep(years,3)[i],'.csv',sep=''))
		print(Sys.time() - startTime)

		output <- merge(lookup.table, zonal.mean.mat, all.x=TRUE)
		output <- output[order(output$strong),]
		output$score <- as.vector(as.matrix(strong))
		write.csv(output, paste('d:/strongholds/lulc.strong.',rep(scenarios,each=3)[i],'.y',rep(years,3)[i],'.full.csv',sep=''))
	
		file.remove(dir('C:/Users/cwilsey/AppData/Local/Temp/R_raster_cwilsey/',full.names=TRUE))
	}
}
stop('cbw')

