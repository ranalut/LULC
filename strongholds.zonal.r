library(raster)
create.zones <- 'y'

if (create.zones=='n')
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

# LULC data
lulc.brick <- brick('d:/lulc/historical/conus.lulc.brick.r250m.y2005.tif')

startTime <- Sys.time()
zonal.mean.mat <- zonal(lulc.brick, zones, fun='mean', digits=4, na.rm=TRUE) # This took 4.45 hours.
write.csv(zonal.mean.mat, 'd:/strongholds/lulc.strong.2005.csv')
print(Sys.time() - startTime)

output <- merge(lookup.table, zonal.mean.mat, all.x=TRUE)
output <- output[order(output$strong),]
output$score <- as.vector(as.matrix(strong))
write.csv(output, 'd:/strongholds/lulc.strong.2005.full.csv')



stop('cbw')

