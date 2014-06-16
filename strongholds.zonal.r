library(raster)

# load lulc data 
lulc.brick <- raster('d:/lulc/historical/conus.lulc.brick.r250m.y2005.tif')

# load strongholds grid
strong <- raster('d:/strongholds/ensemble_te_in_bbs_cbc.img')
plot(strong); cat('strongholds loaded...\n')

# Crop and reproject Strongholds
proj.lulc.extent <- projectExtent(lulc.brick,crs=crs(strong))
# s.grid <- crop(strong, extent(-2357953,2282797,-1785520,1500000), snap='out')
crop.strong <- crop(strong, proj.lulc.extent, snap='out')
plot(crop.strong); cat('strongholds cropped to LULC\n') # ; stop('cbw')

strong.250 <- disaggregate(crop.strong, fact=40)
cat('disaggregated to 250m\n')
lulc.brick.resample <- resample(lulc.brick, strong.250, method='ngb')
cat('resampled LULC\n')

na.grid <- as.matrix(crop.strong)
zone.grid <- matrix(seq(1,prod(dim(crop.strong)[1:2]),1),nrow=dim(crop.strong)[1],ncol=dim(crop.strong)[2])
zone.grid[is.na(na.grid)==TRUE] <- NA
s.grid <- raster(zone.grid,template=crop.strong)
plot(s.grid); cat('zonal grid\n')

zonal.mean.mat <- zonal(lulc.brick.resample, s.grid, fun='mean', digits=4, na.rm=TRUE)
write.csv(zonal.mean.mat, 'd:/strongholds/lulc.ensemble.te.bbs.cbc.csv')

crop.strong.mat <- as.matrix(crop.strong)
output.table <- data.frame(zonal.mean.mat,score=as.vector(crop.strong.mat))
write.csv(output.table, 'd:/strongholds/lulc.zscore.ensemble.te.bbs.cbc.csv')

