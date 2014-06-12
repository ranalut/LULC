library(raster)

# load lulc data 
lulc.brick <- raster('d:/lulc/historical/conus.lulc.brick.r250m.y2005.tif')

# load strongholds grid
strongholds <- raster('d:/strongholds/ensemble_all_te.img')

# Crop and reproject Strongholds
proj.lulc.extent <- projectExtent(lulc.brick,crs=crs(strongholds))
# s.grid <- crop(strongholds, extent(-2357953,2282797,-1785520,1500000), snap='out')
s.grid <- crop(strongholds, proj.lulc.extent, snap='out')
print(dim(s.grid))
# s.grid <- projectRaster(s.grid, crs=crs(lulc.brick)) # , alignOnly=TRUE)
# print(dim(s.grid))
# stop('cbw')

na.grid <- as.matrix(s.grid)
zone.grid <- matrix(seq(1,prod(dim(s.grid)[1:2]),1),nrow=dim(s.grid)[1],ncol=dim(s.grid)[2])
zone.grid[is.na(na.grid)==TRUE] <- NA
s.grid <- raster(zone.grid,template=s.grid)
# s.grid <- mask(s.grid,mask=strongholds)
# plot(s.grid); stop('cbw')

zonal.mean.mat <- zonal(lulc.brick, s.grid, fun='mean', digits=4, na.rm=TRUE)



