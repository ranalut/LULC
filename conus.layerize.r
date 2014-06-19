library(raster)

drive <- 'd'
scenarios <- c('a1b','a2','b2')
years <- c(2020,2060,2080)
paths1 <- paste(drive,':/LULC/conus_',scenarios,'/conus_',scenarios,sep='')
paths2 <- paste(rep(paths1,each=3),'_y',years,sep='')
paths3 <- paste(paths2,'.img',sep='')
# stop('cbw')

# for (i in 1:length(paths3))
for (i in 6:8)
{
	print(paths3[i])
	temp <- raster(paths3[i])
	
	lulc.data <- layerize(temp)
	
	writeRaster(lulc.data, paste(paths2[i],'.tif', sep=''), overwrite=TRUE)
	file.remove(dir('C:/Users/cwilsey/AppData/Local/Temp/R_raster_cwilsey/',full.names=TRUE))
}

