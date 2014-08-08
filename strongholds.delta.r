library(raster)

fut.scenario <- 'a2'
fut.yr <- 		'2080'
strong <- raster('d:/strongholds/ensemble_te_in_bbs_cbc.img')
legend.conus <- read.csv('d:/lulc/legend_conus.txt', header=FALSE, stringsAsFactors=FALSE)
colnames(legend.conus) <- c('index','lulc')

strong.2005 <- read.csv('d:/strongholds/lulc.strong.2005.full.csv', row.names=1)
strong.2080 <- read.csv(paste('d:/strongholds/lulc.strong.',fut.scenario,'.y',fut.yr,'.full.csv',sep=''), row.names=1)

delta <- strong.2005[,1:2]
delta <- cbind(delta,(strong.2080[,3:19] - strong.2005[,3:19]),strong.2005[,20])

delta.dev <- raster(matrix(as.vector(delta[,4]),ncol=dim(strong)[2]),template=strong)
# plot(delta.dev)
writeRaster(delta.dev,paste('d:/strongholds/lulc.strong.',fut.scenario,'.y',fut.yr,'.delta.dev.tif',sep=''))

delta.ag <- raster(matrix(as.vector(delta[,15]),ncol=dim(strong)[2]),template=strong)
# plot(delta.ag)
writeRaster(delta.ag,paste('d:/strongholds/lulc.strong.',fut.scenario,'.y',fut.yr,'.delta.ag.tif',sep=''))

