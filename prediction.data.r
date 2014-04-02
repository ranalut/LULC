
# Prediction
library(dismo)
library(gbm)
# library(tcltk2)
source('layers.lulc.r')
source('build.similar.raster.r')
startTime <- Sys.time()

# Workspace and parameters
drive <- 'd'
workspace <- paste(drive,':/LULC',sep='')
cell.size <- 250
year <- 2005
the.radius <- 12070 # 24140 # 48280
ag.factor <- 4 # 1 # 4

lulc.data <- layers.lulc(
			file.in=paste(workspace,'/Historical/conus_historical_y',year,'.img',sep=''),
			the.crop=paste(workspace,'/gp_backcast_1938_1992/gp_lcyear_1992.tif',sep=''),
			ag.fact=ag.factor, # NA if no aggregate
			ag.fun=modal # NA if no aggregate
			)
temp <- unstack(lulc.data)
temp <- build.similar(file.name=temp[[1]],value=36,var.name='hours')
pred.data <- addLayer(lulc.data, temp)
print(Sys.time()-startTime)

the.weights <- focalWeight(pred.data, d=the.radius, type='circle')
pred.data <- unstack(pred.data)
pred.data.focal <- list()

for (n in 1:length(pred.data))
{
	pred.data.focal[[n]] <- focal(pred.data[[n]], w=the.weights)
	cat('done ',n,' ',Sys.time()-startTime,'\n')
}

pred.data.focal <- brick(pred.data.focal)
print(pred.data.focal)
print(names(print.data.focal))
endTime <- Sys.time()
print(endTime-startTime)
# stop('cbw')

# names(pred.data.focal) <- c(paste('X',seq(1,17,1),sep=''),'hours')

writeRaster(pred.data.focal,paste(workspace,'/Predictions/gp.lulc.pred.data.r',the.radius,'m.',ag.factor*cell.size,'m.y',i,'.tif',sep=''), overwrite=TRUE)
