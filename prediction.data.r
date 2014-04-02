
# Prediction

library(dismo)
library(gbm)
library(tcltk2)
source('layers.lulc.r')
source('build.similar.raster.r')
startTime <- Sys.time()

# source('settings.r')
# Workspace and parameters
drive <- 'z'
workspace <- paste(drive,':/LULC',sep='')
the.radii <- c(24140,48280) # 48280 # 24140
cell.size <- 250
ag.factors <- c(1,4) # 1 # 4

# Versions
# 1 is default parameters, 2 is increasing learning rate for most, decreasing for some.
# 3 is adjusting complexity to keep learning rates < 0.01. 
ver <- 2
i <- 2005
the.radius <- the.radii[2]
ag.factor <- ag.factors[2]

lulc.data <- layers.lulc(
			file.in=paste(workspace,'/Historical/conus_historical_y',i,'.img',sep=''),
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

the.mask <- raster(paste(workspace,'/gp_backcast_1938_1992/gp_lcyear_1992_',ag.factor*cell.size,'m.tif',sep=''))
# if (ag.factor!=1) { the.mask <- aggregate(the.mask, fact=ag.factor, fun=modal) }
pred.data.focal <- mask(x=pred.data.focal, mask=the.mask, maskvalue=0)
endTime <- Sys.time()
print(endTime-startTime)
# stop('cbw')

names(pred.data.focal) <- c(paste('X',seq(0,16,1),sep=''),'hours')
