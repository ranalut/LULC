
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

# spp <- read.csv('z:/lulc/gp_focal_spp_list.csv', stringsAsFactors=FALSE, row.names=1)

spp <- c('HASP','WEME','MCLO','AMKE','RTHA','FEHA','LEOW')
for (n in 1:length(spp))
{
	startTime <- Sys.time()
	species <- spp[n]
	load(paste(workspace,'/Models/gp.lulc.brt.',ver,'.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))
	
	prediction <- predict(pred.data.focal, brt.model, n.trees=brt.model$n.trees, type='response', progress='window', na.rm=TRUE)
	prediction <- round(prediction,3)
	plot(prediction, main=species)
	
	writeRaster(prediction,paste(workspace,'/Predictions/gp.lulc.v',ver,'.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.y',i,'.tif',sep=''), overwrite=TRUE)
	
	print(Sys.time()-startTime)
}



# for (i in 1:length(spp$BBL_ABBREV)) # max is 22
# {
	# counter <- 0
	
	# for (n in 1:2)
	# {
		# for (j in 1:2)
		# {
			# the.radius <- the.radii[n]
			# ag.factor <- ag.factors[j]
			# species <- spp$BBL_ABBREV[i]
			
			# if (file.exists(paste(workspace,'/Models/gp.lulc.brt.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))==FALSE) { next(j) }
			
			# load(paste(workspace,'/Species/gp.lulc.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))
			# load(paste(workspace,'/Models/gp.lulc.brt.',ver,'.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))

			# evaluation <- model.eval(the.model=brt.model, covariates=pa.bird.data[,c(7,11:25)], test.rows=test.rows, obs=pa.bird.data[,9], spp=species)
			
			# names(evaluation) <- c('dev.exp.cv','dev.exp.test','cor.cv','cor.test')
			# save(evaluation,file=paste(workspace,'/Models/gp.lulc.eval.',ver,'.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))
			
			# counter <- counter + 1
			# output[i,counter] <- evaluation[['dev.exp.test']]
			# col.names[counter] <- paste('r',the.radius,'m.',ag.factor*cell.size,'m',sep='')
			
			# # cat('\nend nass',species,'############################\n')
			# # stop('cbw')
		# }
	# }
# }

