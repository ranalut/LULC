
library(dismo)
library(gbm)
library(tcltk2)
library(randomForest)

source('train.test.data.r')

# Build BRT Models

# Workspace and parameters
workspace.d <- 'D:/GreatPlains_LULC_CLIMATE'
workspace.z <- 'Z:/GreatPlains_LULC_CLIMATE'
ver <- 8

# Test and training datasets
n <- 6822 # There are 6822 Records
# create.test.data(row.numbers=seq(1,n,1), proportion=0.2, file.name=paste(workspace,'/conus.lulc.test.rows.v.txt',sep=''))
test.rows <- scan(file=paste(workspace.z,'/conus.lulc.test.rows.v1.txt',sep=''),what=numeric())
train.rows <- drop.test.rows(row.numbers=seq(1,n,1), test.rows=test.rows)

spp <- read.csv(paste('z:/lulc/gp_focal_spp_list_v',ver,'.csv',sep=''), stringsAsFactors=FALSE, row.names=1)

for (i in 1:21) # 1:11 # 12:21 # length(spp$BBL_ABBREV)) # max is 21
{
	species <- spp$BBL_ABBREV[i]
	# if(file.exists(paste(workspace.d,'/Models/gp.bioclim.brt.',ver,'.',species,'.rdata',sep=''))==TRUE) { cat(the.radius,'\n'); next(i) }
	
	# DATA
	load(paste(workspace.d,'/CONUS/Species/conus.lulc.bioclim.',ver,'.',species,'.rdata',sep=''))
	the.data <- pa.bird.data[train.rows,]
	
	# print(colnames(the.data)); print(dim(the.data)); stop('cbw')
	cat('\nnstart model,',species)
	cat(' points considered...',dim(the.data)[1],'\n')
	# print(table(the.data$how_many))
	# stop('cbw')
	
	# Full Model
	brt.model <- gbm.step(data=the.data, gbm.x=c(7,11:46), gbm.y=10, family="bernoulli", tree.complexity=spp$complexity[i], learning.rate=spp$learning.rate[i], bag.fraction=0.5, verbose=FALSE)
	cat('# trees = ',brt.model$n.trees,'\n')
	save(brt.model,file=paste(workspace.d,'/Models/conus.lulc.bioclim.brt.',ver,'.',species,'.rdata',sep=''))

	brt.model <- gbm.step(data=the.data, gbm.x=c(7,11:29), gbm.y=10, family="bernoulli", tree.complexity=spp$complexity[i], learning.rate=spp$learning.rate[i], bag.fraction=0.5, verbose=FALSE)
	cat('# trees = ',brt.model$n.trees,'\n')
	save(brt.model,file=paste(workspace.d,'/Models/conus.bioclim.brt.',ver,'.',species,'.rdata',sep=''))

	brt.model <- gbm.step(data=the.data, gbm.x=c(7,30:46), gbm.y=10, family="bernoulli", tree.complexity=spp$complexity[i], learning.rate=spp$learning.rate[i], bag.fraction=0.5, verbose=FALSE)
	cat('# trees = ',brt.model$n.trees,'\n')
	save(brt.model,file=paste(workspace.d,'/Models/conus.lulc.brt.',ver,'.',species,'.rdata',sep=''))
	
	cat('\nend model',species,'############################\n')
	# stop('cbw')
}
# stop('cbw')
