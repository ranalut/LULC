
library(dismo)
library(gbm)
library(tcltk2)
library(randomForest)

source('train.test.data.r')

# Build BRT Models

# Workspace and parameters
workspace.d <- 'D:/ClimateData/Maurer2010Historical'
workspace.z <- 'Z:/LULC'
the.radius <- 12070
cell.size <- 250
ag.factor <- 1 # c(1,4) # 1 # 4
ver <- 8
ver.lulc <- 6
include.backcast <- 'n'
type <- 'brt'

# Test and training datasets
n <- 6822 # There are 6822 Records
# create.test.data(row.numbers=seq(1,n,1), proportion=0.2, file.name=paste(workspace,'/Models/gp.lulc.test.rows.v1.txt',sep=''))
test.rows <- scan(file=paste(workspace.z,'/Models/gp.lulc.test.rows.v1.txt',sep=''),what=numeric())
train.rows <- drop.test.rows(row.numbers=seq(1,n,1), test.rows=test.rows)

spp <- read.csv(paste('z:/lulc/gp_focal_spp_list_v',ver,'.csv',sep=''), stringsAsFactors=FALSE, row.names=1)

for (i in 1:21) # 1:11 # 12:21 # length(spp$BBL_ABBREV)) # max is 21
{
	species <- spp$BBL_ABBREV[i]
	# if(file.exists(paste(workspace.d,'/Models/gp.bioclim.brt.',ver,'.',species,'.rdata',sep=''))==TRUE) { cat(the.radius,'\n'); next(i) }
	
	# BIOCLIM
	load(paste(workspace.d,'/Species/gp.bioclim.',ver,'.',species,'.rdata',sep=''))
	the.data <- pa.bird.data[train.rows,]
	
	# LULC
	load(paste(workspace.z,'/Species/gp.lulc.',ver.lulc,'.',species,'.r',the.radius,'m.',cell.size*ag.factor,'m.rdata',sep=''))
	the.data <- data.frame(the.data,pa.bird.data[train.rows,11:27])
	
	if (include.backcast=='n') { the.data <- the.data[the.data$count_yr >= 92,] }
	# print(colnames(the.data)); print(dim(the.data)); stop('cbw')
	cat('\nnstart model,',species)
	cat(' points considered...',dim(the.data)[1],'\n')
	# print(table(the.data$how_many))
	# stop('cbw')
	
	# BRT
	if (type=='brt')
	{
		brt.model <- gbm.step(data=the.data, gbm.x=c(7,11:46), gbm.y=10, family="bernoulli", tree.complexity=spp$complexity[i], learning.rate=spp$learning.rate[i], bag.fraction=0.5, verbose=FALSE)
		cat('# trees = ',brt.model$n.trees,'\n')
		if (include.backcast=='n') { save(brt.model,file=paste(workspace.d,'/Models/gp.lulc.bioclim.brt.',ver,'.92.',species,'.rdata',sep='')) }
		else { save(brt.model,file=paste(workspace.d,'/Models/gp.lulc.bioclim.brt.',ver,'.',species,'.rdata',sep='')) }
	}
	cat('\nend model',species,'############################\n')
	# stop('cbw')
}
# stop('cbw')
