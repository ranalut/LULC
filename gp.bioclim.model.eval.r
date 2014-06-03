
# Evaluate BRT models.
library(dismo)
library(gbm)
library(tcltk2)
library(randomForest)

source('train.test.data.r')
source('model.eval.fxn.r')
source('deviance.explained.r')
source('o.co.error.r')

# source('settings.r')
# Workspace and parameters
workspace.d <- 'D:/ClimateData/Maurer2010Historical'
workspace.z <- 'Z:/LULC'
the.radius <- 12070
ver <- 8
include.backcast <- 'n'
type <- 'brt'

# Test and training datasets
n <- 6822 # There are 6822 Records
test.rows <- scan(file=paste(workspace.z,'/Models/gp.lulc.test.rows.v1.txt',sep=''),what=numeric())
train.rows <- drop.test.rows(row.numbers=seq(1,n,1), test.rows=test.rows)

spp <- read.csv(paste('z:/lulc/gp_focal_spp_list_v',ver,'.csv',sep=''), stringsAsFactors=FALSE, row.names=1)

output <- as.data.frame(matrix(rep(NA,dim(spp)[1]*8),ncol=8))
colnames(output) <- c('auc','kappa','cutoff','train.om','train.co','test.om','test.co','dev.exp.test')

for (i in 1:21) # 1:length(spp$BBL_ABBREV) # c(1,2,5:21)
{
	species <- spp$BBL_ABBREV[i]
	
	# if(file.exists(paste(workspace.d,'/Models/gp.bioclim.brt.',ver,'.',species,'.rdata',sep=''))==FALSE) { cat('missing... ',species,' ',the.radius,'\n'); next(i) }
	
	load(paste(workspace.d,'/Species/gp.bioclim.',ver,'.',species,'.rdata',sep=''))
	
	if (include.backcast=='n')
	{
		load(paste(workspace.d,'/Models/gp.bioclim.',type,'.',ver,'.92.',species,'.rdata',sep=''))
		cat('\nloaded... ',species,' ',the.radius,'\n')
	
		temp <- as.integer(rownames(pa.bird.data))
		if (type=='brt') { evaluation <- model.eval.binary(the.model=brt.model, covariates=pa.bird.data[temp%in%train.rows & pa.bird.data$count_yr >= 92,c(7,10:29)], test.cov=pa.bird.data[temp%in%test.rows & pa.bird.data$count_yr >= 92,c(7,10:29)], n.trees=brt.model$n.trees) }
		save(evaluation,file=paste(workspace.d,'/Models/gp.bioclim.',type,'.eval.',ver,'.92.',species,'.rdata',sep=''))
	}
	else
	{
		load(paste(workspace.d,'/Models/gp.bioclim.',type,'.',ver,'.',species,'.rdata',sep=''))
		cat('\nloaded... ',species,' ',the.radius,'\n')
		
		# if (type=='rf') { evaluation <- model.eval.binary(the.model=rf.model, covariates=pa.bird.data[train.rows,c(7,10:29)], test.cov=pa.bird.data[test.rows,c(7,10:29)]) }
		if (type=='brt') { evaluation <- model.eval.binary(the.model=brt.model, covariates=pa.bird.data[train.rows,c(7,10:29)], test.cov=pa.bird.data[test.rows,c(7,10:29)], n.trees=brt.model$n.trees) }
		save(evaluation,file=paste(workspace.d,'/Models/gp.bioclim.',type,'.eval.',ver,'.',species,'.rdata',sep=''))
	}
		
	output[i,] <- as.numeric(c(evaluation[[1]],evaluation[[2]],evaluation[[3]],o.co.error(evaluation[[4]]),o.co.error(evaluation[[5]]),evaluation[[6]]))
	# stop('cbw')
}
output <- data.frame(species=spp$BBL_ABBREV,output)

if (include.backcast=='n') 
{ 
	write.csv(output,paste(workspace.d,'/Models/EvaluationTables/gp.bioclim.',type,'.eval.',ver,'.92.csv',sep=''))
}
if (include.backcast=='y')
{
	write.csv(output,paste(workspace.d,'/Models/EvaluationTables/gp.bioclim.',type,'.eval.',ver,'.csv',sep=''))
}
# stop('cbw')

