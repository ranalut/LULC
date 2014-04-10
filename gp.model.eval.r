
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
drive <- 'z'
workspace <- paste(drive,':/LULC',sep='')
the.radii <- c(12070,24140,48280) # c(12070,24140,48280) # 48280 # 24140
cell.size <- 250
ag.factors <- c(1,4) # 1 # 4

# Versions
# 1 (really none) is default parameters, lr = 0.01, tc = 5
# 2 is increasing learning rate for most (0.03) , decreasing for some (0.005).
# 3 is adjusting complexity to keep learning rates < 0.01. 
ver <- 6
no.backcast <- 'n'
type <- 'brt'

# Test and training datasets
n <- 6822 # There are 6822 Records
test.rows <- scan(file=paste(workspace,'/Models/gp.lulc.test.rows.v1.txt',sep=''),what=numeric())
train.rows <- drop.test.rows(row.numbers=seq(1,n,1), test.rows=test.rows)

spp <- read.csv(paste('z:/lulc/gp_focal_spp_list_v',ver,'.csv',sep=''), stringsAsFactors=FALSE, row.names=1)

for (n in 1) # 1:3
{
	for (j in 2)
	{
		output <- as.data.frame(matrix(rep(NA,dim(spp)[1]*6),ncol=6))
		colnames(output) <- c('auc','cutoff','train.om','train.co','test.om','test.co')

		for (i in 1:length(spp$BBL_ABBREV)) # c(1,2,5:21) # 1:length(spp$BBL_ABBREV) # max is 22
		{
			the.radius <- the.radii[n]
			ag.factor <- ag.factors[j]
			species <- spp$BBL_ABBREV[i]
			
			if (file.exists(paste(workspace,'/Models/GreatPlains/Distribution/gp.lulc.',type,'.',ver,'.',species,'.r',the.radius,'m.', ag.factor*cell.size,'m.rdata',sep=''))==FALSE) { cat('missing... ',species,' ',the.radius,' ',ag.factor*cell.size,'\n'); next(j) }
			
			load(paste(workspace,'/Species/gp.lulc.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))
			load(paste(workspace,'/Models/GreatPlains/Distribution/gp.lulc.',type,'.',ver,'.',species,'.r',the.radius,'m.',ag.factor*cell.size, 'm.rdata',sep=''))
			cat('loaded... ',species,' ',the.radius,' ',ag.factor*cell.size,'\n')
			
			if (no.backcast=='y')
			{
				# evaluation <- model.eval.binary(the.model=rf.model, covariates=pa.bird.data[train.rows,], test.cov=pa.bird.data[test.rows,])
				# covariates <- pa.bird.data[test.rows,]
				# covariates <- covariates[covariates$count_yr >= 92,c(7,11:27)]
				# obs <- pa.bird.data[test.rows,]
				# obs <- obs[obs$count_yr >= 92,9]
				# evaluation <- model.eval.abund(the.model=brt.model, covariates=covariates, obs=obs, spp=species)
			}
			else
			{
				if (type=='rf') { evaluation <- model.eval.binary(the.model=rf.model, covariates=pa.bird.data[train.rows,c(7,10:27)], test.cov=pa.bird.data[test.rows,c(7,10:27)]) }
				if (type=='brt') { evaluation <- model.eval.binary(the.model=brt.model, covariates=pa.bird.data[train.rows,c(7,10:27)], test.cov=pa.bird.data[test.rows,c(7,10:27)], n.trees=brt.model$n.trees) }
			}
			
			save(evaluation,file=paste(workspace,'/Models/GreatPlains/Distribution/gp.lulc.',type,'.eval.',ver,'.',species, '.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))
			
			output[i,] <- as.numeric(c(evaluation[[1]],evaluation[[2]],o.co.error(evaluation[[3]]),o.co.error(evaluation[[4]])))
			
			# cat('\nend nass',species,'############################\n')
			# stop('cbw')
		}
		output <- data.frame(species=spp$BBL_ABBREV,output)
		write.csv(output,paste(workspace,'/Models/GreatPlains/Distribution/EvaluationTables/gp.lulc.',type,'.eval.',ver,'.r', the.radius,'m.',ag.factor*cell.size,'m.csv',sep=''))
		# stop('cbw')
	}
}

