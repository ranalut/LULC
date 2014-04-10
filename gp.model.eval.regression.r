
# Evaluate BRT models.
library(dismo)
library(gbm)
library(tcltk2)

source('train.test.data.r')
source('model.eval.fxn.r')
source('deviance.explained.r')
source('gp.model.eval.r')

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

# Test and training datasets
n <- 6822 # There are 6822 Records
test.rows <- scan(file=paste(workspace,'/Models/gp.lulc.test.rows.v1.txt',sep=''),what=numeric())
train.rows <- drop.test.rows(row.numbers=seq(1,n,1), test.rows=test.rows)

spp <- read.csv(paste('z:/lulc/gp_focal_spp_list_v',ver,'.csv',sep=''), stringsAsFactors=FALSE, row.names=1)
# output <- as.data.frame(matrix(rep(NA,dim(spp)[1]*length(the.radii)*length(ag.factors)),ncol=length(the.radii)*length(ag.factors)))
output <- as.data.frame(matrix(rep(NA,dim(spp)[1]*length(the.radii)*length(ag.factors)),ncol=length(the.radii)*length(ag.factors)))
col.names <- rep(NA,length(the.radii)*length(ag.factors))

for (i in 1:length(spp$BBL_ABBREV)) # c(1,2,5:21) # 1:length(spp$BBL_ABBREV) # max is 22
{
	counter <- 0
	
	for (n in 1:2) # 1:3
	{
		for (j in 1:2)
		{
			the.radius <- the.radii[n]
			ag.factor <- ag.factors[j]
			species <- spp$BBL_ABBREV[i]
			
			if (file.exists(paste(workspace,'/Models/GreatPlains/Distribution/gp.lulc.rf.',ver,'.',species,'.r',the.radius,'m.', ag.factor*cell.size,'m.rdata',sep=''))==FALSE) { cat('missing... ',species,' ',the.radius,' ',ag.factor*cell.size,'\n'); next(j) }
			# if (file.exists(paste(workspace,'/Models/gp.lulc.brt.',ver,'.',species,'.r',the.radius,'m.', ag.factor*cell.size,'m.rdata',sep=''))==FALSE) { cat('missing... ',species,' ',the.radius,' ',ag.factor*cell.size,'\n'); next(j) }
			
			load(paste(workspace,'/Species/gp.lulc.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))
			load(paste(workspace,'/Models/GreatPlains/Distribution/gp.lulc.rf.',ver,'.',species,'.r',the.radius,'m.',ag.factor*cell.size, 'm.rdata',sep=''))
			# load(paste(workspace,'/Models/gp.lulc.brt.',ver,'.',species,'.r',the.radius,'m.',ag.factor*cell.size, 'm.rdata',sep=''))
			cat('loaded... ',species,' ',the.radius,' ',ag.factor*cell.size,'\n')
			
			if (no.backcast=='y')
			{
				evaluation <- model.eval.binary(the.model=rf.model, covariates=pa.bird.data[train.rows,], test.cov=pa.bird.data[test.rows,])
				# covariates <- pa.bird.data[test.rows,]
				# covariates <- covariates[covariates$count_yr >= 92,c(7,11:27)]
				# obs <- pa.bird.data[test.rows,]
				# obs <- obs[obs$count_yr >= 92,9]
				# evaluation <- model.eval.abund(the.model=brt.model, covariates=covariates, obs=obs, spp=species)
			}
			else
			{
				evaluation <- model.eval.binary(the.model=rf.model, covariates=pa.bird.data[train.rows,c(7,10:27)], test.cov=pa.bird.data[test.rows,c(7,10:27)])
				# evaluation <- model.eval.abund(the.model=brt.model, covariates=pa.bird.data[test.rows,c(7,11:27)], obs=pa.bird.data[test.rows,9], spp=species)
			}
			# names(evaluation) <- c('dev.exp.cv','dev.exp.test','cor.cv','cor.test')
			save(evaluation,file=paste(workspace,'/Models/GreatPlains/Distribution/gp.lulc.rf.eval.',ver,'.',species, '.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))
			# save(evaluation,file=paste(workspace,'/Models/GreatPlains/Abundance/gp.lulc.eval.',ver,'.',species, '.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))
			
			# counter <- counter + 1
			# output[i,counter] <- evaluation[['dev.exp.test']]
			# col.names[counter] <- paste('r',the.radius,'m.',ag.factor*cell.size,'m',sep='')
			
			# cat('\nend nass',species,'############################\n')
			stop('cbw')
		}
	}
}
stop('cbw')
print(output)
temp <- t(output)
output <- data.frame(spp[,c(1:3,6)],output)
colnames(output) <- c(colnames(output)[1:4],col.names)
write.csv(output, paste('z:/lulc/dev.exp.test.v',ver,'.csv',sep=''))

colnames(temp) <- output$BBL_ABBREV
barplot(temp, ylim=c(0,1), legend.text=col.names, args.legend=list(x=1,y=1,bty='n', horiz=TRUE), beside=TRUE)

png(paste(workspace,'/Models/gp.lulc.eval.',ver,'.png',sep=''),width=1200)
	barplot(temp, ylim=c(0,1), legend.text=col.names, args.legend=list(x=100,y=1,bty='n', horiz=TRUE), beside=TRUE, ylab='proportion of deviance explained')
dev.off()
