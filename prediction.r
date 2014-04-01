
# Prediction Data


# Evaluate BRT models.
library(dismo)
library(gbm)
library(tcltk2)

source('train.test.data.r')
source('model.eval.fxn.r')
source('deviance.explained.r')

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

# Test and training datasets
n <- 6822 # There are 6822 Records
test.rows <- scan(file=paste(workspace,'/Models/gp.lulc.test.rows.v1.txt',sep=''),what=numeric())
train.rows <- drop.test.rows(row.numbers=seq(1,n,1), test.rows=test.rows)

spp <- read.csv('z:/lulc/gp_focal_spp_list.csv', stringsAsFactors=FALSE, row.names=1)
output <- as.data.frame(matrix(rep(NA,dim(spp)[1]*length(the.radii)*length(ag.factors)),ncol=length(the.radii)*length(ag.factors)))
col.names <- rep(NA,length(the.radii)*length(ag.factors))

for (i in 1:length(spp$BBL_ABBREV)) # max is 22
{
	counter <- 0
	
	for (n in 1:2)
	{
		for (j in 1:2)
		{
			the.radius <- the.radii[n]
			ag.factor <- ag.factors[j]
			species <- spp$BBL_ABBREV[i]
			
			if (file.exists(paste(workspace,'/Models/gp.lulc.brt.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))==FALSE) { next(j) }
			
			load(paste(workspace,'/Species/gp.lulc.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))
			load(paste(workspace,'/Models/gp.lulc.brt.',ver,'.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))

			evaluation <- model.eval(the.model=brt.model, covariates=pa.bird.data[,c(7,11:25)], test.rows=test.rows, obs=pa.bird.data[,9], spp=species)
			
			names(evaluation) <- c('dev.exp.cv','dev.exp.test','cor.cv','cor.test')
			save(evaluation,file=paste(workspace,'/Models/gp.lulc.eval.',ver,'.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))
			
			counter <- counter + 1
			output[i,counter] <- evaluation[['dev.exp.test']]
			col.names[counter] <- paste('r',the.radius,'m.',ag.factor*cell.size,'m',sep='')
			
			# cat('\nend nass',species,'############################\n')
			# stop('cbw')
		}
	}
}

