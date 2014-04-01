
library(dismo)
library(gbm)
library(tcltk2)

source('train.test.data.r')

# Build BRT Models

# Workspace and parameters
drive <- 'z'
workspace <- paste(drive,':/LULC',sep='')
the.radii <- c(24140,48280) # 48280 # 24140
cell.size <- 250
ag.factors <- c(1,4) # 1 # 4

# Test and training datasets
n <- 6822 # There are 6822 Records
# create.test.data(row.numbers=seq(1,n,1), proportion=0.2, file.name=paste(workspace,'/Models/gp.lulc.test.rows.v1.txt',sep=''))
test.rows <- scan(file=paste(workspace,'/Models/gp.lulc.test.rows.v1.txt',sep=''),what=numeric())
train.rows <- drop.test.rows(row.numbers=seq(1,n,1), test.rows=test.rows)

spp <- read.csv('z:/lulc/gp_focal_spp_list.csv', stringsAsFactors=FALSE, row.names=1)

for (i in 12:22) # 1:11 # 12:22 # length(spp$BBL_ABBREV)) # max is 22
{
	for (n in 1:2)
	{
		for (j in 1:2)
		{
			the.radius <- the.radii[n]
			ag.factor <- ag.factors[j]
			species <- spp$BBL_ABBREV[i]
			if (spp$Run[i]!='y') 
			{
				if (file.exists(paste(workspace,'/Models/gp.lulc.brt.2.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))==TRUE)
				{
					file.copy(from=paste(workspace,'/Models/gp.lulc.brt.2.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''), to=paste(workspace,'/Models/gp.lulc.brt.3.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))
				}
				cat(species,'no BRT\n')
				next(j)
			}
		
			load(paste(workspace,'/Species/gp.lulc.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))

			the.data <- pa.bird.data[train.rows,]
			# print(colnames(the.data)); print(dim(the.data)); stop('cbw')
			cat('\nnstart BRT,',species)
			cat(' points considered...',dim(the.data)[1],'\n')
			# print(table(the.data$how_many))
			brt.model <- gbm.step(data=the.data, gbm.x=c(7,11:28), gbm.y=9, family="poisson", tree.complexity=5, learning.rate=spp$LR[i], bag.fraction=0.5, verbose=FALSE)
			cat('# trees = ',brt.model$n.trees,'\n')
			save(brt.model,file=paste(workspace,'/Models/gp.lulc.brt.3.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))
			cat('\nend BRT',species,'############################\n')
			# stop('cbw')
		}
	}
}




