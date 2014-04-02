
library(dismo)
library(gbm)
library(tcltk2)

# Workspace and parameters
drive <- 'z'
workspace <- paste(drive,':/LULC',sep='')
the.radii <- c(24140,48280) # 48280 # 24140
cell.size <- 250
ag.factors <- c(1,4) # 1 # 4

spp <- read.csv('z:/lulc/gp_focal_spp_list.csv', stringsAsFactors=FALSE, row.names=1)
output <- as.data.frame(matrix(rep(NA,dim(spp)[2]*length(the.radii)*length(ag.factors)),ncol=length(the.radii)*length(ag.factors)))
col.names <- rep(NA,length(the.radii)*length(ag.factors))
learning.rate <- NA
complexity <- NA

for (i in 1:length(spp$BBL_ABBREV))
{
	species <- spp$BBL_ABBREV[i]
	cat('start BRT,',species,'\n')
	counter <- 0
	
	for (n in 1:2)
	{
		for (j in 1:2)
		{
			the.radius <- the.radii[n]
			ag.factor <- ag.factors[j]
			if (species%in%'INBU') { next(j); cat(species,'no BRT\n') }
		
			load(paste(workspace,'/Models/gp.lulc.brt.2.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))
			counter <- counter + 1
			output[i,counter] <- brt.model$n.trees
			col.names[counter] <- paste('r',the.radius,'m.',ag.factor*cell.size,'m',sep='')
			
			learning.rate[i] <- brt.model$shrinkage
			complexity[i] <- brt.model$interaction.depth
		}
	}
}

colnames(output) <- col.names

spp <- cbind(spp,learning.rate, complexity, output)
write.csv(spp, 'z:/lulc/gp_focal_spp_list_v2.csv')


