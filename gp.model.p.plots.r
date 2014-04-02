
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
the.radii <- c(12070,24140,48280) # 48280 # 24140
cell.size <- 250
ag.factors <- c(1,4) # 1 # 4

# Versions
# 1 (really none) is default parameters, lr = 0.01, tc = 5
# 2 is increasing learning rate for most (0.03) , decreasing for some (0.005).
# 3 is adjusting complexity to keep learning rates < 0.01. 
ver <- 5

spp <- read.csv(paste('z:/lulc/gp_focal_spp_list_v',ver,'.csv',sep=''), stringsAsFactors=FALSE, row.names=1)
conus.names <- scan('z:/lulc/legend_conus.txt',what=character(), sep=',')
conus.names <- c('NA',conus.names[seq(2,34,2)])

var.names <- c(paste('X',seq(0,17,1),sep=''),'hours')
boxplot.table <- data.frame(conus=c(conus.names,'hours'),var=var.names)

# for (i in 1:length(spp$BBL_ABBREV)) # max is 22
modifier <- 'raptors'; for (i in c(8,9,13,14,16,17,18,21))
# modifier <- 'long.bunt'; for (i in c(2,4,5,10,12,20))
# modifier <- 'game'; for (i in c(1,3,15))
# modifier <- 'other'; for (i in c(6,7,21,11))
{
	for (n in 1) # 1:2)
	{
		for (j in 1) # 1:2)
		{
			the.radius <- the.radii[n]
			ag.factor <- ag.factors[j]
			species <- spp$BBL_ABBREV[i]
			# if (species=='INBU') { next(j) }
			
			load(paste(workspace,'/Models/gp.lulc.brt.',ver,'.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))
			gbm.plot(brt.model, plot.layout=c(3,3), n.plots=9)
			boxplot.table <- merge(boxplot.table,brt.model$contributions,by='var')
			# stop('cbw')
		}
	}
}

# print(boxplot.table)
plot.table <- t(boxplot.table[,-c(1:2)])
colnames(plot.table) <- boxplot.table$conus
print(plot.table)
# boxplot(plot.table)

png(paste(workspace,'/Models/gp.lulc.var.imp.',modifier,'.',ver,'.png',sep=''),height=600)
	par(mar=c(4,2,4,2))
	boxplot(plot.table, xlab='relative contribution (%)',horizontal=TRUE,yaxt='n',ylim=c(-25,40), main=modifier)
	text(labels=boxplot.table$conus, x=rep(0,length(boxplot.table$conus)),y=seq(1,length(boxplot.table$conus)),pos=2)
dev.off()

