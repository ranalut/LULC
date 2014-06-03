
# Workspace and parameters
drive <- 'z'
workspace <- paste(drive,':/LULC',sep='')
# the.radii <- c(12070,24140)
# cell.size <- 250
# ag.factors <- c(1,4)
# no.backcast <- 'n'
# type <- 'brt'

comparison <- 'clim.lulc.92' # 'temporal.3' #'temporal' # 'temporal.2' # 'temporal' # 'resolution' # 'radius'
# comp.var <- c('12070m','24140m') # c('12km','24km')
# comp.var <- c('1966-2005, annual matching LULC','1966-2005, only 2005 LULC')
# comp.var <- c('1966-2005, annual matching LULC','1992-2005, annual matching LULC')
# comp.var <- c('1966-2005, only 2005 LULC','1992-2005, annual matching LULC')
# comp.var <- c('1966-2005, annual matching LULC','1966-2005, annual matching BIOCLIM')
comp.var <- c('1992-2005, annual matching LULC','1992-2005, annual matching BIOCLIM')
data.1 <- read.csv(paste(drive,':/LULC/Models/GreatPlains/Distribution/EvaluationTables/gp.lulc.brt.eval.8.r12070m.250m.csv',sep=''),header=TRUE,row.names=1)
data.1[,5:8] <- data.1[,5:8]/100
data.2 <- read.csv(paste(drive,':/LULC/Models/GreatPlains/Distribution/EvaluationTables/gp.bioclim.brt.eval.8.92.csv',sep=''),header=TRUE,row.names=1)
data.2[,5:8] <- data.2[,5:8]/100

variables <- c('kappa','dev.exp.test','test.om','test.co')
var.names <- c('KAPPA','DEVIANCE EXPLAINED','OMISSION ERROR','COMMISSION ERROR')

png(paste(workspace,'/Models/GreatPlains/Distribution/EvaluationTables/gp.lulc.fig.',comparison,'.png',sep=''),width=900,height=900,pointsize=14)
	par(mfrow=c(4,1))
	
	for (i in 1:length(variables))
	{
		temp <- data.frame(data.1[,variables[i]],data.2[,variables[i]])
		temp <- t(temp)
		colnames(temp) <- data.1$species
		
		barplot(temp, ylim=c(0,1.15), legend.text=comp.var, args.legend=list(x=dim(data.1)[1]*3,y=1.2,bty='n',horiz=TRUE), beside=TRUE, ylab=variables[i])
		text(x=1,y=1.1,pos=4,labels=var.names[i],cex=1.5)
	}
dev.off()	

png(paste(workspace,'/Models/GreatPlains/Distribution/EvaluationTables/gp.lulc.xyplot.',comparison,'.png',sep=''),width=480,height=480,pointsize=14)
	par(mfrow=c(2,2))
	
	for (i in 1:length(variables))
	{
		temp <- data.frame(data.1[,variables[i]],data.2[,variables[i]])
		rownames(temp) <- data.1$species
		
		plot(temp[,1:2],xlab=comp.var[1],ylab=comp.var[2],main=var.names[i],xlim=c(0,1),ylim=c(0,1)) #, ylim=c(0,1.15), legend.text=comp.var, args.legend=list(x=dim(data.1)[1]*3,y=1.2,bty='n',horiz=TRUE), beside=TRUE, ylab=variables[i])
		abline(a=0,b=1)
		# text(x=1,y=1.1,pos=4,labels=var.names[i],cex=1.5)
	}
dev.off()	

png(paste(workspace,'/Models/GreatPlains/Distribution/EvaluationTables/gp.lulc.xyplot.raptors.',comparison,'.png',sep=''),width=480,height=480,pointsize=14)
	par(mfrow=c(2,2))
	
	for (i in 1:length(variables))
	{
		temp <- data.frame(data.1[,variables[i]],data.2[,variables[i]])
		rownames(temp) <- data.1$species
		
		plot(temp[c('FEHA','LEOW','NOHA','PRFA','AMKE','RTHA','GHOW','RLHA'),1:2],xlab=comp.var[1],ylab=comp.var[2],main=var.names[i],xlim=c(0,1),ylim=c(0,1)) #, ylim=c(0,1.15), legend.text=comp.var, args.legend=list(x=dim(data.1)[1]*3,y=1.2,bty='n',horiz=TRUE), beside=TRUE, ylab=variables[i])
		abline(a=0,b=1)
		# text(x=1,y=1.1,pos=4,labels=var.names[i],cex=1.5)
	}
dev.off()	

png(paste(workspace,'/Models/GreatPlains/Distribution/EvaluationTables/gp.lulc.xyplot.longspurs.',comparison,'.png',sep=''),width=480,height=480,pointsize=14)
	par(mfrow=c(2,2))
	
	for (i in 1:length(variables))
	{
		temp <- data.frame(data.1[,variables[i]],data.2[,variables[i]])
		rownames(temp) <- data.1$species
		
		plot(temp[c('SMLO','MCLO','LARB','CCLO','LALO','SNBU'),1:2],xlab=comp.var[1],ylab=comp.var[2],main=var.names[i],xlim=c(0,1),ylim=c(0,1)) #, ylim=c(0,1.15), legend.text=comp.var, args.legend=list(x=dim(data.1)[1]*3,y=1.2,bty='n',horiz=TRUE), beside=TRUE, ylab=variables[i])
		abline(a=0,b=1)
		# text(x=1,y=1.1,pos=4,labels=var.names[i],cex=1.5)
	}
dev.off()	
	
