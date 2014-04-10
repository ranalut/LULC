
# Workspace and parameters
drive <- 'z'
workspace <- paste(drive,':/LULC',sep='')
the.radii <- c(12070,24140)
cell.size <- 250
ag.factors <- c(1,4)
no.backcast <- 'n'
type <- 'brt'

comparison <- 'temporal' # 'resolution' # 'radius'
comp.var <- c('2005','1966-2005') # c('12km','24km')
data.1 <- read.csv(paste(drive,':/LULC/Models/GreatPlains/Distribution/EvaluationTables/gp.lulc.brt.eval.7.r12070m.250m.csv',sep=''),header=TRUE,row.names=1)
data.1[,4:7] <- data.1[,4:7]/100
data.2 <- read.csv(paste(drive,':/LULC/Models/GreatPlains/Distribution/EvaluationTables/gp.lulc.brt.eval.6.r12070m.250m.csv',sep=''),header=TRUE,row.names=1)
data.2[,4:7] <- data.2[,4:7]/100

variables <- c('auc','test.om','test.co')
var.names <- c('AUC','OMISSION ERROR','COMMISSION ERROR')

png(paste(workspace,'/Models/GreatPlains/Distribution/EvaluationTables/gp.lulc.fig.',comparison,'.png',sep=''),width=900,height=600,pointsize=14)
	par(mfrow=c(3,1))
	
	for (i in 1:length(variables))
	{
		temp <- data.frame(data.1[,variables[i]],data.2[,variables[i]])
		temp <- t(temp)
		colnames(temp) <- data.1$species
		
		barplot(temp, ylim=c(0,1.15), legend.text=comp.var, args.legend=list(x=dim(data.1)[1]*3,y=1.2,bty='n',horiz=TRUE), beside=TRUE, ylab=variables[i])
		text(x=1,y=1.1,pos=4,labels=var.names[i],cex=1.5)
	}
	
dev.off()	
	
# col.names <- c('auc','cutoff','train.omission','train.commission','test.omission','test.commission')


# temp <- t(output[,2:dim(output)[2]])
# temp[3:6,] <- temp[3:6,]/100
# # output <- data.frame(spp[,c(1:3,6)],output)
# # colnames(output) <- c(colnames(output)[1:4],col.names)
# # write.csv(output, paste('z:/lulc/dev.exp.test.v',ver,'.csv',sep=''))

# colnames(temp) <- output$species
# barplot(temp, ylim=c(0,1), legend.text=col.names, args.legend=list(x=1,y=1,bty='n', horiz=TRUE), beside=TRUE)



