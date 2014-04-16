
source('train.test.data.r')

# Workspace and parameters
drive <- 'z'
workspace <- paste(drive,':/LULC',sep='')
the.radii <- c(12070,24140) # 48280 # 24140
cell.size <- 250
ag.factors <- c(1,4) # 1 # 4
ver <- 8
no.backcast <- 'y'
type <- 'brt'

# Test and training datasets
n <- 6822 # There are 6822 Records
# create.test.data(row.numbers=seq(1,n,1), proportion=0.2, file.name=paste(workspace,'/Models/gp.lulc.test.rows.v1.txt',sep=''))
test.rows <- scan(file=paste(workspace,'/Models/gp.lulc.test.rows.v1.txt',sep=''),what=numeric())
train.rows <- drop.test.rows(row.numbers=seq(1,n,1), test.rows=test.rows)

spp <- read.csv(paste('z:/lulc/gp_focal_spp_list_v',ver,'.csv',sep=''), stringsAsFactors=FALSE, row.names=1)
output <- data.frame(matrix(rep(NA,dim(spp)[1]*4),ncol=4))
colnames(output) <- c('train.0','train.1','test.0','test.1')
rownames(output) <- spp$BBL_ABBREV

prev <- function(x) { return(round(x[2]/sum(x),3)) }

for (n in 1) #1:2
{
	for (j in 1) #1:2
	{
		for (i in 1:21) # 1:11 # 12:21 # length(spp$BBL_ABBREV)) # max is 21
		{
			the.radius <- the.radii[n]
			ag.factor <- ag.factors[j]
			species <- spp$BBL_ABBREV[i]
					
			load(paste(workspace,'/Species/gp.lulc.',ver,'.',species,'.r',the.radius,'m.',ag.factor*cell.size,'m.rdata',sep=''))

			the.data <- pa.bird.data[train.rows,]
			if (no.backcast=='y') { the.data <- the.data[the.data$count_yr >= 92,] }
			train.prev <- table(the.data$detect)
			if(length(train.prev)<2) { train.prev <- c(train.prev,0) }
			
			the.data <- pa.bird.data[test.rows,]
			if (no.backcast=='y') { the.data <- the.data[the.data$count_yr >= 92,] }
			test.prev <- table(the.data$detect)
			if(length(test.prev)<2) { test.prev <- c(test.prev,0) }
			
			output[i,] <- c(train.prev,test.prev)
			# print(output)			
			# stop('cbw')
		}
		output$train.prev <- apply(output[,1:2],1,prev)
		output$test.prev <- apply(output[,3:4],1,prev)
		# stop('cbw')
	}
}
print(output)
write.csv(output,paste('z:/lulc/gp.train_test_v1.eval.1992-2005.csv',sep=''))

