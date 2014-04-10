
library(rgdal)
library(raster)
library(sp)

source('rbind.lulc.r')
source('add.col.r')

# Workspace and parameters
drive <- 'z'
workspace <- paste(drive,':/LULC',sep='')
the.radii <- c(12070,24140) # 48280
cell.size <- 250
ag.factors <- c(1,4) # 1 # 4
no.backcast <- 'n'
# single.yr <- 105 #NA
# ver <- 7

# Load and crop CBC pts.
cbc <- readOGR(dsn=workspace,layer='CBC_circles_alb',encoding='ESRI Shapefile')
cat('cbc pts loaded\n')
the.crop <- raster(paste(workspace,'/gp_backcast_1938_1992/gp_lcyear_1992.tif',sep=''))
test <- extract(the.crop,cbc)
cbc <- cbc[is.na(test)==FALSE & test!=0,]
cbc@data$abbrev <- as.character(cbc@data$abbrev)
pt.yr <- read.csv(paste(workspace,'/CirclesByYear.csv',sep=''), stringsAsFactors=FALSE)
cat('all pts in history',dim(pt.yr),'\n')
pt.yr <- pt.yr[pt.yr$abbrev %in% cbc$abbrev,]
cat('all pts in study area',dim(pt.yr),'\n')
pt.yr <- pt.yr[pt.yr$count_yr <= 105 & pt.yr$count_yr >= 66,]
cat('all pts in study years',dim(pt.yr),'\n')

last.minus.first <- function(x) { out <- x[length(x)] - x[1]; return(out) }

for (n in 1) #1:2
{
	for (j in 1) #1:2
	{
	
		the.radius <- the.radii[n]
		ag.factor <- ag.factors[j]

		# Load LULC data
		load(paste(workspace,'/Historical/gp.hist.',cell.size*ag.factor,'m.cbc.r',the.radius,'m.rdata',sep=''))
		
		if (no.backcast=='y')
		{
			all.data <- historical
			all.data <- rbind.lulc(lulc.list=all.data, years=seq(1992,2005,1), pts=cbc)
		}
		else
		{
			load(paste(workspace,'/gp_backcast_1938_1992/gp.backcast.',cell.size*ag.factor,'m.cbc.r',the.radius,'m.rdata',sep=''))
			all.data <- c(gp.backcast,historical)
			all.data <- rbind.lulc(lulc.list=all.data, years=seq(1966,2005,1), pts=cbc)	
		}
		cat('all data',dim(all.data),'\n')
		all.data <- all.data[order(all.data$count_yr),]
	
		target.pts <- unique(pt.yr$abbrev)
		boxplot.data <- data.frame(matrix(rep(NA,17),ncol=17))
		colnames(boxplot.data) <- paste('X',seq(1,17,1),sep='')
		# print(boxplot.data)
		
		for (kk in 1:length(target.pts))
		{
			temp <- all.data[all.data$abbrev==target.pts[kk],]
			# print(temp)
			boxplot.data[kk,] <- apply(temp[,3:19],2,last.minus.first)
			# print(boxplot.data)
			# stop('cbw')
		}
		
		boxplot(boxplot.data)
		stop('cbw')
		
	}
}
