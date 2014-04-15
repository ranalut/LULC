library(rgdal)
library(raster)
library(sp)

source('rbind.lulc.r')
source('add.col.r')

# Workspace and parameters
drive <- 'z'
workspace <- paste(drive,':/LULC',sep='')
the.radii <- c(12070,24140,48280) # 48280 # 24140
cell.size <- 250
ag.factors <- c(1,4) # 1 # 4
no.backcast <- 'y'
single.yr <- NA # 105 #NA
ver <- 8

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
# test <- SpatialPoints(pt.yr[,c('latitude','longitude')])
# plot(test)

for (n in 1) #1:2
{
	for (j in 1:2) #1:2
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

		# Loop through species...
		# spp <- read.csv('z:/lulc/gp_focal_spp_list.csv')
		spp <- read.csv(paste('z:/lulc/gp_focal_spp_list_v',ver,'.csv',sep=''), stringsAsFactors=FALSE, row.names=1)

		for (i in 1:dim(spp)[1])
		{
			species <- spp$BBL_ABBREV[i]
			bird.data <- read.csv(paste(drive,':/CBC-Data/',species,'.csv',sep=''), stringsAsFactors=FALSE)
			bird.data <- bird.data[,c('abbrev','count_yr','how_many','detect')]

			# First add the species count data
			pa.bird.data <- merge(pt.yr,bird.data,by=c('abbrev','count_yr'),all.x=TRUE)
			# print(dim(pa.bird.data))
			pa.bird.data$how_many[is.na(pa.bird.data$how_many)==TRUE] <- 0
			pa.bird.data$detect[is.na(pa.bird.data$detect)==TRUE] <- 0

			# Second add LULC data
			if (is.na(single.yr)==FALSE) { pa.bird.data$count_yr <- rep(single.yr,length(pa.bird.data$count_yr)) }
			pa.bird.data <- merge(pa.bird.data, all.data, by=c('abbrev','count_yr'),all.x=TRUE)
			# print(dim(pa.bird.data)); print(head(pa.bird.data)); stop('cbw')
			
			# Save for building models in the next step
			save(pa.bird.data, file=paste(workspace,'/Species/gp.lulc.',ver,'.',species,'.r',the.radius,'m.',cell.size*ag.factor,'m.rdata',sep=''))
			# stop('cbw')
			cat(species,' ')
		}
	cat('\n')
	}
}

# Testing
# write.csv(pa.bird.data, paste(workspace,'/test.csv',sep=''))
# for (i in c(1365,307,239)) { print(bird.data[bird.data$how_many==i,]); print(pa.bird.data[pa.bird.data$how_many==i,]) }

# Merge such that we include circles that were surveyed in a given year and count as absences.

