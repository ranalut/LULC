library(rgdal)
library(raster)
library(sp)

source('rbind.bioclim.r')
source('rbind.lulc.r')

# Workspace and parameters
workspace.d <- 'D:/GreatPlains_LULC_Climate/'
workspace.z <- 'Z:/GreatPlains_LULC_Climate/'
data.d <- 'd:/ClimateData'
data.z <- 'z:/LULC'

the.radius <- 12070
cell.size <- 250
ag.factor <- 1

single.yr <- NA # 105 #NA
ver <- 8

# Load and crop CBC pts.
cbc <- readOGR(dsn=data.z,layer='CBC_circles_alb',encoding='ESRI Shapefile')
cat('cbc pts loaded\n')
the.crop <- raster(paste(data.z,'/Historical/conus_historical_y1992.img',sep=''))
test <- extract(the.crop,cbc)
cbc <- cbc[is.na(test)==FALSE & test!=0,] 
# plot(cbc); stop('cbw')
cbc@data$abbrev <- as.character(cbc@data$abbrev)
pt.yr <- read.csv(paste(data.z,'/CirclesByYear.csv',sep=''), stringsAsFactors=FALSE)
cat('all pts in history',dim(pt.yr),'\n')
pt.yr <- pt.yr[pt.yr$abbrev %in% cbc@data$abbrev,]
cat('all pts in study area',dim(pt.yr),'\n')
pt.yr <- pt.yr[pt.yr$count_yr <= 105 & pt.yr$count_yr >= 92,] # CONUS LULC only available post 1992.
cat('all pts in study years',dim(pt.yr),'\n')
# test <- SpatialPoints(pt.yr[,c('longitude','latitude')])
# plot(test); stop('cbw')

# Load BIOCLIM data
load(paste(data.d,'/Maurer2010Historical/maurer.hist.cbc.bioclim.rdata',sep='')) # Called hist.bioclim
all.data <- rbind.bioclim(bioclim.list=hist.bioclim, years=seq(1966,2005,1), pts=cbc)

# Load LULC data
load(paste(data.d,'/Historical/conus.hist.',cell.size*ag.factor,'m.cbc.r',the.radius,'m.rdata',sep=''))
temp <- rbind.lulc(lulc.list=historical, years=seq(1966,2005,1), pts=cbc)	

all.data <- data.frame(all.data,temp[,11:27])

cat('all data',dim(all.data),'\n')
stop('cbw')

# Loop through species...
spp <- read.csv(paste(data.z,'/gp_focal_spp_list_v',ver,'.csv',sep=''), stringsAsFactors=FALSE, row.names=1)

for (i in 1:dim(spp)[1])
{
	species <- spp$BBL_ABBREV[i]
	bird.data <- read.csv(paste('z:/CBC-Data/',species,'.csv',sep=''), stringsAsFactors=FALSE)
	bird.data <- bird.data[,c('abbrev','count_yr','how_many','detect')]

	# First add the species count data
	pa.bird.data <- merge(pt.yr,bird.data,by=c('abbrev','count_yr'),all.x=TRUE)
	# print(dim(pa.bird.data))
	pa.bird.data$how_many[is.na(pa.bird.data$how_many)==TRUE] <- 0
	pa.bird.data$detect[is.na(pa.bird.data$detect)==TRUE] <- 0

	# Second add BioClimate data
	if (is.na(single.yr)==FALSE) { pa.bird.data$count_yr <- rep(single.yr,length(pa.bird.data$count_yr)) }
	pa.bird.data <- merge(pa.bird.data, all.data, by=c('abbrev','count_yr'),all.x=TRUE)
	# print(dim(pa.bird.data)); print(head(pa.bird.data)); stop('cbw')
	
	# Save for building models in the next step
	save(pa.bird.data, file=paste(workspace.d,'/CONUS/Species/conus.lulc.bioclim.',ver,'.',species,'.rdata',sep=''))
	# stop('cbw')
	cat(species,' ')
}
cat('\n')

# Testing
# write.csv(pa.bird.data, paste(workspace,'/test.csv',sep=''))
# for (i in c(1365,307,239)) { print(bird.data[bird.data$how_many==i,]); print(pa.bird.data[pa.bird.data$how_many==i,]) }

# Merge such that we include circles that were surveyed in a given year and count as absences.

