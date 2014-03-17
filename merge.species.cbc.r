
source('rbind.lulc.r')
source('add.col.r')

# Workspace and parameters
drive <- 'z'
workspace <- paste(drive,':/LULC',sep='')
the.radius <- 24140 # 48280 # 24140
cell.size <- 250
ag.factor <- 1 # 4

# Load and crop CBC pts.
cbc <- readOGR(dsn=workspace,layer='CBC_circles_alb',encoding='ESRI Shapefile')
cat('cbc pts loaded\n')
the.crop <- raster(paste(workspace,'/gp_backcast_1938_1992/gp_lcyear_1992.tif',sep=''))
test <- extract(the.crop,cbc)
cbc <- cbc[is.na(test)==FALSE & test!=0,]
pt.yr <- read.csv(paste(workspace,'/CirclesByYear.csv',sep=''))
cat('all pts in history',dim(pt.yr),'\n')
pt.yr <- pt.yr[pt.yr$abbrev %in% cbc$abbrev,]
cat('all pts in study area',dim(pt.yr),'\n')
pt.yr <- pt.yr[pt.yr$count_yr <= 105 & pt.yr$count_yr >= 66,]
cat('all pts in study years',dim(pt.yr),'\n')
# test <- SpatialPoints(pt.yr[,c('latitude','longitude')])
# plot(test)

# Load LULC data
load(paste(workspace,'/Historical/gp.hist.',cell.size*ag.factor,'m.cbc.r',the.radius,'m.rdata',sep=''))
load(paste(workspace,'/gp_backcast_1938_1992/gp.backcast.',cell.size*ag.factor,'m.cbc.r',the.radius,'m.rdata',sep=''))
all.data <- c(gp.backcast,historical)
all.data <- rbind.lulc(lulc.list=all.data, years=seq(1966,2005,1), pts=cbc)
cat('all data',dim(all.data),'\n')

# Loop through species...
spp <- read.csv('z:/lulc/gp_focal_spp_list.csv')

species <- 'smlo'
bird.data <- read.csv(paste(workspace,'/',species,'.csv',sep=''))
bird.data <- bird.data[,c('abbrev','count_yr','how_many','detect')]

# First add the species count data
pa.bird.data <- merge(pt.yr,bird.data,by=c('abbrev','count_yr'),all.x=TRUE)
print(dim(pa.bird.data))
pa.bird.data$how_many[is.na(pa.bird.data$how_many)==TRUE] <- 0
pa.bird.data$detect[is.na(pa.bird.data$detect)==TRUE] <- 0

# Second add LULC data
pa.bird.data <- merge(pa.bird.data, all.data, by=c('abbrev','count_yr'),all.x=TRUE)

# Save for building models in the next step


# Testing
# write.csv(pa.bird.data, paste(workspace,'/test.csv',sep=''))
# for (i in c(1365,307,239)) { print(bird.data[bird.data$how_many==i,]); print(pa.bird.data[pa.bird.data$how_many==i,]) }



# Merge such that we include circles that were surveyed in a given year and count as absences.

