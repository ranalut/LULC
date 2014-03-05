


drive <- 'z'
workspace <- paste(drive,':/LULC',sep='')
the.radius <- 24140 # 48280 # 24140
cell.size <- 250
ag.factor <- 1 # 4

# Load and crop pts.
cbc <- readOGR(dsn=workspace,layer='CBC_circles_alb',encoding='ESRI Shapefile')
cat('cbc pts loaded\n')
the.crop <- raster(paste(workspace,'/gp_backcast_1938_1992/gp_lcyear_1992.tif',sep=''))
test <- extract(the.crop,cbc)
cbc <- cbc[is.na(test)==FALSE & test!=0,]

load(paste(workspace,'/Historical/gp.hist.',cell.size*ag.factor,'m.cbc.r',the.radius,'m.rdata',sep=''))
load(paste(workspace,'/gp_backcast_1938_1992/gp.backcast.',cell.size*ag.factor,'m.cbc.r',the.radius,'m.rdata',sep=''))

all.data <- c(gp.backcast,historical)


