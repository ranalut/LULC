# Packages
library(raster)
library(rgdal)
library(sp)
library(ncdf)

# Functions
sum.na <- function(x) { return(sum(is.na(x))) }

# Workspace
drive <- 'd'
workspace <- paste(drive,':/ClimateData',sep='')
# the.radius <- 12070 # c(12070,24140) # 12070 # 48280 # 24140
# cell.size <- 250

r.raster.temp <- 'C:/Users/cwilsey/AppData/Local/Temp/R_raster_cwilsey2/'

# Load pts.
cbc <- readOGR(dsn=workspace,layer='CBC_circles',encoding='ESRI Shapefile')
cat('cbc pts loaded\n')
# plot(cbc); print(dim(cbc))
# stop('cbw')

# Time Series
rasterOptions(tmpdir=r.raster.temp) 
file.prefix <- c('/Maurer2010Historical/nldas_met_update.obs.monthly.')
variables <- c('pr','tas','tasmax','tasmin')

for (n in 1:length(variables))
{
	hist.clim <- list()

	for (i in 1966:2005)
	{
		list.name <- paste('y',i,sep='')
		file.name <- paste(workspace,file.prefix,variables[n],'.',i,'.nc',sep='')

		climate.data <- brick(file.name)
			
		temp <- extract(climate.data, cbc, df=TRUE, nl=12)
		temp <- data.frame(abbrev=cbc@data$abbrev,temp)
		test <- apply(temp,1,sum.na)
		hist.clim[[list.name]] <- temp[test==0,]
		# stop('cbw')
			
		save(hist.clim, file=paste(workspace,'/Maurer2010Historical/maurer.hist.cbc.',variables[n],'.rdata',sep=''))
		
		# print(head(hist.clim[[list.name]]))
		
		# Remove all the temporary files for that year's calculations.  
		file.remove(dir(r.raster.temp,full.names=TRUE))
		# stop('cbw')
		cat('done',variables[n],' ',i,'\n')
	}
}

