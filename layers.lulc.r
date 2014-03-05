library(raster)
library(sp)

# Functions
layers.lulc <- function(file.in, the.crop, ag.fact, ag.fun)
{
	# Load LULC data.
	the.data <- raster(file.in)
	cat('raster loaded... ')
	
	# Crop data
	if (is.na(the.crop)==FALSE)
	{
		r.crop <- raster(the.crop)
		the.data <- crop(the.data, r.crop)
		# plot(the.data); stop('cbw')
		cat('cropped... ')
	}
	
	# Aggregate data
	if (is.na(ag.fact)==FALSE)
	{
		the.data <- aggregate(the.data, fact=ag.fact, fun=ag.fun)
		# plot(the.data); stop('cbw')
		cat('aggregated... ')
	}
	
	# Create a binary raster brick with layerize.
	the.brick <- layerize(the.data) # This can take a long time.
	cat('layerized... ')

	return(the.brick)
}
