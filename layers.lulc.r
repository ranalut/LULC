library(raster)
library(sp)

# Functions
layers.lulc <- function(file.in, rdata.out, the.crop)
{
	# Load LULC data.
	the.data <- raster(file.in)
	cat('raster loaded... ')
	
	# Crop data
	r.crop <- raster(the.crop)
	the.data <- crop(the.data, r.crop)
	cat('cropped... ')
	
	# Create a binary raster brick with layerize.
	the.brick <- layerize(the.data) # This can take a long time.
	cat('layerized... ')

	save(the.brick, file=rdata.out) # These files are too big to save. Cropped is 10+ GB.
	cat('written to disc\n')
}
