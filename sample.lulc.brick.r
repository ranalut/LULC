library(raster)
library(sp)

sample.lulc.brick <- function(rdata.in, the.pts, the.radius) # Radius in map units of the CRS (m)
{
	load(rdata.in)
	cat('data loaded... ')
	
	# Sample at CBC points applying an appropriate buffer.
	the.output <- extract(the.brick, the.pts, buffer=the.radius, fun=mean, na.rm=TRUE)
	cat('extracted\n')
	
	return(the.output)
}
