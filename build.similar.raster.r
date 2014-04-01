
build.similar <- function(file.name,value,var.name)
{
	temp <- raster(file.name)
	temp <- setValues(temp,value)
	names(temp) <- var.name
	return(temp)
}

