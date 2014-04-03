

mean.excl.na <- function(x,...) # the.weights,...)
{
	# print(x) # focal multiplies by weigth then provides data by row.  L to R top row, second row, ...
	# stop('cbw')
	if (is.na(x[ceiling(length(x)/2)])==TRUE) { return(NA) }
	
	denominator <- length(x[is.na(x)==FALSE & x>0])
	x <- x / denominator
	return(sum(x, na.rm=TRUE))
}
