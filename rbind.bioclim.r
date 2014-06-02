# rbind from a list and add columns
# source('add.col.r')

rbind.bioclim <- function(bioclim.list, years, pts)
{
	the.names <- paste('y',years,sep='')
	the.data <- bioclim.list
	n <- dim(the.data[[1]])[1]
	
	for (i in 1:length(the.names))
	{
		the.data[[the.names[i]]] <- data.frame(count_yr=rep((years[i]-1900),n),the.data[[the.names[i]]])
	}
	# return(the.data)
	
	output <- the.data[[the.names[1]]]
	# cat(dim(output)[1],' ')
	for (i in 2:length(the.names))
	{
		output <- rbind(output,the.data[[the.names[i]]])
		# cat(dim(output)[1],' ')
	}
	return(as.data.frame(output))
}

