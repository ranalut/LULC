# rbind from a list and add columns

rbind.lulc <- function(lulc.list, years, pts)
{
	the.names <- paste('y',years,sep='')
	n <- seq(1,length(names(lulc.list)),1)
	test <- names(lulc.list) %in% the.names
	n <- n[test==TRUE]
	the.data <- lulc.list[[n]]
	
	for (i in 1:length(the.names))
	{
		the.data[[i]] <- data.frame(abbrev=pts$abbrev, count_yr=rep((years[i]-1900),dim(the.data)[1]),the.data[[i]])
	}
	
	output <- the.data[[1]]
	for (i in 2:length(the.names))
	{
		output <- rbind(output,the.data[[i]])
	}
	return(as.data.fram(output))
}


