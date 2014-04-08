
add.col <- function(in.data,ncols)
{
	target.names <- paste('X',ncols,sep='')
	# cat('col names',colnames(in.data),'\n')
	
	temp <- as.list(data.frame(in.data))
	# print(temp); stop('cbw')
	temp.names <- names(temp)
	# cat('temp.names',temp.names,'\n')
	
	test <- target.names %in% temp.names
	# cat('test',test,'\n')
	add.col <- target.names[test==FALSE]
	cat('add.col',add.col,'\n')
	
	if(length(add.col)==0) { return(in.data) }
	
	empty.cols <- data.frame(matrix(rep(0,length(add.col)*dim(in.data)[1]),ncol=length(add.col)))
	colnames(empty.cols) <- add.col
	empty.cols <- as.list(empty.cols)
	# cat('empty.cols names',names(empty.cols),'\n')
	output <- c(temp,empty.cols)
	# cat('output names',names(output),'\n')
	output <- as.data.frame(output)
	output <- round(output,4)
	return(output)
}


