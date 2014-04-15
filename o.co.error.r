
# Calculate omission and commission error from a confustion matrix

o.co.error <- function(mat)
{
	if(sum(dim(mat))!=4) { return(list(NA,NA)) }
	omission <- round(100*mat[1,2]/sum(mat[,2]))
	commission <- round(100*mat[2,1]/sum(mat[,1]))
	return(list(omission,commission))
}
