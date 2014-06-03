
lm.and.offset.models <- function(the.data)
{
	lm.model <- lm(the.data[,2] ~ the.data[,1])
	offset.model <- lm(the.data[,2] ~ the.data[,1] + offset(the.data[,1])
	
	return(list(lm.model,offset.model))
}
