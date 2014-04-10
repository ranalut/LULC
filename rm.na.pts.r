
# True means there is at least one NA in the vector.
rm.na.pts <- function(x)
{
	temp <- sum(is.na(x))
	ifelse(temp==0,return(FALSE),return(TRUE))
}

# test <- matrix(c(1,2,3,NA,1,2,3,4,1,2,3,NA),nrow=3,byrow=TRUE)
# test.out <- apply(X=test,MAR=1,FUN=rm.na.pts)
# print(test.out)
