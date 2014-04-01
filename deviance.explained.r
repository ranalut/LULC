

dsq <- function(mean.null, validation)
{
	output <- (mean.null - validation) / mean.null
	return(round(output,3))
}

# Dsquared <- function(model, adjust = FALSE) {
# # version 1.1 (13 Aug 2013)
# # calculates the explained deviance of a GLM
# # model: a model object of class "glm"
# # adjust: logical, whether or not to use the adjusted deviance taking into acount the nr of observations and parameters (Weisberg 1980; Guisan & Zimmermann 2000)
  # d2 <- (model$null.deviance - model$deviance) / model$null.deviance
  # if (adjust) {
    # n <- length(model$fitted.values)
    # p <- length(model$coefficients)
    # d2 <- 1 - ((n - 1) / (n - p)) * (1 - d2)
  # }
  # return(d2)
# }  # end Dsquared function