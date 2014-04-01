
# Model evaluation


model.eval <- function(the.model, covariates, test.rows, obs, spp)
# model.eval <- function(the.model=nass.models[[i]], covariates=nass.spp.data[[i]][,c(4:5,9:26)], test.rows=test.rows, obs=nass.spp.data[[i]][,6], spp=spp.names[i])
{
	test <- predict.gbm(newdata=covariates[test.rows,], the.model, n.trees=the.model$n.trees, type='response', progress='window', na.rm=TRUE)
	plot(test ~ obs[test.rows], main=paste(spp,', fitted ~ obs',sep=''), xlab='counts', ylab='predicted')
	dev.exp.cv <- dsq(
		mean.null=the.model$self.statistics$mean.null, 
		validation=the.model$cv.statistics$deviance.mean
		)
	# cat('deviance explained training data',dev.exp.cv,'\n')
	cor.cv <- the.model$cv.statistics$correlation.mean
	
	dev.exp.test <- dsq(
		mean.null=calc.deviance(as.numeric(obs[test.rows]), rep(mean(as.numeric(obs[test.rows])),length(test)), family='poisson'),
		validation=calc.deviance(as.numeric(obs[test.rows]), test, family='poisson')
		)
	cat('deviance explained test data',dev.exp.test,'\n')
	cor.test <- cor(test,obs[test.rows])
	
	# stop('cbw')
	return(list(dev.exp.cv, dev.exp.test, cor.cv, cor.test))
}

