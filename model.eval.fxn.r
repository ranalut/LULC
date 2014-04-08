
# Model evaluation
library(PresenceAbsence)

model.eval.abund <- function(the.model, covariates, obs, spp)
# model.eval <- function(the.model=nass.models[[i]], covariates=nass.spp.data[[i]][,c(4:5,9:26)], test.rows=test.rows, obs=nass.spp.data[[i]][,6], spp=spp.names[i])
{
	test <- predict.gbm(newdata=covariates, the.model, n.trees=the.model$n.trees, type='response', progress='window', na.rm=TRUE)
	plot(test ~ obs, main=paste(spp,', fitted ~ obs',sep=''), xlab='counts', ylab='predicted')
	dev.exp.cv <- dsq(
		mean.null=the.model$self.statistics$mean.null, 
		validation=the.model$cv.statistics$deviance.mean
		)
	# cat('deviance explained training data',dev.exp.cv,'\n')
	cor.cv <- the.model$cv.statistics$correlation.mean
	
	dev.exp.test <- dsq(
		mean.null=calc.deviance(as.numeric(obs), rep(mean(as.numeric(obs)),length(test)), family='poisson'),
		validation=calc.deviance(as.numeric(obs), test, family='poisson')
		)
	cat('deviance explained test data',dev.exp.test,'\n')
	cor.test <- cor(test,obs)
	
	# stop('cbw')
	return(list(dev.exp.cv, dev.exp.test, cor.cv, cor.test))
}


model.eval.binary <- function(the.model, covariates, test.cov)
{
	# Threshold
	train.pred <- predict(the.model, newdata=covariates, type='prob')
	thresh.table <- data.frame(seq(1,dim(covariates)[2],1),covariates[,c('detect')],train.pred[,2])
	# thresh.table$detect <- as.numeric(thresh.table$detect) - 1
	auc <- auc(thresh.table) 
	print(auc)
	thresh.optim <- optimal.thresholds(DATA=thresh.table, opt.methods=c('Sens=Spec','MaxKappa','ReqSpec'),req.spec=1)
	print(thresh.optim)
	cutoff <- thresh.optim[2,2] # Choose the max kappa threshold
	
	# Test Data
	test.pred <- predict(the.model, newdata=test.cov, type='prob')
	test.pred <- as.data.frame(test.pred)
	test.pred$pred <- ifelse(as.numeric(test.pred[,2]) >= cutoff,1,0)
	test.pred$obs <- as.numeric(test.cov[,'detect']) - 1
	conf.matrix <- table(test.pred[,3:4])
	print(conf.matrix)

	return(list(auc,cutoff,conf.matrix))
}
