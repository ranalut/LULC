
# Model evaluation
library(PresenceAbsence)
library(dismo)

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

model.eval.binary <- function(the.model, covariates, test.cov, n.trees=NA)
{
	# Threshold
	if (is.na(n.trees)==FALSE) 
	{
		train.pred <- predict(the.model, newdata=covariates, type='response',n.trees=n.trees)
		temp <- train.pred
	}
	else 
	{
		train.pred <- predict(the.model, newdata=covariates, type='prob')
		temp <- as.numeric(train.pred[,2])
	}
	thresh.table <- data.frame(sites=seq(1,dim(covariates)[1],1),obs=covariates[,c('detect')],pred=temp)
	# print(head(thresh.table)); print(table(thresh.table$obs))
	auc <- auc(thresh.table) 
	cat('AUC... ',as.numeric(auc[1]),'\n')
	auc.roc.plot(thresh.table)
	thresh.optim <- optimal.thresholds(DATA=thresh.table, opt.methods=c('Sens=Spec','MaxKappa','ReqSpec'),req.spec=1)
	# print(thresh.optim)
	cutoff <- thresh.optim[2,2] # Choose the max kappa threshold
	cat('cutoff... ',cutoff,'\n')
	thresh.table$pred <- ifelse(as.numeric(thresh.table$pred) >= cutoff,1,0) 
	conf.matrix.train <- table(thresh.table[,2:3])
	
	# Test Data
	if (is.na(n.trees)==FALSE) { pred.prob <- predict(the.model, newdata=test.cov, type='response', n.trees=n.trees) }
	else { pred.prob <- predict(the.model, newdata=test.cov, type='prob') }
	if (is.vector(pred.prob)==FALSE) { pred.prob <- as.numeric(pred.prob[,2]) }
	
	test.pred <- data.frame(sites=seq(1,dim(test.cov)[1]))
	test.pred$obs <- as.numeric(test.cov[,'detect'])
	test.pred$pred <- ifelse(pred.prob >= cutoff,1,0)
	conf.matrix.test <- table(test.pred[,c('pred','obs')])
	# print(conf.matrix.test)
	
	kap <- Kappa(cmx(test.pred,threshold=cutoff),st.dev=FALSE)
	cat('Kappa... ',kap,'\n')
	
	dev.exp.test <- dsq(
		mean.null=calc.deviance(as.numeric(test.cov$detect), rep(mean(as.numeric(test.cov$detect)),dim(test.cov)[1]), family="bernoulli"),
		validation=calc.deviance(as.numeric(test.cov$detect), pred.prob, family="bernoulli")
		)
	cat('deviance explained... ',dev.exp.test,'\n')
	return(list(round(auc[1],3),round(kap,3), cutoff, conf.matrix.train, conf.matrix.test, dev.exp.test))
}
