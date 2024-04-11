#' Third approach to estimate the epistemic and aleatoric uncertainty involved in a binary classification.
#' 
#' @author Robin Senge (`rsenge@inovex.de`)

# rm(list=ls())

source("R/2014-reliable-classification/commons.R", chdir=T)

# ----------------------------------------
# Fitness function for optimization
# ----------------------------------------
fitness.log <- function(train, bM, bM0, x, loglikeM0) {
	
	clindex <- dim(train)[2]
	
	if(clindex != length(bM)) stop("Dimensions of data and bM do not match!")
	if(clindex != length(bM0)) stop("Dimensions of data and bM0 do not match!")
	if(clindex != length(x)) stop("Dimensions of data and x do not match!")
	
	labels <- train[,clindex]
	train <- train[,1:(clindex-1)]
	
	l <- x[clindex]
	x <- t(x[1:(clindex-1)])
	
	# x
	v <- lin(bM, x)
	px <- ifelse(l == 0, 1-trans(v), trans(v))
	px <- ifelse(px == 0, rep(eps,length=length(px)), px)

	# M
	vs <- apply(train, 1, function(x){lin(bM, x)})
	ps <- apply(t(vs), 1, trans)
	ps <- ifelse(labels == 0, 1-ps, ps)
	ps <- ifelse(ps == 0, rep(eps,length=length(ps)), ps)

	
	like <- exp(sum(log(ps))-loglikeM0)
	fitness <- min(like, high(px))
	
	fitness
}

# ----------------------------------------
# Calculating the weak preferences
# ----------------------------------------
weakPref.log <- function(form, train, test) {

	Pmat <- matrix(nrow=dim(test)[1], ncol=2)
	colnames(Pmat) <- c('weakPref0','weakPref1')
	bM0 <- glm(form, data=train, family=binomial)$coefficients
	#cntrl = trainControl(method = "none", returnData = F)
	#bM0 <- train(x, y, trControl=cntrl, method="glm")$coefficients
	bM0 <- ifelse(is.na(bM0), rep(0, length=length(bM0)), bM0)
	
	# log-likelihood of M0
	v0s <- apply(train[,-dim(train)[2]], 1, function(x){lin(bM0, x)})
	p0s <- apply(t(v0s), 1, trans)
	p0s <- ifelse(train[,dim(train)[2]] == 0, 1-p0s, p0s)
	loglikeM0 <- sum(log(p0s))
	
	for(i in 1:dim(test)[1]) {
		
	  tryCatch({
	    
		  x0 <- as.numeric(c(test[i,],0))
		  Pn <- optim(bM0, function(bM) {fitness.log(train, bM, bM0, x0, loglikeM0)}, control=c(fnscale=-1, maxit=100))$value
		
		  x1 <- as.numeric(c(test[i,],1))
		  Pp <- optim(bM0, function(bM) {fitness.log(train, bM, bM0, x1, loglikeM0)}, control=c(fnscale=-1, maxit=100))$value
		  
		  Pmat[i,1] <- Pn 
		  Pmat[i,2] <- Pp 
		  
	  }, finally={
	    
	    Pmat[i,1] <- NA 
	    Pmat[i,2] <- NA
	  })
	
	}
	Pmat
}

relike.optim.uq.log = function(form, train, test) {

  wp <- weakPref.log(form, train, test)
  tfpr = foreach(i=1:nrow(wp), .combine ="rbind") %do% {
    # i <- 1
    fpr(wp[i, 1], wp[i, 2])
  }
  colnames(tfpr) <- c("p0_optim", "p1_optim", "ua_optim", "ue_optim", "ut_optim")
  tfpr
}



# ---------------------------------------
# Implementation for table classifier
# ---------------------------------------

toCell <- function(x) {sum(2^(which(x=='1')-1))}


buildMLM <- function(train) {

	dims <- dim(train)[2]
	model <- as.numeric(by(train[,dims], train[,1:(dims-1)], function(v) { length(which(v=='1'))/length(v) }))
	ifelse(is.na(model), 0.5, model)
	
}


predMLM <- function(model, test) {
	
	cells <- apply(test, 1, toCell)+1
	model[cells]
			
}

fitness.table <- function(train, bMx, bM0, x, cell, loglikeM0) {
	
	clindex <- dim(train)[2]
	
	labels <- train[,clindex]
	train <- train[,1:(clindex-1)]
	
	l <- x[clindex]
	x <- t(x[1:(clindex-1)])
	bM <- bM0
	bM[cell] <- bMx
		
	# x
	v <- predMLM(bM, x)
	px <- ifelse(l == 0, 1-v, v)
	px <- ifelse(px == 0, rep(eps,length=length(px)), px)

	# M
	vs <- apply(train, 1, function(x){predMLM(bM, t(x))})
	ps <- ifelse(labels == 0, 1-vs, vs)
	ps <- ifelse(ps == 0, rep(eps,length=length(ps)), ps)

	like <- exp(sum(log(ps))-loglikeM0)
	fitness <- min(like, high(px))
	
	fitness
}

weakPref.table <- function(train, test) {

	Pmat <- matrix(nrow=dim(test)[1], ncol=2)
	colnames(Pmat) <- c('weakPref0','weakPref1')
	
	bM0 <- buildMLM(train)
	
	# log-likelihood of M0
	v0s <- apply(train[,-dim(train)[2]], 1, function(x){predMLM(bM0, t(x))})
	p0s <- ifelse(train[,dim(train)[2]] == 0, 1-v0s, v0s)
	p0s <- ifelse(p0s == 0, rep(eps,length=length(p0s)), p0s)
	loglikeM0 <- sum(log(p0s))
	
	for(i in 1:dim(test)[1]) {
		
		# i <- 1
		
		cell <- toCell(test[i,])+1
		
		x0 <- as.numeric(c(test[i,],0))
		#Pn <- optim(bM0[cell], function(bM) {fitness.table(train, bM, bM0, x0, cell, loglikeM0)}, method='Brent', lower=0, upper=1, control=c(fnscale=-1))$value
		Pn <- optimize(function(bM) {fitness.table(train, bM, bM0, x0, cell, loglikeM0)}, lower=0, upper=1, maximum=TRUE, tol=1e-3)$objective
		
		x1 <- as.numeric(c(test[i,],1))
		#Pp <- optim(bM0[cell], function(bM) {fitness.table(train, bM, bM0, x1, cell, loglikeM0)}, method='Brent', lower=0, upper=1, control=c(fnscale=-1))$value
		Pp <- optimize(function(bM) {fitness.table(train, bM, bM0, x1, cell, loglikeM0)}, lower=0, upper=1, maximum=TRUE, tol=1e-3)$objective
		
		Pmat[i,1] <- Pn 
		Pmat[i,2] <- Pp 
	
	}
		
	Pmat
		
}


# ---------------------------------------
# Implementation for Bernoulli Classifier
# ---------------------------------------

buildBernoulli <- function(train) {
  (length(which(train == 1))+1) / (length(train)+2)
}

predBernoulli <- function(model, n) {
  rep(model, length=n)
}

fitness.bernoulli <- function(train, bM, bM0, l, loglikeM0) {
  
  # x
  v <- predBernoulli(bM, 1) # constant model is not dependent on x!
  px <- ifelse(l == 0, 1-v, v)
  
  
  # M
  vs <- sapply(train, function(x){predBernoulli(bM, 1)})
  ps <- ifelse(train == 0, 1-vs, vs)
  
  like <- exp(sum(log(ps))-loglikeM0)
  fitness <- min(like, high(px))
  
  fitness
}

weakPref.bernoulli <- function(train, test) {
  
  Pmat <- matrix(nrow=length(test), ncol=2)
  colnames(Pmat) <- c('weakPref0','weakPref1')
  
  bM0 <- buildBernoulli(train)
  
  # log-likelihood of M0
  v0s <- sapply(train, function(x){predBernoulli(bM0, 1)})
  p0s <- ifelse(train == 0, 1-v0s, v0s)
  
  loglikeM0 <- sum(log(p0s))
  
  for(i in 1:length(test)) {
    
    # i <- 1
    
    Pn <- optimize(function(bM) {fitness.bernoulli(train, bM, bM0, 0, loglikeM0)}, lower=0, upper=1, maximum=TRUE, tol=1e-3)$objective
    
    Pp <- optimize(function(bM) {fitness.bernoulli(train, bM, bM0, 1, loglikeM0)}, lower=0, upper=1, maximum=TRUE, tol=1e-3)$objective
    
    Pmat[i,1] <- Pn 
    Pmat[i,2] <- Pp 
    
  }
  
  Pmat
  
}





 
