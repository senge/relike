# rm(list=ls())

###########################################
# Bernoulli experiment for third approach #
###########################################

# Published in SENGE, Robin, et al. Reliable classification: Learning classifiers that distinguish aleatoric and epistemic uncertainty. Information Sciences, 2014, 255. Jg., S. 16-29.

source('R/2014-reliable-classification/third_approach.R', chdir=T)

num_repetitions = 50
num_instances = 200
coin_biases <- seq(0.0, 0.5, by=0.25) # dice probabilities

data <- sapply(coin_biases, 
              function(p){
                sapply(1:num_repetitions, 
                       function(i){rbinom(num_instances, 1, p)})}, 
              simplify=FALSE)

results <- array(dim=c(length(coin_biases), num_repetitions, num_instances, 4))

# Iterate over different coin biases
for(i in 1:length(coin_biases)) {
	
	# i <- 1
	p <- coin_biases[i]
	trains <- data[[i]]
	
	# Iterate over experiment repetitions
	for(r in 1:num_repetitions) {

		train <- trains[,r]
		weakPrefs <- t(sapply(1:length(train)-1, function(j) {
			
			# Train data is dice rolls 1:j and test using dice roll j+1
			weakPref.bernoulli(train[1:j], train[j+1])
			
		}))
		
		results[i,r,,] <- t(apply(weakPrefs, 1, function(inst){fpr(inst[1], inst[2])}))
		
		message(paste('-',r))
	}
	message(paste('+',p))
}

save(results, file='bernoulli_results.Robj')
load(file='bernoulli_results.Robj')

mean_results <- apply(results, c(1,3,4), mean)
mean_results[,1,3] <- 0 
mean_results[,1,4] <- 1

cols <- c('dark red', 'dark green', 'dark blue')

par(mfrow=c(2,2))

# Plotting strict positive preference
plot(range(1:num_instances), range(mean_results[,,3:4]), type='n', main='Strict Positive Preference', ylab='value' , xlab='instances')
for(i in 1:length(coin_biases)) {
  lines(1:num_instances, mean_results[i,,1], col=cols[i], lwd=2, lty=i+1)	
}
legend(5, 1, format(coin_biases, digits=2), col=cols, lty=2:4, title='p = ', lwd=2) 

# Plotting strict negative preference
plot(range(1:num_instances), range(mean_results[,,3:4]), type='n', main='Strict Negative Preference', ylab='value' , xlab='instances')
for(i in 1:length(coin_biases)) {
  lines(1:num_instances, mean_results[i,,2], col=cols[i], lwd=2, lty=i+1)	
}
legend(5, 1, coin_biases, col=cols, lty=2:4, title='p = ', lwd=2) 

# Plotting Aleatoric Uncertainty
plot(range(1:num_instances), range(mean_results[,,3:4]), type='n', main='Bernoulli - Aleatoric Uncertainty', ylab='value' , xlab='instances')
for(i in 1:length(coin_biases)) {
	lines(1:num_instances, mean_results[i,,3], col=cols[i], lwd=2, lty=i+1)	
}
legend(5, 1, format(coin_biases, digits=2), col=cols, lty=2:4, title='p = ', lwd=2) 

# Plotting Epistemic Uncertainty
plot(range(1:num_instances), range(mean_results[,,3:4]), type='n', main='Bernoulli - Epistemic Uncertainty', ylab='value' , xlab='instances')
for(i in 1:length(coin_biases)) {
	lines(1:num_instances, mean_results[i,,4], col=cols[i], lwd=2, lty=i+1)	
}
legend(5, 1, coin_biases, col=cols, lty=2:4, title='p = ', lwd=2) 

