
#' Uncertainty quantification based on information theoretic measures (entropy 
#' and mutual information) within a bootstrapped ensemble.
#'
#' @param x Matrix of features.
#' @param y Vector of responses (factor).
#' @param x0 Matrix of features of target instances for which aleatoric and 
#'   epistemic uncertainty as well ensemble averaged probabilities shall be 
#'   calculated. 
#' @param train_fun A function to perform model training. Parameters of the 
#'   train_fun should be a feature matrix x and a response factor vector. The 
#'   returned object represents one model, estimating E(Y|X).
#' @param predict_fun A function to perform prediction new feature values. 
#'   Its input arguments should be a model produced by train_fun, and a matrix
#'   of new feature values at which we want to make predictions.
#' @param bag_size Size of the ensemble.
#' @param seed Random seed in order to be able to reproduce exact experiments.

info.uq = function(x, y, x0, train_fun, predict_fun, bag_size=100, seed=23) {
  
  #x = df_train_x
  #y = df_train_y
  #x0 = df_test_x
  
  
  # Requirements
  require(foreach)
  require(iterators)
  
  # Initialization
  set.seed(seed)
  x = as.matrix(x)
  y = as.factor(y)
  nc = length(levels(y))
  labs = levels(y)
  n = nrow(x)
  p = ncol(x)
  x0 = as.matrix(x0)
  n0 = nrow(x0)
  
  # Check parameter
  if(nrow(x) != length(y)) stop("Number of rows of matrix x needs to be equal to length of vector y.")
  if(nc != 2) stop("Number of classes is not two (but ", length(levels(y)), "). Currently only binary classification is implemented.")
  
  # Train classifiers with bootstrapped data samples
  ensemble = foreach(b = 1:bag_size) %do% {
    
    sample_idx = sample(1:n, replace = TRUE) 
    sample_x = x[sample_idx,]
    sample_y = y[sample_idx]
    
    train_fun(sample_x, sample_y)
  }
  
  message("Trained ensemble.")
  
  # Calculate probabilistic predictions of test instances
  tmp <- foreach(m = iter(ensemble)) %do% 
    predict_fun(m, x0)
  
  pred_x0 <- array(dim = c(length(tmp), dim(tmp[[1]])), 0) 
  for(m in 1:length(tmp)) {pred_x0[m,,] <- tmp[[m]]}
  
  # aleatoric uncertainty
  ua <- apply(
    apply(pred_x0, 1:2, function(p){sum(-p*log(p))}),
    2, function(i){mean(i)})
  
  # total uncertainty
  ut <- 
    apply(
      apply(pred_x0, 2:3, function(p){mean(p)}),
      1, function(p){sum(-p*log2(p))})
  
  # epistemic uncertainty
  ue <- ut - ua
  
  message("Calculated uncertainies.")
  
  # averaged probabilities
  
  avgProbs <- apply(pred_x0, 2:3, mean)
  colnames(avgProbs) <- paste0("p", labs)
  
  message("Averaged probabilities.")
  
  return(cbind(avgProbs, ua, ue, ut))
  
}