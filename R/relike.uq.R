
#' Uncertainty quantification based on relative likelihood within a bootstrapped ensemble.
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

relike.uq = function(x, y, x0, train_fun, predict_fun, bag_size=100, seed=23,
                     incremental=F, min_sample_frac=0.1) {
  
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
    
    # b = 1
    
    # Do the sampling
    if(incremental) {
      sample_idx = sample(1:n, min(max(ceiling(b*n/bag_size), ceiling(min_sample_frac*n)), n), replace = TRUE)
    } else {
      sample_idx = sample(1:n, replace = TRUE)   
    }
    
    # Check for missing labels in train dataset -> add at least one example (randomly)
    missing_label = labs[which(table(y[sample_idx]) == 0)]
    if(length(missing_label) > 0) {
      sample_idx = c(sample_idx, sample(which(y == missing_label), 1))
    }
    
    sample_x = x[sample_idx,]
    sample_y = y[sample_idx]
    
    # Train the model
    train_fun(sample_x, sample_y)
  }
  
  message("Trained ensemble.")
  
  # Calculate log-likelihoods of bootstrapped models (on original training data)
  log_likes <- foreach(m = iter(ensemble), .combine = "c") %do% {
    pred.raw = predict_fun(m, x)
    classes = colnames(pred.raw)
    sum(log(sapply(1:n, function(i) {
      pred.raw[i,which(classes == y[i])]
    })))
  }
  
  # Relative / normalized likelihoods of models
  rel_likes = exp(log_likes - max(log_likes))
  
  # Calculate probabilistic predictions of test instances
  tmp <- foreach(m = iter(ensemble)) %do% 
    predict_fun(m, x0)
  
  pred_x0 <- 
    foreach(label = iter(labs)) %do% {
      foreach(preds = iter(tmp), .combine = "cbind") %do% {
        preds[, label]
      }
    }
    
  # Weak preferences / plausibilities
  weakprefs <- matrix(unlist(
    foreach(label = iter(labs)) %do% {
      foreach(ps = iter(pred_x0[[which(labs == label)]], by = "row"), .combine = "rbind") %do% {
    
        # weak preference of class
        ps = as.numeric(ps)
        high_ps = pmax(0, 2*ps-1)
        wpref = max(pmin(rel_likes, high_ps))
      }
    }), ncol=2)
  
  # uncertainty values
  uq = foreach(wp = iter(weakprefs, by="row"), .combine = "rbind") %do% {
    c(ua=min(1-wp), ue=min(wp), ut=min(1-wp)+min(wp))
  }
  rownames(uq) <- NULL
  
  message("Estimated uncertainty.")
  
  # averaged probabilities
  avgProbs <- matrix(unlist(
    foreach(label = iter(labs)) %do% {
      foreach(ps = iter(pred_x0[[which(labs == label)]], by = "row"), .combine = "rbind") %do% {
        mean(ps)
      }
    }), ncol=2)
  colnames(avgProbs) <- paste0("p", labs)
  
  message("Averaged probabilities.")
  
  return(cbind(avgProbs, uq))
}