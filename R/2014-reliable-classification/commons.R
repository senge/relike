
# optimzation step size
eps <- 1e-5

# ----------------------------------------
# High fuzzy set membership function
# ----------------------------------------
high <- function(x){
  ifelse(
    x <= 0.5, 
    0,
    2*(x-0.5)
  )
}

# -----------------------------------------
# Calculation of the fuzzy preference relation 
# (Pref (-), Pref (+), Aleatoric Uncert., Epistemic Uncert., Total Uncert.)
# -----------------------------------------
fpr <- function(Rn, Rp) {
  T <- min
  aleaU <- apply(cbind(1-Rn, 1-Rp), 1, T)
  episU <- apply(cbind(Rn, Rp), 1, T)
  striP <- ifelse(Rn == Rp, 
                  (1-(aleaU + episU))/2,
                  ifelse(Rp > Rn, 1-(aleaU + episU), 0))
  striN <- 1-(striP + aleaU + episU) 
  
  return(c(striN, striP, aleaU, episU, aleaU+episU))
}

# ---------------------------------------
# Implementation for logistic regression
# ---------------------------------------
# ----------------------------------------
# Apply linear function
# ----------------------------------------
lin <- function(b, x){
  sum(b*c(1,x))
}

# ----------------------------------------
# Apply logit transformation
# ----------------------------------------
trans <- function(v) {
  exp(v)/(1+exp(v))
}