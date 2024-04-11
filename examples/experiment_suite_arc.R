# rm(list=ls())

library(foreach)
source("examples/experiment_accuracy_rejection_curve.R")

log_msg <- function(...) {
  message(paste(now(), ...))
}

# UQ methods and their parameter and ranges
train_split_proportion = 0.7
num_repititions = 1000

uq_funs = c(
  partial(relike.uq, bag_size = 200, incremental=F),
  partial(relike.uq, bag_size = 200, incremental=T, min_sample_frac=0.1),
  partial(info.uq, bag_size = 200) #,
  # relike.optim.uq.log
)

uq_fun_names = c(
  "relike.uq",
  "relike.uq.inc",
  "info.uq" #,
  #"relike.uq.optim"
)

# Base models
base_train_funs = c(
  relike.train.fun.glm,
  #relike.train.fun.nb,
  partial(relike.train.fun.knn, k=5),
  partial(relike.train.fun.knn, k=7),
  partial(relike.train.fun.knn, k=11),
  #relike.train.fun.gbm,
  relike.train.fun.part
) 
base_predict_funs = c(
  relike.predict.fun.glm,
  #relike.predict.fun.nb,
  relike.predict.fun.knn,
  relike.predict.fun.knn,
  relike.predict.fun.knn,
  #relike.predict.fun.gbm,
  relike.predict.fun.part
)
base_model_names = c(
  "glm",
  #"nb",
  "5nn",
  "7nn",
  "11nn",
  #"gbm" ,
  "part"
)

# Datasets
data_files = c(
  "biomed.csv",
  #"breast_cancer.csv",
  "cancer.csv",
  "heart.csv",
  "spect.csv",
  "blood.csv",
  "bupa.csv",
  "diabetes.csv",
  "thorax_comb.csv"
)


# Running the experiments sequentially

for( u in 1:length(uq_funs)) {
  # u = 1
  
  for( b in 1:length(base_train_funs)) {
    # b = 1
    
    for( d in 1:length(data_files)) {
      # d = 1
      
      log_msg("Starting ", uq_fun_names[u], ", ", base_model_names[b], ", ", data_files[d])
      
      tryCatch(
        
        experiment_arc(
          train_fun = base_train_funs[[b]],
          predict_fun = base_predict_funs[[b]],
          base_model_name = base_model_names[b],
          
          uq_fun = uq_funs[[u]],
          uq_fun_name = uq_fun_names[u],
          
          data_file = data_files[d],
          
          train_split_proportion = train_split_proportion,
          num_repititions = num_repititions
        
        ), error=function(e) {log_msg("Skipped due to error:", e)}) 
      
      log_msg(u, ".", b, ".", d, " done.")
      
    }
  }
}

log_msg("Finished experiments.")
