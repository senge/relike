# rm(list=ls())

#.libPaths(c("/usr/local/lib/R/site-library", "/usr/lib/R/site-library", "/usr/lib/R/library", .libPaths()))


# Setup
library(tidyverse)
library(foreach)
library(doParallel)
source("R/relike.uq.R")
source("R/relike.model.funs.R")
source("R/2014-reliable-classification/third_approach.R")

# Parameters
train_split_proportion = 0.7
num_repititions = 100 #100
bag_size = 200
file_name = "diabetes.csv"  #"thorax_comb.csv" #"spect.csv"

# Dataset
df <- read_csv(paste0("data/", file_name)) 
df[[ncol(df)]] <- factor(df[[ncol(df)]])
df <- na.omit(df) 
n = nrow(df)
p = ncol(df)
labs = levels(df %>% pull(p))

# The experiment
cl = makeCluster(8)
registerDoParallel(cl)

set.seed(23)

res = foreach(i = 1:num_repititions, .combine = "rbind", .packages = c("tidyverse", "foreach", "iterators", "caret")) %dopar% {
  
  foreach(k = c(seq(2, 10, by=2), seq(15, 50, by=5))+1, .combine = "rbind") %do% {
  
    # k = 2
    
    train_fun = partial(relike.train.fun.knn, k=k)
    predict_fun = relike.predict.fun.knn
    
    # i = 1
    
    # Train / test split
    sample_idx = runif(n)
    df_train = df[which(sample_idx < train_split_proportion),]
    df_train_x = as.matrix(df_train[,1:(p-1)])
    df_train_y = df_train %>% pull(p)
    df_test = df[which(sample_idx >= train_split_proportion),]
    df_test_x = df_test[,1:(p-1)]
    df_test_y = df_test %>% pull(p)
    
    # Calculate uncertainties
    uq = relike.uq(x = df_train_x, y = df_train_y, x0 = df_test_x, train_fun = train_fun, predict_fun = predict_fun, bag_size = bag_size, calc_info_measures = T)
    
    #target_col = colnames(df_train)[ncol(df_train)]
    #uq_optim = relike.optim.uq.log(as.formula(paste0(target_col, "~.")), df_train, df_test_x)
    
    # Merge test truth
    message(paste0("done ", i))
    tmp = data.frame(
      k=k,
      i=i,
      uq,
      #uq_optim,
      y=df_test_y, 
      yhat=as.factor(ifelse(uq[,which(colnames(uq) == paste0("p", labs[1]))] > uq[,which(colnames(uq) == paste0("p", labs[2]))], labs[1], labs[2]))#,
      #yhat_optim=as.factor(ifelse(uq[,which(colnames(uq) == paste0("p", labs[1]))] > uq[,which(colnames(uq) == paste0("p", labs[2]))], labs[1], labs[2]))
    )
    #return(tmp)
  }
}

stopCluster(cl)

save(res, file = paste0("data/results/experiment_growing_capacity_with_knn.", file_name, ".result.Robj"))


# Average accuracy on repetitions for relative likelihood
tmp = res %>% 
  mutate(err = ifelse(y == yhat, 0, 1)) %>%
  group_by(i, k) %>%
  mutate(acc = 1-err) %>%
  
  arrange(desc(ue), runif(length(ue))) %>% # epistemic uncertainty + random tie break
  mutate(rej_perc_ue = ceiling(row_number() / length(ue) * 100)) %>%
  arrange(desc(ua), runif(length(ua))) %>% # aleatoric uncertainty + random tie break
  mutate(rej_perc_ua = ceiling(row_number() / length(ua) * 100)) %>%
  arrange(desc(ut), runif(length(ut))) %>% # total uncertainty + random tie break
  mutate(rej_perc_ut = ceiling(row_number() / length(ut) * 100)) %>%
  
  arrange(desc(ue_info), runif(length(ue_info))) %>% # epistemic uncertainty + random tie break
  mutate(rej_perc_ue_info = ceiling(row_number() / length(ue_info) * 100)) %>%
  arrange(desc(ua_info), runif(length(ua_info))) %>% # aleatoric uncertainty + random tie break
  mutate(rej_perc_ua_info = ceiling(row_number() / length(ua_info) * 100)) %>%
  arrange(desc(ut_info), runif(length(ut_info))) %>% # total uncertainty + random tie break
  mutate(rej_perc_ut_info = ceiling(row_number() / length(ut_info) * 100)) %>%
  
  #arrange(desc(ue_optim), runif(length(ue_optim))) %>% # epistemic uncertainty + random tie break
  #mutate(rej_perc_ue_optim = ceiling(row_number() / length(ue_optim) * 100)) %>%
  #arrange(desc(ua_optim), runif(length(ua_optim))) %>% # aleatoric uncertainty + random tie break
  #mutate(rej_perc_ua_optim = ceiling(row_number() / length(ua_optim) * 100)) %>%
  #arrange(desc(ut_optim), runif(length(ut_optim))) %>% # total uncertainty + random tie break
  #mutate(rej_perc_ut_optim = ceiling(row_number() / length(ut_optim) * 100)) %>%
  
  arrange(desc(runif(length(acc)))) %>% # random order baseline
  mutate(rej_perc_bl = ceiling(row_number() / length(acc) * 100)) %>%
  ungroup() 

arc = foreach(p = 0:99, .combine = "rbind") %do% {
  foreach(k=unique(tmp$k), .combine = "rbind") %do% {
    c(
      rej_perc = p,
      k = k,
      
      arc_ue_ensemble = mean(tmp %>% filter(rej_perc_ue > p) %>% pull(acc)),
      arc_ua_ensemble = mean(tmp %>% filter(rej_perc_ua > p) %>% pull(acc)),
      arc_ut_ensemble = mean(tmp %>% filter(rej_perc_ut > p) %>% pull(acc)),
      
      arc_ue_info = mean(tmp %>% filter(rej_perc_ue_info > p) %>% pull(acc)),
      arc_ua_info = mean(tmp %>% filter(rej_perc_ua_info > p) %>% pull(acc)),
      arc_ut_info = mean(tmp %>% filter(rej_perc_ut_info > p) %>% pull(acc)),
      
      #arc_ue_optim = mean(tmp %>% filter(rej_perc_ue_optim > p) %>% pull(acc)),
      #arc_ua_optim = mean(tmp %>% filter(rej_perc_ua_optim > p) %>% pull(acc)),
      #arc_ut_optim = mean(tmp %>% filter(rej_perc_ut_optim > p) %>% pull(acc)),
      
      arc_rd = mean(tmp %>% filter(rej_perc_bl > p) %>% pull(acc))
    )
  }
}

save(arc, file = paste0("data/results/experiment_growing_capacity_with_knn_odd.", file_name, ".arc.Robj"))




#load(file = paste0("data/results/experiment_growing_capacity_with_knn_odd.", file_name, ".result.Robj"))

#res %>%
#  ggplot(aes(x=factor(k), y=ue)) + 
#  geom_boxplot()








