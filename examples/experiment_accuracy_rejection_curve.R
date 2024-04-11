# rm(list=ls())

#.libPaths(c("/usr/local/lib/R/site-library", "/usr/lib/R/site-library", "/usr/lib/R/library", .libPaths()))


# Setup
library(tidyverse)
library(foreach)
library(doParallel)
source("R/relike.uq.R")
source("R/info.uq.R")
source("R/relike.model.funs.R")
source("R/2014-reliable-classification/third_approach.R")

experiment_arc <- function(
    
  train_fun,
  predict_fun,
  base_model_name,
  uq_fun,
  uq_fun_name,
  data_file,
  train_split_proportion = 0.7,
  num_repititions = 1000
  
) {
  
  #uq_fun = partial(relike.uq, bag_size = 200)
  #uq_fun_name = "relike.uq"
  #train_fun = relike.train.fun.glm
  #predict_fun = relike.predict.fun.glm
  #base_model_name = "glm"
  #data_file = "spect.csv"
  
  # Dataset
  df <- read_csv(paste0("data/", data_file), show_col_types = FALSE) 
  df[[ncol(df)]] <- factor(df[[ncol(df)]])
  df <- na.omit(df) 
  n = nrow(df)
  p = ncol(df)
  labs = levels(df %>% pull(p))
  
  # Check for raw data
  file_raw = paste0("data/results/arc_raw_", base_model_name, "_", data_file, "_", uq_fun_name, ".Robj")
  if(!file.exists(file_raw)) {
    
    message("Running the experiment...")
    
    # The experiment
    cl = makeCluster(8)
    registerDoParallel(cl)
    
    res = foreach(i = 1:num_repititions, .combine = "rbind", .packages = c("tidyverse", "foreach", "iterators", "caret")) %dopar% {
      
      # i = 1
      
      set.seed(23+i)
      
      # Train / test split
      sample_idx = runif(n)
      df_train = df[which(sample_idx < train_split_proportion),]
      df_train_x = as.matrix(df_train[,1:(p-1)])
      df_train_y = df_train %>% pull(p)
      df_test = df[which(sample_idx >= train_split_proportion),]
      df_test_x = df_test[,1:(p-1)]
      df_test_y = df_test %>% pull(p)
      target_col = colnames(df_train)[ncol(df_train)]
      
      # Calculate uncertainties
      uq = uq_fun(x = df_train_x, y = df_train_y, x0 = df_test_x, train_fun = train_fun, predict_fun = predict_fun)
      
      # Merge test truth
      message(paste0("done ", i))
      tmp = data.frame(
        i=i,
        uq,
        y=df_test_y, 
        yhat=as.factor(ifelse(uq[,which(colnames(uq) == paste0("p", labs[1]))] > uq[,which(colnames(uq) == paste0("p", labs[2]))], labs[1], labs[2]))
      )
      
      return(tmp)
    }
    
    stopCluster(cl)
    
    save(res, file = file_raw)
  } else {
    message("Raw data already there.")
  }
  
  
  # Check for ARC data
  file_arc = paste0("data/results/arc_result_", base_model_name, "_", data_file, "_", uq_fun_name, ".Robj")
  if(!file.exists(file_arc)) {
    
    message("Calculating ARC data...")
    
    load(file_raw)
    
    set.seed(42)
    
    # Average accuracy on repetitions for relative likelihood
    tmp = res %>% 
      mutate(err = ifelse(y == yhat, 0, 1)) %>%
      mutate(acc = 1-err) %>%
      
      group_by(i) %>%
      
      arrange(desc(ue), runif(length(ue))) %>% # epistemic uncertainty + random tie break
      mutate(rej_perc_ue = ceiling(row_number() / length(ue) * 100)) %>%
      arrange(desc(ua), runif(length(ua))) %>% # aleatoric uncertainty + random tie break
      mutate(rej_perc_ua = ceiling(row_number() / length(ua) * 100)) %>%
      arrange(desc(ut), runif(length(ut))) %>% # total uncertainty + random tie break
      mutate(rej_perc_ut = ceiling(row_number() / length(ut) * 100)) %>%
      
      arrange(desc(runif(length(acc)))) %>% # random order baseline
      mutate(rej_perc_bl = ceiling(row_number() / length(acc) * 100)) %>%
      ungroup()
    
    arc = as.data.frame(foreach(p = 0:99, .combine = "rbind") %do% {
      
      c(
        rej_perc = p,
        
        arc_ue = mean(tmp %>% filter(rej_perc_ue > p) %>% pull(acc)),
        arc_ua = mean(tmp %>% filter(rej_perc_ua > p) %>% pull(acc)),
        arc_ut = mean(tmp %>% filter(rej_perc_ut > p) %>% pull(acc)),
        
        arc_rd = mean(tmp %>% filter(rej_perc_bl > p) %>% pull(acc)),
        
        data_file = data_file, 
        uq_fun = uq_fun_name,
        model_name = base_model_name)
    })
    
    arc$rej_perc <- as.double(arc$rej_perc)
    arc$arc_ue <- as.double(arc$arc_ue)
    arc$arc_ua <- as.double(arc$arc_ua)
    arc$arc_ut <- as.double(arc$arc_ut)
    arc$arc_rd <- as.double(arc$arc_rd)
    
    save(arc, file = paste0("data/results/arc_result_", base_model_name, "_", data_file, "_", uq_fun_name, ".Robj"))
    
  } else {
    message("ARC data already there.")
  }
  
  # Check for PDF
  file_pdf = paste0("visuals/arc_result_", base_model_name, "_", data_file, "_", uq_fun_name, ".pdf")
  if(!file.exists(file_pdf)) {
    
    message("Creating ARC PDF...")
    load(file_arc)
    
    # Storing ARC visuals
    pdf(file = file_pdf, paper = "a4", bg = "white")
    
    p1 = as.data.frame(arc) %>% 
      select(rej_perc, arc_ut, arc_rd) %>%
      gather(arc_ut, arc_rd, key="measure", value="value") %>%
      
      ggplot(aes(x=rej_perc, y=value, color=measure)) +
      geom_line() + 
      theme_minimal() + 
      labs(title=paste0("ARC - Total uncertainty (", uq_fun_name, "-", base_model_name,")"),
           subtitle = data_file,
           x = "Rejection %", y = "Accuracy %")
    
    p2 = as.data.frame(arc) %>% 
      select(rej_perc, arc_ue, arc_rd) %>%
      gather(arc_ue, arc_rd, key="measure", value="value") %>%
      
      ggplot(aes(x=rej_perc, y=value, color=measure)) +
      geom_line() + 
      theme_minimal() + 
      labs(title=paste0("ARC - Epistemic uncertainty (", uq_fun_name, "-", base_model_name,")"),
           subtitle = data_file,
           x = "Rejection %", y = "Accuracy %")
    
    p3 = as.data.frame(arc) %>% 
      select(rej_perc, arc_ua, arc_rd) %>%
      gather(arc_ua, arc_rd, key="measure", value="value") %>%
      
      ggplot(aes(x=rej_perc, y=value, color=measure)) +
      geom_line() + 
      theme_minimal() + 
      labs(title=paste0("ARC - Aleatoric uncertainty (", uq_fun_name, "-", base_model_name,")"),
           subtitle = data_file,
           x = "Rejection %", y = "Accuracy %")
    
    gridExtra::grid.arrange(p1, p2, p3, nrow = 3)
    
    dev.off()
    
  } else {
    message("ARC PDF already there.")
  }
  
}

