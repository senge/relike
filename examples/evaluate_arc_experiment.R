# rm(list=ls())

library(foreach)
library(tidyverse)


# Load ARC result data
arc_result_files = list.files(path="data/results/", pattern="arc_result_.+.Robj")

arc_all = foreach(f = arc_result_files, .combine="rbind") %do% {
  load(paste0("data/results/", f))
  return(arc)
}

data_files = sort(unique(arc_all$data_file))
uq_funs = sort(unique(arc_all$uq_fun))
base_model_names = sort(unique(arc_all$model_name))

pdf(file = "visuals/arc_result_comparison.pdf", paper = "a4", bg = "white")

for(base_model_name in base_model_names) {
  
  # base_model_name = base_model_names[1]
  
  for(data_file_name in data_files) {

    # data_file_name = data_files[1]
    
    p1 = arc_all %>% 
      filter(data_file == data_file_name & model_name == base_model_name) %>%
      select(rej_perc, arc_ut, uq_fun) %>%
      
      ggplot(aes(x=rej_perc, y=arc_ut, color=uq_fun)) +
      geom_line() + 
      theme_minimal() + 
      labs(title=paste0("ARC - Total uncertainty (", base_model_name,")"),
           subtitle = data_file_name,
           x = "Rejection %", y = "Accuracy %")
    
    p2 = arc_all %>% 
      filter(data_file == data_file_name & model_name == base_model_name) %>%
      select(rej_perc, arc_ue, uq_fun) %>%
      
      ggplot(aes(x=rej_perc, y=arc_ue, color=uq_fun)) +
      geom_line() + 
      theme_minimal() + 
      labs(title=paste0("ARC - Epistemic uncertainty (", base_model_name,")"),
           subtitle = data_file_name,
           x = "Rejection %", y = "Accuracy %")
    
    p3 = arc_all %>% 
      filter(data_file == data_file_name & model_name == base_model_name) %>%
      select(rej_perc, arc_ua, uq_fun) %>%
      
      ggplot(aes(x=rej_perc, y=arc_ua, color=uq_fun)) +
      geom_line() + 
      theme_minimal() + 
      labs(title=paste0("ARC - Aleatoric uncertainty (", base_model_name,")"),
           subtitle = data_file_name,
           x = "Rejection %", y = "Accuracy %")
    
    gridExtra::grid.arrange(p1, p2, p3, nrow = 3)
    
  }
  
}

dev.off()






