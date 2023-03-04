get_treatment_effects <- function(df_list){
  
  df_treatment <- df_list$forced_treatment
  df_nontreatment <- df_list$forced_nontreatment
  
  bind_rows(df_nontreatment, df_treatment) %>% 
    gather(var,value, -sim_id, -label) %>% 
    spread(label,value) %>%  
    mutate(treatment_effect = forced_treatment - forced_nontreatment) %>% 
    group_by(var) %>% 
    summarise(treatment_effect = mean(treatment_effect)) %>% 
    separate(var, into = c("var","period"), sep = "_") %>% 
    mutate(period = str_extract(period, "[0-9]+")) %>% 
    spread(var, treatment_effect)
  
  
  
  
}  