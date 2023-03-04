add_manipulations <- function(dag_panel){
  
  forced_treatment <- 
    dag_panel %>% manipulate(p_t1 = 0) %>% manipulate(p_t2 = 0) %>% 
    manipulate(p_t3 = 1)
  
  forced_nontreatment <-  
    dag_panel %>% manipulate(p_t1 = 0) %>% manipulate(p_t2 = 0) %>% 
    manipulate(p_t3 = 0) %>% manipulate(p_t4 = 0) %>% 
    manipulate(p_t5 = 0)
  
  observed <- 
    dag_panel %>% manipulate(p_t1 = 0) %>% manipulate(p_t2 = 0)
    
list(
  observed = observed,
  forced_nontreatment = forced_nontreatment, 
  forced_treatment = forced_treatment)  

}