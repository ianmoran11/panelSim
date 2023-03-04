reg_prep   <- function(df){
  
  observed_df <- 
    df %>% panel_gather() %>% spread(Variable, Value) %>% 
    group_by(sim_id) %>% mutate(fe = mean(fe, na.rm = T)) 
  
  observed_df_01 <- observed_df %>% group_by(sim_id) %>% mutate(psum = sum(p)) 
  
  observed_df_02 <- c(1:5) %>% map(add_f,my_df= observed_df_01) %>% 
    bind_cols() %>% bind_cols(observed_df_01, .)
  
  observed_df_02
  
}
  
add_f <- function(current_year,my_df){
  year_sym <- sym(paste0("f",current_year))
  pnum_sym <- sym(paste0("p",current_year))
  my_df %>% 
    ungroup() %>% 
    mutate(!!year_sym:= as.numeric(Period == current_year)) %>% 
    mutate(!!pnum_sym:= as.numeric(psum == (6 - current_year))) %>% 
    select(!!year_sym,!!pnum_sym)
}
