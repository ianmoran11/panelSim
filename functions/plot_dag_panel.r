plot_dag_panel <- function(dag_panel){
  
  coords <- layout_(dag_panel, as_star()) 
  
  coord_df <-  
    dag_panel %>% 
    mutate(x_coord = str_extract(name, "[0-9+]")) %>% 
    mutate(y_coord = map_int(name,function(name,df){ which(sort(unique(pull(df,name) %>% str_remove_all("[0-9]+"))) == name %>% str_remove_all("[0-9]+"))}, df  = as_tibble(.))) %>% 
    select(x_coord, y_coord) %>% 
    as_tibble() %>% 
    mutate_all(as.numeric) %>% 
    as.matrix() 
  
  plot(dag_panel,layout = coord_df) 
  
  
}