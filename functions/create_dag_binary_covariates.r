create_dag_binary_covariates <- function(){
  
  # Number of observations to be simulated
  sample_no <- 10000
  
  # Data generating process for variables 
  y <- v("y", .f = d(~ rnorm(n = sample_no, mean = rsum(.x), sd =  .025)))
  p <- v("p", .f = d(~ as.numeric(rbernoulli(n = sample_no, p = inv.logit(-4 + rsum(.x))))))
  k <- v("k", .f = d(~ rnorm(n = sample_no, mean = rsum(.x), sd =  .025)))
  l <- v("l", .f = d(~ rnorm(n = sample_no, mean = rsum(.x), sd =  .025)))
  fe <- v("fe_t1", .f = d(~ rnorm(n = sample_no, mean = rsum(.x), sd =  .025)))
  
  # Causal relationship between variables  
  dag_cs <-
    (k * b(.5) * p) +
    (l * b(.5) * p) + 
    (k * b(1.0) * l) +
    (l * b(0.3) * y) +
    (k * b(0.3) * y) +
    (p * b(0.1) * y) +
    NULL
  
  
  plot(dag_cs)
  
  # Plot causal relationships   
  dag_cs %>% simulate()
  
  # Plot causal relationships   
  dag_cs %>% plot()
  
  # Declare time series structure -------------------------------------------------
  
  # 5 periods 
  t1 <- v("t1", .f = d(~ rnorm(n = sample_n, mean = rsum(.x), sd =  1)))
  t2 <- v("t2", .f = d(~ rnorm(n = sample_n, mean = rsum(.x), sd =  1)))
  t3 <- v("t3", .f = d(~ rnorm(n = sample_n, mean = rsum(.x), sd =  1)))
  t4 <- v("t4", .f = d(~ rnorm(n = sample_n, mean = rsum(.x), sd =  1)))
  t5 <- v("t5", .f = d(~ rnorm(n = sample_n, mean = rsum(.x), sd =  1)))
  
  # Decleare periods as sequential 
  dag_ts <-  (t1 * b(1) * t2) + (t2 * b(1) * t3) + (t3 * b(1) * t4) + (t4 * b(1) * t5) 
  
  # Plot periods  
  dag_ts %>% plot()
  
  # Create panel data genrating process ------------------------------------------
  
  # Create panel network using cartesian product 
  dag_panel <- cartesian_product(dag_cs,dag_ts,node_combine =  ~ c(.x), edge_combine =  ~ c(.x,.y))
  
  plot(dag_panel)
  
  # remove links from y_t to y_t+1
  dag_panel <- 
    dag_panel %>% 
    activate("edges") %>% 
    mutate(drop = str_detect(from_name,"y") & str_detect(to_name, "y")) %>% 
    filter(!drop) %>% 
    activate("nodes")
  
  dag_panel <- 
    (fe * b(20) * dag_panel) %>% 
    activate("edges") %>% 
    mutate(from_name  = map_chr(from, ~ .N() %>% filter(row_number() == .x) %>% pull(name))) %>% 
    mutate(to_name  = map_chr(to, ~ .N() %>% filter(row_number() == .x) %>% pull(name))) %>% 
    mutate(drop = str_detect(from_name,"fe") & !str_detect(to_name, "y|p")) %>% 
    filter(!drop) %>% 
    activate("nodes")
  
  dag_panel  <- 
    dag_panel %>% 
    activate("edges") %>% 
    mutate(.attrs = map_if(.attrs,is.null, ~ list(function(.value){.value * 5})))  %>% 
    mutate(programme_persistence = str_detect(from_name,"p") & str_detect(to_name,"p"))  %>% 
    mutate(.attrs = map_if(.attrs,programme_persistence, ~ list(function(.value){.value * 1000})))  %>% 
    activate("nodes")
  
  p_t1 <- dag_panel %>% filter(str_detect(name, "p_t1")) 
  p_t2 <- dag_panel %>% filter(str_detect(name, "p_t2")) 
  p_t3 <- dag_panel %>% filter(str_detect(name, "p_t3")) 
  p_t4 <- dag_panel %>% filter(str_detect(name, "p_t4")) 
  p_t5 <- dag_panel %>% filter(str_detect(name, "p_t5")) 
  
  
  k_t2 <- dag_panel %>% filter(str_detect(name, "k_t2")) 
  k_t3 <- dag_panel %>% filter(str_detect(name, "k_t3")) 
  k_t4 <- dag_panel %>% filter(str_detect(name, "k_t4")) 
  k_t5 <- dag_panel %>% filter(str_detect(name, "k_t5")) 
  
  dag_panel <- 
    dag_panel + 
    (p_t1 * b(.1) * k_t2) + 
    (p_t2 * b(.1) * k_t3) + 
    (p_t3 * b(.1) * k_t4) + 
    (p_t4 * b(.1) * k_t5) 
  
  dag_panel
}

