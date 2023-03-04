create_dag_binary_no_covariates <- function(){
  
  sample_no <- 10000
  
  # Data generating process for variables 
  y <- v("y", .f = d(~ rnorm(n = sample_no, mean = rsum(.x), sd =  .025)))
  p <- v("p", .f = d(~ as.numeric(rbernoulli(n = sample_no, p = inv.logit(-2 + rsum(.x))))))
  k <- v("k", .f = d(~ rnorm(n = sample_no, mean = rsum(.x), sd =  .025)))
  l <- v("l", .f = d(~ rnorm(n = sample_no, mean = rsum(.x), sd =  .025)))
  fe <- v("fe_t0", .f = d(~ rnorm(n = sample_no, mean = rsum(.x), sd =  .025)))
  
  # Causal relationship between variables  
  dag_cs <-
    (p * b(0.1) * y) +  
    NULL
  
  # Declare time series structure -------------------------------------------------
  
  # 5 periods 
  t1 <- v("t1", .f = d(~ rnorm(n = sample_n, mean = rsum(.x), sd =  1)))
  t2 <- v("t2", .f = d(~ rnorm(n = sample_n, mean = rsum(.x), sd =  1)))
  t3 <- v("t3", .f = d(~ rnorm(n = sample_n, mean = rsum(.x), sd =  1)))
  t4 <- v("t4", .f = d(~ rnorm(n = sample_n, mean = rsum(.x), sd =  1)))
  t5 <- v("t5", .f = d(~ rnorm(n = sample_n, mean = rsum(.x), sd =  1)))
  
  # Decleare periods as sequential 
  dag_ts <-  (t1 * b(1) * t2) + (t2 * b(1) * t3) + (t3 * b(1) * t4) + (t4 * b(1) * t5)
  
  # Create panel data genrating process ------------------------------------------
  
  # Create panel network using cartesian product 