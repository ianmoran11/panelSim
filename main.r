rm(list = ls())
library(devtools)
load_all("J:/AID/Business Functions/Data and Analytical Services/Innovation Research/Ians Testlab/ralget")
load_all("J:/AID/Business Functions/Data and Analytical Services/Innovation Research/Ians Testlab/raldag")
library(tidyverse)
library(tidygraph)
library(ggraph)
library(patchwork)
library(magrittr)
library(ggforce)
options(width = 200)
library(igraph) 

list.files("functions",full.names = T)  %>% map(source)


dag_binary_no_covariates <- create_dag_binary_no_covariates()
dag_binary_covariates <- create_dag_binary_covariates()


dag_binary_no_covariates_m <- add_manipulations(dag_binary_no_covariates)
dag_binary_covariates_m <- add_manipulations(dag_binary_covariates)

dfs_binary_no_covariates_m <- map2(dag_binary_no_covariates_m, names(dag_binary_no_covariates_m),~ simulate(.x, label = .y, seed = 123))
dfs_binary_covariates_m <- map2(dag_binary_covariates_m, names(dag_binary_covariates_m),~ simulate(.x, label = .y, seed = 123))

treatment_effect_df <- dfs_binary_no_covariates_m %>% get_treatment_effects()
treatment_effect_df <- dfs_binary_covariates_m %>% get_treatment_effects()


dfs_binary_covariates_m$observed %>% group_by(period) %>% 
  summarise(mean(p))


reg_df <- dfs_binary_covariates_m$observed %>% reg_prep() %>% mutate(p_ever = max(p,na.rm =T))

reg_df %>% group_by(Period) %>% 
  summarise(p = mean(p)) %>% 
  mutate(p_diff = p - lag(p,1))
  
w <- 
    c((.0218 + .0196 +  0.0186),(.0218 + .0196),(0.0218)) /
sum(c((.0218 + .0196 +  0.0186),(.0218 + .0196),(0.0218)))

t <- 
c(.1,.16,.25)

tibble(w, t) %>% mutate(c = w*t) %>% pull(c) %>% sum()




coef_df <- 
  lm(
    y ~ 
      p_ever + p2:f2 + p2:f3 + p2:f4 + p2:f5 +
              p3:f3 + p3:f4 + p3:f5 +
                      p4:f4 + p4:f5 +
                              p5:f5 +
      f2 + f3 + f4 + f5 + p2 + p3 + p4 + p5,
      data = reg_df) %>% tidy %>% arrange(term)

coef_df %>% select(term, estimate) %>%  
  mutate(p = str_extract(term,"p[0-9]")) %>% 
  mutate(f = str_extract(term,"f[0-9]")) %>% 
  mutate(k = str_extract(term,"k[0-9]")) %>% 
  spread(p, estimate) %>% print(n = Inf)
  


  
coef_df %>% 
    mutate(cohort = str_extract(term, "p[0-9]+") %>% str_extract("[0-9]+")) %>%
    mutate(year = str_extract(term, "f[0-9]+")%>% str_extract("[0-9]+") %>% as.numeric()) %>%
  ggplot(aes(x = year, y = estimate, group = cohort, color = cohort)) + geom_line() + 
    geom_line(data = treatment_effect_df %>% 
                mutate(year = as.numeric(period), cohort = "actual", estimate = y)) +
    theme_minimal()


options(width = 100)
reg_df %>% glimpse


coef_df <- 
  lm(
    y ~  p + f2 +f3 + f4 + f5 + p3 + p4 + p5,
    data = reg_df) %>% tidy %>% arrange(term)

coef_df %>% select(term, estimate) %>%  
  mutate(p = str_extract(term,"p[0-9]")) %>% 
  mutate(f = str_extract(term,"f[0-9]")) %>% 
  mutate(k = str_extract(term,"k[0-9]")) %>% 
  spread(p, estimate) %>% print(n = Inf)

(.1 + .16 + .25)/3



coef_df %>% 
  mutate(cohort = str_extract(term, "p[0-9]+") %>% str_extract("[0-9]+")) %>%
  mutate(year = str_extract(term, "f[0-9]+")%>% str_extract("[0-9]+") %>% as.numeric()) %>%
  ggplot(aes(x = year, y = estimate, group = cohort, color = cohort)) + geom_line() + 
  geom_line(data = treatment_effect_df %>% 
              mutate(year = as.numeric(period), cohort = "actual", estimate = y)) +
  theme_minimal()



