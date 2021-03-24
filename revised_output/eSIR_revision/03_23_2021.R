R_0                <- 3.28
pi0                <- round(c(3.3293869, # Inputs from epiestim - block-wise R means
                              2.3361034, 
                              1.3709448, 
                              1.4125903, 
                              1.3295714, 
                              1.2279825, 
                              1.2479330,
                              1.0986448,
                              1.0434275,
                              0.9121052) / R_0, 2)
pi0                <- c(pi0, mean(pi0[(length(pi0) - 4):length(pi0)]))

x <- 2.08
l <- 1.41
u <- 2.12

as_tibble(cbind(x*pi0, l*pi0, u*pi0)) %>% 
  mutate(text = paste0(round(V1,2),
                       "[", 
                       round(V2, 2),
                       ", ", 
                       round(V3, 2), "]"))
