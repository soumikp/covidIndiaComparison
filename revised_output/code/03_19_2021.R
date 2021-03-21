##### dependencies #####
require(tidyverse)
require(here)
require(janitor)
require(glue)
require(EpiEstim)
require(gghighlight)
require(simpleboot)
require(gt)
require(gridExtra)
require(grid)
require(lmPerm)
require(ggrepel)
require(coin)

##### cleaning #####
date <- "2020-10-15"
set_seed <- 46342
set.seed(set_seed)

f_col <- "#e01d96" # color for female countries
m_col <- "#00274C" # color for male countries
b_rep <- 100000     # number of bootstrap iterations


dat <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", col_types = cols()) %>%
  dplyr::rename(state = `Province/State`, name = `Country/Region`) %>%
  dplyr::select(-c(Lat, Long, state)) %>%
  group_by(name) %>%
  summarize_all(list(sum)) %>%
  ungroup() %>%
  pivot_longer(names_to = "date", values_to = "cases", -name) %>% 
  filter(name == "India") %>% 
  mutate(date = as.Date(date, "%m/%d/%y"))

dat <- dat %>% filter(date >= "2020-03-03" & date <= "2020-10-15")

dat <- dat %>%
  arrange(date) %>%
  dplyr::rename(tot_cases = cases) %>%
  mutate(
    cases = tot_cases - dplyr::lag(tot_cases)
  ) %>%
  drop_na(cases) %>%
  ungroup() %>%
  filter(tot_cases > 50) %>%
  group_by(name) %>%
  arrange(date) %>%
  mutate(
    day = seq(n())
  ) %>%
  ungroup() %>%
  filter(!(cases <=0 | is.na(cases)))

estR0_out <- function(dat) {
  
  t_start <- seq(2, nrow(dat) - 4)
  t_end   <- t_start + 4
  
  # from EpiEstim
  res <- estimate_R(
    incid = dat$cases,
    method = "parametric_si",
    config = make_config(list(
      mean_si             = 7,
      std_si              = 4.5,
      si_parametric_distr = "G",
      t_start             = t_start,
      t_end               = t_end,
      seed                = set_seed))
  ) 
  
  tibble(
    date_num = res$dates
  ) %>% left_join(
    res$R, by = c("date_num" = "t_end")
  ) %>%
    dplyr::select(
      date_num, t_start, r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`
    ) %>%
    add_column(date = dat$date) %>%
    dplyr::select(-date_num) %>%
    select(date, everything())
}



# time-varying R
# apply time-varying R function to all countries ----------
options(warn = -1)

r0_est <- dat %>%
  dplyr::select(date, cases, name) %>%
  nest(data = c(-name)) %>%
  mutate(
    estR0 = map(data, ~estR0_out(dat = .x))
  ) %>%
  unnest(estR0) %>%
  dplyr::select(-data)

options(warn = 1)


Lockdown 1.0 (March 25 – April 14)
Lockdown 2.0 (April 15 – May 3)
Lockdown 3.0 (May 4 – May 17)
Lockdown 4.0 (May 18 – May 31)
Unlock 1.0 (June 1 - June 30)
Unlock 2.0 (July 1 - July 31)
Unlock 3.0 (August 1 - August 31)
Unlock 4.0 (September 1 - September 30)
Unlock 5.0 (Octorber 1 - Octorber 15)

r0 <- NULL


r0 <- c(r0, r0_est %>%
          drop_na() %>% 
          filter(date <= "2020-03-24") %>% 
          summarise(r = mean(r)))
r0 <- c(r0, r0_est %>%
          drop_na() %>% 
          filter(date >= "2020-03-25" & date <= "2020-04-14") %>% 
          summarise(r = mean(r)))
r0 <- c(r0, r0_est %>%
          drop_na() %>% 
          filter(date >= "2020-04-15" & date <= "2020-05-03") %>% 
          summarise(r = mean(r)))
r0 <- c(r0, r0_est %>%
          drop_na() %>% 
          filter(date >= "2020-05-04" & date <= "2020-05-17") %>% 
          summarise(r = mean(r)))
r0 <- c(r0, r0_est %>%
          drop_na() %>% 
          filter(date >= "2020-05-18" & date <= "2020-05-31") %>% 
          summarise(r = mean(r)))
r0 <- c(r0, r0_est %>%
          drop_na() %>% 
          filter(date >= "2020-06-01" & date <= "2020-06-30") %>% 
          summarise(r = mean(r)))
r0 <- c(r0, r0_est %>%
          drop_na() %>% 
          filter(date >= "2020-07-01" & date <= "2020-07-31") %>% 
          summarise(r = mean(r)))
r0 <- c(r0, r0_est %>%
          drop_na() %>% 
          filter(date >= "2020-08-01" & date <= "2020-08-31") %>% 
          summarise(r = mean(r)))
r0 <- c(r0, r0_est %>%
          drop_na() %>% 
          filter(date >= "2020-09-01" & date <= "2020-09-30") %>% 
          summarise(r = mean(r)))
r0 <- c(r0, r0_est %>%
          drop_na() %>% 
          filter(date >= "2020-10-01") %>% 
          summarise(r = mean(r)))
