require(tidyverse)
require(lubridate)

obs <- read_csv("revised_output/clean_revision/observed.csv") %>% 
  filter(Date >= "2020-10-15")
bl <- read_csv("revised_output/clean_revision/baseline.csv") %>% 
  filter(date >= "2020-10-15")
es <- read_csv("revised_output/clean_revision/esir.csv") %>% 
  filter(date >= "2020-10-15")
sp <- read_csv("revised_output/clean_revision/saphire.csv") %>% 
  filter(date >= "2020-10-15")
sf <- read_csv("revised_output/clean_revision/seirfansy.csv") %>% 
  filter(date >= "2020-10-15")
icm <- read_csv("revised_output/clean_revision/icm.csv") %>% 
  filter(date >= "2020-10-15")

metrics<- function(data, ref){
  round(c(50*DescTools::SMAPE(x = data, ref = ref), 
    sqrt(mean((1 - data/ref)^2))), 3)
}

# ## table 1 - epiestim based R(T) for eSIR
# es %>% 
#   select(date, esir.crc.estim) %>% 
#   rename(cases = esir.crc.estim)


### table 2
metrics(bl$bl.crc.estim, obs$total.case) #only total for BL

metrics(es$esir.arc.estim, obs$active.case) #ll three for esir
metrics(es$esir.crc.estim, obs$total.case)
metrics(es$esir.crd.estim, obs$total.death)

metrics(sp$sap.crc.estim, obs$total.case) #only total for SAPHIRE

metrics(sf$seirf.arc.estim, obs$active.case) #ll three for SEIRF
metrics(sf$seirf.crc.estim, obs$total.case)
metrics(sf$seirf.crd.estim, obs$total.death)

metrics(icm$icm.ctc.estim, obs$total.case)
metrics(icm$icm.ctd.estim, obs$total.death)##nothing for ICM


## table 3 
metrics2 <- function(obs, ref, data){
  round(unlist(c(sqrt(mean(((ref - obs)/(data - obs))^2)), 
          cor(obs, data), 
          epiR::epi.ccc(obs, data)$rho.c[1])), 3)
  
}

metrics2(obs$total.case, bl$bl.crc.estim, bl$bl.crc.estim)
metrics2(obs$total.case, bl$bl.crc.estim, es$esir.crc.estim)
metrics2(obs$total.case, bl$bl.crc.estim, sp$sap.crc.estim)
metrics2(obs$total.case, bl$bl.crc.estim, sf$seirf.crc.estim)

metrics2(obs$total.death, es$esir.crd.estim, es$esir.crd.estim)
metrics2(obs$total.death, es$esir.crd.estim, sf$seirf.crd.estim)




### table 4
dates <- as_datetime(c("2020-10-31", "2020-11-30", "2020-12-31"))
obs %>% 
  select(Date, active.case) %>% 
  filter(Date %in%  dates) %>% 
  rename(obs = active.case) %>% 
  add_column(sf %>% filter(date %in% dates) %>% select(contains("atc"))) %>%
  mutate(text = paste0(round(seirf.atc.estim/10^6, 2), 
                       " (", round(seirf.atc.low/10^6, 2), "-", 
                       round(seirf.atc.high/10^6, 2), ") [",
                       round(seirf.atc.estim/obs, 2), "]"))


