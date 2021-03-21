require(tidyverse)
require(lubridate)

obs <- read_csv("revised_output/clean_revision/observed.csv") %>% 
  filter(Date >= "2020-10-15")
bl <- read_csv("revised_output/clean_revision/baseline.csv") %>% 
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


### table 2
metrics(bl$bl.crc.estim, obs$total.case) #only total for BL

metrics(sp$sap.crc.estim, obs$total.case) #only total for SAPHIRE

metrics(sf$seirf.arc.estim, obs$active.case) #ll three for SEIRF
metrics(sf$seirf.crc.estim, obs$total.case)
metrics(sf$seirf.crd.estim, obs$total.death)

##nothing for ICM
