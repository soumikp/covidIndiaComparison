## to collect data and generate tables, plots. 

require(tidyverse)


#observed and baseline done
baseline <- read_csv("~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/Baseline_revision/baseline.csv")
observed <- baseline %>% select(contains(c("Date", "observed")))
baseline <-  baseline %>% select(-contains("observed"))

## eSIR still waiting for Rupam da's input


## FANSY done
seirFansy <- read_csv("~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/SEIRfansy_revision/Projections_India.csv") %>%
  filter(date > "2020-10-15" & date <= "2020-12-31")

seirFansy.deaths <- seirFansy %>% select(contains(c("date", "Deceased")))
seirFansy.cases <- seirFansy %>% select(-contains(c("Deceased")))

##SAPHIRE still waiting for feedback from Xuelin
# saphire <- read_delim("~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/SAPHIRE_revision/SAPHIRE_reported_unreported_cases_of_india.txt",
#            delim = "\t") %>%
#   rename(Date = "a1$Date") %>%
#   filter(Date >= "2020-10-15") %>%
#   rename(cumulative_reported_cases) %>%
#   select(Date, predicted.total)
# 
# saphire <- saphire %>%
#   add_column(predicted.active = c(NA, diff(saphire$predicted.total))) %>%
#   drop_na()
# 
# saphire.ci <- read.table("~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/SAPHIRE_revision/SAPHIRE_estimates_and_prediction.txt")
# names(saphire.ci) <- saphire.ci[1,]
# saphire.ci <- saphire.ci[,!names(saphire.ci) == "time"]
# saphire.ci <- as_tibble(saphire.ci[-1,])
# saphire.ci %>% filter(Date == "2020-10-16") %>% select(S, E, P, I, A, H, R)

##ICM
icm <- read_