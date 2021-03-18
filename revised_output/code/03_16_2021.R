## to collect data and generate tables, plots. 
require(tidyverse)
require(lubridate)

#observed counts
Observed = read.csv("~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/code/case_time_series.csv")
# Set consistent parameters
Start_Data = "2020-03-15"
End_Data = "2020-10-15"
Start_Pred = "2020-10-19"
End_Pred = "2020-12-31"

Colors = c(
  "Observed" = "black",
  
  "eSIR"    = "red",
  "eSEIR_1"    = "blue",
  "eSEIR_2"    = "green",
  "ICM"    = "yellow"
)



# Observed infection data

Observed$Date = seq.Date(
  from = as.Date("2020-01-30"),
  to = as.Date("2021-03-15"),
  by = "day"
)




Observed_Data = Observed[as.numeric(as.Date(Observed$Date)) >= as.numeric(as.Date(Start_Data)) &
                           as.numeric(as.Date(Observed$Date)) <= as.numeric(as.Date(End_Pred)), c("Date",
                                                                                                  "Total.Confirmed",
                                                                                                  "Total.Recovered",
                                                                                                  "Total.Deceased")]
colnames(Observed_Data)[-1] = c("Total_Confirmed",
                                "Total_Recovered",
                                "Total_Death")

Observed_Data$Active_Confirmed = Observed_Data$Total_Confirmed - 
  Observed_Data$Total_Recovered -
  Observed_Data$Total_Death

Total_Confirmed_Observed = reshape2::melt(Observed_Data[, 1:2], id.vars = "Date")
Total_Confirmed_Observed$Date = as.character(Total_Confirmed_Observed$Date)
Total_Confirmed_Observed$variable = "Observed"

Active_Confirmed_Observed = reshape2::melt(Observed_Data[, c(1, 5)], id.vars = "Date")
Active_Confirmed_Observed$Date = as.character(Active_Confirmed_Observed$Date)
Active_Confirmed_Observed$variable = "Observed"

Death_Confirmed_Observed = reshape2::melt(Observed_Data[, c(1, 4)], id.vars = "Date")
Death_Confirmed_Observed$Date = as.character(Death_Confirmed_Observed$Date)
Death_Confirmed_Observed$variable = "Observed"

observed <- as_tibble(cbind(Death_Confirmed_Observed$Date, 
      Active_Confirmed_Observed$value,
      Total_Confirmed_Observed$value, 
      Death_Confirmed_Observed$value)) %>% 
  rename(Date = V1,
         active.case = V2, 
         total.case = V3, 
         total.death = V4) %>% 
  mutate(Date = lubridate::as_datetime(Date), 
         active.case = as.numeric(active.case), 
         total.case = as.numeric(total.case), 
         total.death = as.numeric(total.death))

observed.test <- observed %>% 
  filter(Date > "2020-10-15")

write_csv(observed, "observed.csv")

# nomenclature: 
## ARC = active, reported, case
## AUC = active, unreported, case
## ATC = active, total, case
## CRC = cumulative, reported, case ...and so on. 
## CRD = cumulative, reported, death


#baseline done
baseline <- read_csv("~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/Baseline_revision/baseline.csv")
baseline <-  baseline %>% select(-contains("observed")) %>% 
  rename(bl.crc.estim = predicted.point, 
         bl.crc.low = predicted.low, 
         bl.crc.high = predicted.high) %>% 
  mutate(bl.crc.width = bl.crc.high - bl.crc.low)

## eSIR done
esir <- read_csv("~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/eSIR_revision/eSIR Results.csv") %>% 
  mutate(date = as_datetime(forecast_dt), 
         esir.arc.estim = India_active, 
         esir.arc.low = India_active_low, 
         esir.arc.high = India_active_up, 
         esir.crc.estim = India_confirm, 
         esir.crc.low = India_confirm_low, 
         esir.crc.high = India_confirm_up, 
         esir.crd.estim = India_death, 
         esir.crd.low = India_death_low, 
         esir.crd.high = India_death_up) %>% 
  select(contains(c("date", "esir."))) %>% 
  mutate(esir.arc.width = esir.arc.high - esir.arc.low, 
         esir.crc.width = esir.crc.high - esir.crc.low, 
         esir.crd.width = esir.crd.high - esir.crd.low)
  

## FANSY done
seirFansy <- read_csv("~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/SEIRfansy_revision/Projections_India.csv") %>%
  filter(date > "2020-10-15" & date <= "2020-12-31") %>% 
  mutate(date = as_datetime(date), 
         seirf.arc.estim = Current_Reported_Infected_Mean, 
         seirf.arc.low = Current_Reported_Infected_Lower_CI, 
         seirf.arc.high = Current_Reported_Infected_Upper_CI, 
         seirf.auc.estim = Current_Unreported_Infected_Mean, 
         seirf.auc.low = Current_Unreported_Infected_Lower_CI, 
         seirf.auc.high = Current_Unreported_Infected_Upper_CI, 
         seirf.atc.estim = Total_Current_Cases_Mean, 
         seirf.atc.low = Total_Current_Cases_Lower_CI, 
         seirf.atc.high = Total_Current_Cases_Upper_CI, 
         
         seirf.crc.estim = Cum_Reported_Cases_Mean, 
         seirf.crc.low = Cum_Reported_Cases_Lower_CI, 
         seirf.crc.high = Cum_Reported_Cases_Upper_CI, 
         seirf.cuc.estim = Cum_Unreported_Cases_Mean, 
         seirf.cuc.low = Cum_Unreported_Cases_Lower_CI, 
         seirf.cuc.high = Cum_Unreported_Cases_Upper_CI, 
         seirf.ctc.estim = Total_Cum_Cases_Mean, 
         seirf.ctc.low = Total_Cum_Cases_Lower_CI, 
         seirf.ctc.high = Total_Cum_Cases_Upper_CI, 
         
         seirf.crd.estim = Total_Reported_Deceased_Mean, 
         seirf.crd.low = Total_Reported_Deceased_Lower_CI, 
         seirf.crd.high = Total_Reported_Deceased_Upper_CI, 
         seirf.cud.estim = Total_Unreported_Deceased_Mean, 
         seirf.cud.low = Total_Unreported_Deceased_Lower_CI, 
         seirf.cud.high = Total_Unreported_Deceased_Upper_CI, 
         seirf.ctd.estim = Total_Deceased_Mean, 
         seirf.ctd.low = Total_Deceased_Lower_CI, 
         seirf.ctd.high = Total_Deceased_Upper_CI) %>% 
  select(contains(c("date", "seirf"))) %>% 
  mutate(seirf.arc.width = seirf.arc.high - seirf.arc.low,
         seirf.auc.width = seirf.auc.high - seirf.auc.low, 
         seirf.atc.width = seirf.atc.high - seirf.atc.low, 
         
         seirf.crc.width = seirf.crc.high - seirf.crc.low,
         seirf.cuc.width = seirf.cuc.high - seirf.cuc.low, 
         seirf.ctc.width = seirf.ctc.high - seirf.ctc.low, 
         
         seirf.crd.width = seirf.crd.high - seirf.crd.low,
         seirf.cud.width = seirf.cud.high - seirf.cud.low, 
         seirf.ctd.width = seirf.ctd.high - seirf.ctd.low)

##SAPHIRE done
saphire <- read_delim("~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/SAPHIRE_revision/SAPHIRE_estimates_and_prediction.txt", 
                      delim = "\t") %>% 
  mutate(date = as_datetime(Date)) %>% 
  select(-Date) %>% 
  mutate(sap.crc.estim = cumulative_reported, 
         sap.crc.low = cumulative_reported_lower, 
         sap.crc.high = cumulative_reported_upper,
         sap.cuc.estim = cumulative_unreported, 
         sap.cuc.low = cumulative_unreported_lower, 
         sap.cuc.high = cumulative_unreported_upper,
         sap.ctc.estim = cumulative_total, 
         sap.ctc.low = cumulative_total_lower, 
         sap.ctc.high = cumulative_total_upper) %>% 
  select(contains(c("date", "sap"))) %>% 
  filter(date >= "2020-10-15" & date <= "2020-12-31")

##ICM still waiting to hear back from Dr. Mishra on CI for cumulative cases.
icm <- read_csv("~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/ICM_revision/india_results_ifr_0_04_totals.csv") %>% 
  mutate(date = as_datetime(as.Date(date, format = "%d/%m/%y"))) %>% 
  rename(icm.ctc.estim = "Total Infections Mean", 
         icm.ctc.low = "Total LowerCI Infections", 
         icm.ctc.high = "Total UpperCI Infections", 
         
         icm.ctd.estim = "Total ExpDeaths Mean", 
         icm.ctd.low = "Total LowerCI ExpDeaths", 
         icm.ctd.high = "Total UpperCI ExpDeaths") %>%
  select(contains(c("date", "icm."))) %>% 
  mutate(icm.ctc.width = icm.ctc.high - icm.ctc.low, 
         icm.ctd.width = icm.ctd.high - icm.ctd.low) %>% 
  filter(date >= "2020-10-15" & date <= "2020-12-31")


##write files and gtfo
write_csv(baseline, 
          "~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/clean_revision/baseline.csv")
write_csv(esir,
          "~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/clean_revision/esir.csv")
write_csv(saphire,
          "~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/clean_revision/saphire.csv")
write_csv(seirFansy,
          "~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/clean_revision/seirfansy.csv")
write_csv(icm,
          "~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/clean_revision/icm.csv")
