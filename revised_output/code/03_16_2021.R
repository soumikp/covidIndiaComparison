## to collect data and generate tables, plots. 

require(tidyverse)

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


#baseline done
baseline <- read_csv("~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/Baseline_revision/baseline.csv")
baseline <-  baseline %>% select(-contains("observed"))

## eSIR still waiting for Rupam da's input


## FANSY still waiting for the kids to call
seirFansy <- read_csv("~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/SEIRfansy_revision/Projections_India.csv") %>%
  filter(date > "2020-10-15" & date <= "2020-12-31")

seirFansy.deaths <- seirFansy %>% select(contains(c("date", "Deceased")))
seirFansy.cases <- seirFansy %>% select(-contains(c("Deceased")))

##SAPHIRE
saphire <- read_delim("~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/SAPHIRE_revision/SAPHIRE_reported_unreported_cases_of_india.txt",
           delim = "\t") %>%
  rename(Date = "a1$Date") %>%
  filter(Date >= "2020-10-15") %>%
  rename(cumulative_reported_cases) %>%
  select(Date, predicted.total)

saphire <- saphire %>%
  add_column(predicted.active = c(NA, diff(saphire$predicted.total))) %>%
  drop_na()

saphire.ci <- read.table("~/Box/COVID India Comparisons/Revisions/covidIndiaComparison/revised_output/SAPHIRE_revision/SAPHIRE_estimates_and_prediction.txt")
names(saphire.ci) <- saphire.ci[1,]
saphire.ci <- saphire.ci[,!names(saphire.ci) == "time"]
saphire.ci <- as_tibble(saphire.ci[-1,])
saphire.ci %>% filter(Date == "2020-10-16") %>% select(S, E, P, I, A, H, R)

##ICM
icm <- read_