library(tidyverse)

start.date = "2020-03-15"
end.date = "2020-10-15"

start.pred = "2020-10-16"
end.pred = "2020-12-31"

model_dt = seq.Date(from = as.Date(start.date),
                    to = as.Date(end.date),
                    by = "day")
forecast_dt = seq.Date(from = as.Date(start.pred),
                       to = as.Date(end.pred),
                       by = "day")

train.length = length(model_dt)
forecast.length = length(forecast_dt)

casename = "nat_revision_no_int"

adj = T
adj_len = 2

N = 1.38e9
death_in_R = 0.0184

Observed = read.csv(file = url(
  "https://api.covid19india.org/csv/latest/case_time_series.csv"
))

Observed_Data = Observed[as.numeric(as.Date(Observed$Date_YMD)) >= as.numeric(as.Date(start.date)) &
                           as.numeric(as.Date(Observed$Date_YMD)) <= as.numeric(as.Date(end.date)), c("Date_YMD",
                                                                                                      "Total.Confirmed",
                                                                                                      "Total.Recovered",
                                                                                                      "Total.Deceased")]
colnames(Observed_Data) = c("Date", "Total_Confirmed", "Total_Recovered", "Total_Death")

Observed_Data$Active_Confirmed = Observed_Data$Total_Confirmed - Observed_Data$Total_Recovered -
  Observed_Data$Total_Death

load(paste0("revised_output/eSIR_revision/eSIR Outputs/",
            casename,
            "_plot_data.Rdata"))

other_plot = plot_data_ls[[2]]

T_prime = other_plot[[1]]
T_fin = other_plot[[2]]
chron_ls = other_plot[[3]]
dthetaI_tp1 = other_plot[[4]]
dthetaI_tp2 = other_plot[[5]]
dthetaI_tp1_date = other_plot[[6]]
dthetaI_tp2_date = other_plot[[7]]
beta_p_mean = other_plot[[8]]
gamma_p_mean = other_plot[[9]]
R0_p_mean = other_plot[[10]]

spaghetti_plot_ls = plot_data_ls[[3]]
spaghetti_ht = spaghetti_plot_ls[[1]]
dthetaI_mean_data = spaghetti_plot_ls[[2]]
sample_dthetaI_mat_long = spaghetti_plot_ls[[3]]
first_tp_date_ci = spaghetti_plot_ls[[4]]
second_tp_date_ci = spaghetti_plot_ls[[5]]

infection_plot_ls = plot_data_ls[[4]]
y_text_ht = infection_plot_ls[[1]]
data_poly = infection_plot_ls[[2]]
data_comp = infection_plot_ls[[3]]
data_pre = infection_plot_ls[[4]]

removed_plot_ls = plot_data_ls[[5]]
r_text_ht = removed_plot_ls[[1]]
data_poly_R = removed_plot_ls[[2]]
data_comp_R = removed_plot_ls[[3]]
data_pre_R = removed_plot_ls[[4]]

India_confirm =
  round(N * (data_comp[(train.length + 1):(train.length + forecast.length), "mean"] +
               data_comp_R[(train.length + 1):(train.length + forecast.length), "mean"]))

India_confirm_up =
  round(N * (data_comp[(train.length + 1):(train.length + forecast.length), "upper"] +
               data_comp_R[(train.length + 1):(train.length + forecast.length), "upper"]))

India_confirm_low =
  round(N * (data_comp[(train.length + 1):(train.length + forecast.length), "lower"] +
               data_comp_R[(train.length + 1):(train.length + forecast.length), "lower"]))

dataf = Observed_Data$Total_Confirmed

if (adj == T) {
  adj_v =
    mean(as.vector(dataf[(T_prime - adj_len):T_prime]) / N / (data_comp[(T_prime -
                                                                           adj_len):T_prime, "mean"] + data_comp_R[(T_prime - adj_len):T_prime, "mean"]))
  India_confirm = round(India_confirm * adj_v)
  India_confirm_up = round(India_confirm_up * adj_v)
  India_confirm_low = round(India_confirm_low * adj_v)
}

India_active =
  round(N * (data_comp[(train.length + 1):(train.length + forecast.length), "mean"]))

India_active_up =
  round(N * (data_comp[(train.length + 1):(train.length + forecast.length), "upper"]))

India_active_low =
  round(N * (data_comp[(train.length + 1):(train.length + forecast.length), "lower"]))

dataf = Observed_Data$Active_Confirmed

if (adj == T) {
  adj_v =
    mean(as.vector(dataf[(T_prime - adj_len):T_prime]) / N / (data_comp[(T_prime -
                                                                           adj_len):T_prime, "mean"]))
  India_active = round(India_active * adj_v)
  India_active_up = round(India_active_up * adj_v)
  India_active_low = round(India_active_low * adj_v)
}

India_active =
  round(N * (data_comp[(train.length + 1):(train.length + forecast.length), "mean"]))

India_active_up =
  round(N * (data_comp[(train.length + 1):(train.length + forecast.length), "upper"]))

India_active_low =
  round(N * (data_comp[(train.length + 1):(train.length + forecast.length), "lower"]))

dataf = Observed_Data$Active_Confirmed

if (adj == T) {
  adj_v =
    mean(as.vector(dataf[(T_prime - adj_len):T_prime]) / N / (data_comp[(T_prime -
                                                                           adj_len):T_prime, "mean"]))
  India_active = round(India_active * adj_v)
  India_active_up = round(India_active_up * adj_v)
  India_active_low = round(India_active_low * adj_v)
}

India_death =
  round(death_in_R * N * (data_comp_R[(train.length + 1):(train.length + forecast.length), "mean"]))

India_death_up =
  round(death_in_R * N * (data_comp_R[(train.length + 1):(train.length + forecast.length), "upper"]))

India_death_low =
  round(death_in_R * N * (data_comp_R[(train.length + 1):(train.length + forecast.length), "lower"]))

dataf = Observed_Data$Total_Death

if (adj == T) {
  adj_v =
    mean(as.vector(dataf[(T_prime - adj_len):T_prime]) / (death_in_R * N) / (data_comp_R[(T_prime -
                                                                                            adj_len):T_prime, "mean"]))
  India_death = round(India_death * adj_v)
  India_death_up = round(India_death_up * adj_v)
  India_death_low = round(India_death_low * adj_v)
}

Results = data.frame(
  forecast_dt,
  India_active,
  India_active_low,
  India_active_up,
  India_confirm,
  India_confirm_low,
  India_confirm_up,
  India_death,
  India_death_low,
  India_death_up
)

write.csv(x = Results,
          file = "revised_output/eSIR_revision/eSIR Results.csv",
          row.names = FALSE)
