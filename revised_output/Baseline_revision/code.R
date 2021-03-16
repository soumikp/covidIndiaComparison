require(tidyverse)

dat <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", col_types = cols()) %>%
  dplyr::rename(state = `Province/State`, name = `Country/Region`) %>%
  dplyr::select(-c(Lat, Long, state)) %>%
  group_by(name) %>%
  summarize_all(list(sum)) %>%
  ungroup() %>%
  pivot_longer(names_to = "date", values_to = "cases", -name) %>%
  mutate(
    date = as.Date(date, "%m/%d/%y")#,
    #fhos = as.numeric(name %in% women_countries)
  )

helper <- function(x, lag){
  n = length(x)
  y <- c(rep(NA, lag),
         (x[(1+lag):n] - x[1:(n - lag)])/(2*x[(1+lag):n]))
  
}

x <- dat %>% 
  filter(name == "India") %>% 
  filter(date >= "2020-03-15") %>% 
  mutate(l = helper(cases, 2)) %>% 
  mutate(t = (seq_along(date)-3)) %>% 
  drop_na()


model <- lm(log(l) ~ t, 
             data = x %>% 
               filter(date<="2020-10-15"))


predicted <- 1/((1-2*exp(predict.lm(model, 
                                    newdata = x %>% 
                                      filter(date>"2020-10-15") %>% 
                                      filter(date<="2020-12-31"))))/x %>% 
                  filter(date>"2020-10-15") %>% filter(date<="2020-12-31") %>% pull(cases))

observed <- x %>% filter(date>"2020-10-15") %>% filter(date<="2020-12-31") %>% select(date, cases)


observed <- observed %>% add_column(predicted_cases = as.numeric(predicted))


write.table(observed, "baseline.txt")

data <- as_tibble(read.table("~/Box/COVID India Comparisons/Results/India/Codes/estimate_and_prediction_of_india_2.txt",
                             header = TRUE))

data <- data %>% 
  mutate(Date = as.POSIXct(Date)) %>% 
  filter(Date > "2020-06-18", Date <="2020-07-18")

mean(((predicted - observed)^2)/((data %>% pull(I) - observed)^2))


plot(x = predicted, 
     y = data %>% pull(I))

ggplot(data = x %>% 
         filter(date<="2020-07-18"),
       aes(x = t, 
           y = log(l))) + 
  geom_point() + 
  stat_smooth(method = "lm") + 
  stat_smooth(method = "smooth")


ggplot(data) + 
  geom_point(aes(x = t, y = l)) + 
  geom_smooth(stat = "smooth")


