library(here)
library(lubridate)
library(TTR)

load(here("data", "CKL.rda"))

# Change Point
COVID_StateofEmergency_Date <- as.Date("2020-03-18")

# Time Series Data Frame
## Make a full data frame of all the months from August 2016 to June 2025
CKL_full_months <- tibble(Call_Month = seq(from = floor_date(min(CKL$Call_Date), "month"),
                                           to   = floor_date(max(CKL$Call_Date), "month"),
                                           by   = "month"))


## Monthly Call Counts
CKL_call_counts_monthly <-  CKL |>
  mutate(Call_Month = floor_date(Call_Date, "month")) |>
  count(Call_Month, name = "Num_Calls")


## Monthly call counts with Intervention variables
CKL_call_counts_monthly_w_Intervention <- CKL_full_months |>
  left_join(CKL_call_counts_monthly, by = "Call_Month") |>
  mutate(Num_Calls = replace_na(Num_Calls, 0),
         Time_Point = row_number(),

         # Intervention
         Intervention = if_else(Call_Month < COVID_StateofEmergency_Date, 0L, 1L),
         Intervention = factor(Intervention, levels = c(0,1)),

         # Calculate the # of months post intervention
         Post_Intervention = if_else(Intervention == 1L,
                                     Time_Point - min(Time_Point[Intervention == 1L]) + 1L,0L)) |>
  select(Time_Point, everything())

# Monthly Call Counts Plotted
CKL_call_per_month_cont_lineplot <- ggplot(CKL_call_counts_monthly_w_Intervention,
                                           aes(x = Call_Month,
                                               y = Num_Calls)) +
  geom_point() +
  geom_vline(xintercept = COVID_StateofEmergency_Date,
             color = "red") +
  labs(title = "Number of CKL Drug Toxicity Calls per Month",
       x = "Month",
       y = "Number of Calls") +
  scale_x_date(date_breaks = "6 month",
               date_labels = "%b %Y")

CKL_call_per_month_cont_lineplot

# ITS Model
CKL_ITS_mod <- lm(Num_Calls ~ Time_Point + Intervention + Post_Intervention,
                  data = CKL_call_counts_monthly_w_Intervention)

CKL_ITS_mod |> summary()

# Monthly ITS - Using `ts()`
MonthlyCallCount.ts <- ts(CKL_call_counts_monthly_w_Intervention$Num_Calls,
                          frequency = 12,
                          start= c(2016, 1))
MonthlyCallCount.ts

ts.plot(MonthlyCallCount.ts)

# Call Counts per Day
CKL_daily <- tibble(Call_Day = seq(from = floor_date(min(CKL$Call_Date), "day"),
                                   to   = floor_date(max(CKL$Call_Date), "day"),
                                   by   = "day"))

# Daily Call Counts
CKL_call_counts_daily <-  CKL |>
  mutate(Call_Day = floor_date(Call_Date, "day")) |>
  count(Call_Day, name = "Num_Calls")


# Monthly call counts with Intervention variables
CKL_call_counts_daily_w_Intervention <- CKL_daily |>
  left_join(CKL_call_counts_daily, by = "Call_Day") |>
  mutate(Num_Calls = replace_na(Num_Calls, 0),
         Time_Point = row_number(),

         # No Intervention = 0 ; Intervention = 1
         Intervention = as.factor(if_else(Call_Day < COVID_StateofEmergency_Date, 0, 1)),

         # Calculate the # of months post intervention
         Post_Intervention = if_else(Intervention == 1,
                                     Time_Point - min(Time_Point[Intervention == 1]) + 1,0)) |>
  select(Time_Point, everything())

## Plot Calls per Day with Intervention
CKL_call_daily_plot <- ggplot(CKL_call_counts_daily_w_Intervention,
                              aes(x = Call_Day,
                                  y = Num_Calls)) +
  geom_point() +
  geom_vline(xintercept = COVID_StateofEmergency_Date,
             color = "red") +
  labs(title = "Number of HPS Drug Toxicity Calls per Month",
       x = "Day",
       y = "Number of Calls")

CKL_call_daily_plot

## Daily ITS - Using `ts()`
CKL_DailyCallCount.ts <- ts(CKL_call_counts_daily_w_Intervention$Num_Calls)
CKL_DailyCallCount.ts

ts.plot(CKL_DailyCallCount.ts)

## Daily ITS - Simple Moving Average
CKL_DailyCallCount.sma <- sma(CKL_DailyCallCount.ts, n =14)
CKL_DailyCallCount.sma

plot.ts(CKL_DailyCallCount.sma)



















