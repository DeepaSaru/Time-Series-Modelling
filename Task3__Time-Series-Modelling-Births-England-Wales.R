# ==============================================================================
#                            PROJECT INFORMATION
# ------------------------------------------------------------------------------
# Title        : Time Series Modelling of Live Births in England & Wales
# Author       : Deepa Ramu
# Student ID   : @00801743
# Course/Module: Applied Statistics and Data Visualization
# Description  : 
#   This R script performs exploratory data analysis (EDA), time-series 
#   transformation, model building, and forecasting for annual live births in 
#   England & Wales. Multiple forecasting models are evaluated and compared, 
#   including Holt-Winters, ARIMA, ETS, and Neural Network models.
# ==============================================================================


# ==============================================================================
#                               TABLE OF CONTENTS
# ------------------------------------------------------------------------------
# 1. Load Libraries
#
# 2. Data Import & Exploratory Data Analysis (EDA)
#       - 2.1  Read Excel file
#       - 2.2  Select Births (England & Wales) dataset & remove metadata
#       - 2.3  Dimensions & structure
#       - 2.4  Identify time range
#       - 2.5  Year-over-Year Change Analysis 
#       - 2.6  Year-over-Year Percentage Change
#       - 2.7  Boxplot Across Historical Periods
#       - 2.8  Summary Table — Key Descriptive Statistics
#       - 2.9  Rolling Mean & Rolling Standard Deviation — Trend & Variability
#
# 3. Convert to Time Series 
#       - Convert to ts() object
#       - 3.1 Plot Original Time Series
#       - 3.2 Trend Extraction Using Simple Moving Average (SMA)
#
# 4. Time-Series Forecasting Models
#       - 4.1 Holt-Winters Exponential Smoothing (Simple)
#       - 4.2 Holt’s Exponential Smoothing (Additive Trend, No Seasonality)
#       - 4.3 ARIMA (AutoRegressive Integrated Moving Average)
#       - 4.4 ETS (Error–Trend–Seasonal Framework)
#       - 4.5 Neural Network Time Series Model (NNAR)
#
# 5. Model Fitting & Forecast Comparison (10-year horizon)
#       - Model Accuracy Comparison (RMSE, MAE, MAPE, MASE, ACF1)
# ==============================================================================



# ==============================================================================
# 1. Install & Load Required Libraries
# ==============================================================================

# Install first time only
install.packages(c("readxl", "dplyr", "ggplot2", "forecast", "tseries","TTR"))

# Load libraries
library(readxl)    # Read Excel
library(dplyr)     # Data wrangling
library(ggplot2)   # Visualisation
library(forecast)  # Time-series models
library(tseries)   # ADF test
library(TTR)       # Simple Moving Average (SMA)
library(scales)    # Axis formatting

# ==============================================================================
# 2. Data Import
# ==============================================================================

# --- FilePath ---
file_path <- "Vital statistics in the UK.xlsx"

# --- Read Birth sheet (skip metadata) ---
birth_raw <- read_excel(file_path, sheet = "Birth", skip = 5)

# --- View column names ---
names(birth_raw)

births_england_wales <- birth_raw %>%
  
  # Select only the columns we need and rename them
  select(
    year = Year,                                      
    live_births_england_wales = `Number of live births: England and Wales`
  ) %>%
  
  # Remove rows where either year or births is missing
  filter(!is.na(year), !is.na(live_births_england_wales)) %>%  
  
  # Convert both columns to numeric to ensure they can be used in analysis
  mutate(
    year = as.numeric(year),                          
    live_births_england_wales = as.numeric(live_births_england_wales)
  )


# --- Preview first rows ---
head(births_england_wales)

# --- Preview first rows ---
tail(births_england_wales)

# --- Summary statistics ---
summary(births_england_wales)  

# --- Identify time range ---
start_year <- min(births_england_wales$year)
cat(paste0("The dataset begins from the year ", start_year, ".", "\n"))
end_year   <- max(births_england_wales$year)
cat(paste0("The dataset ends at ", end_year, ".\n"))

# ==============================================================================
# --- Year-over-Year Change Analysis ---
# ==============================================================================
births_england_wales <- births_england_wales %>%
  mutate(yearly_change = live_births_england_wales - lag(live_births_england_wales))

# Remove NA from first row
births_england_wales_clean <- births_england_wales %>%
  filter(!is.na(yearly_change))

ggplot(births_england_wales_clean, aes(year, yearly_change)) +
  geom_line(color = "darkred", size = 0.9) +
  scale_x_continuous(
    breaks = seq(start_year, end_year, by = 20)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  labs(
    title = "Year-over-Year Change in Live Births – England & Wales",
    x = "Year",
    y = "Change in Live Births"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# ==============================================================================
# ---- Year-over-Year Percentage Change ----
# ==============================================================================

births_england_wales <- births_england_wales %>%
  mutate(pct_change = (yearly_change / lag(live_births_england_wales)) * 100)

# Plot YoY %
ggplot(births_england_wales %>% filter(!is.na(pct_change)),
       aes(year, pct_change)) +
  geom_line(color = "blue", size = 0.9) +
  scale_x_continuous(
    breaks = seq(start_year, end_year, by = 10)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Year-over-Year % Change in Live Births – England & Wales",
    x = "Year",
    y = "% Change"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# ==============================================================================
# ---- Boxplot Across Historical Periods ----
# ==============================================================================
births_england_wales <- births_england_wales %>%
  mutate(period = case_when(
    year < 1900 ~ "Before 1900",
    year < 1950 ~ "1900–1949",
    year < 2000 ~ "1950–1999",
    TRUE ~ "2000–Present"
  ))

ggplot(births_england_wales, aes(period, live_births_england_wales)) +
  geom_boxplot(fill = "orange") +
  labs(
    title = "Live Births by Historical Period",
    x = "Period",
    y = "Number of Live Births"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# ==============================================================================
# ---- Summary Table — Key Descriptive Stats ----
# ==============================================================================

birth_summary <- births_england_wales %>%
  summarise(
    count = n(),
    min_births = min(live_births_england_wales),
    max_births = max(live_births_england_wales),
    mean_births = mean(live_births_england_wales),
    median_births = median(live_births_england_wales),
    sd_births = sd(live_births_england_wales)
  )

birth_summary

# ==============================================================================
# ---- Rolling Mean & Rolling SD — Trend + Variability ----
# ==============================================================================

library(zoo)
births_england_wales <- births_england_wales %>%
  mutate(
    roll_mean_10 = rollmean(live_births_england_wales, 10, fill = NA),
    roll_sd_10   = rollapply(live_births_england_wales, 10, sd, fill = NA)
  )

ggplot(births_england_wales, aes(year, roll_sd_10)) +
  geom_line(color = "purple") +
  labs(
    title = "10-Year Rolling Standard Deviation of Live Births",
    x = "Year",
    y = "Rolling Std. Dev."
  ) +
  theme_minimal()

# ==============================================================================
# 3. Convert to Time-Series Object
# ==============================================================================

birthtimeseries <- ts(
  births_england_wales$live_births_england_wales,
  start = start_year,
  frequency = 1                      # Annual data → non-seasonal
)

# ==============================================================================
# ---- Plot Original Time Series ----
# ==============================================================================

autoplot(birthtimeseries) +
  labs(
    title = "Annual Live Births – England & Wales",
    x = "Year",
    y = "Number of Live Births"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# ==============================================================================
# ---- Trend Extraction Using SMA (Non-Seasonal Data) ----
# ==============================================================================

# 3-year SMA
birthtimeseriesSMA3 <- SMA(birthtimeseries, n = 3)

autoplot(birthtimeseries) +
  autolayer(birthtimeseriesSMA3, color = "red", size = 1.2) +
  labs(
    title = "3-year SMA Trend – England & Wales Births",
    x = "Year",
    y = "Number of Live Births",
    caption = "Red = 3-year SMA"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# 5-year SMA
birthtimeseriesSMA5 <- SMA(birthtimeseries, n = 5)

autoplot(birthtimeseries) +
  autolayer(birthtimeseriesSMA5, color = "red", size = 1.2) +
  labs(
    title = "5-year SMA Trend – England & Wales Births",
    x = "Year",
    y = "Number of Live Births",
    caption = "Red = 5-year SMA"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# ==============================================================================
# 4.  Time-Series Forecasting Models
# ==============================================================================

#===============================================================================
# ---- Model 1 - Holt-Winters Exponential Smoothing(Simple) ----
#===============================================================================

# -- beta = FALSE → no trend component & gamma = FALSE → no seasonal component
birthtimeseriesforecasts <- HoltWinters(birthtimeseries, beta=FALSE, gamma=FALSE)
birthtimeseriesforecasts

#Summary

# Good exploratory step
# But NOT suitable because births data clearly has a trend

# alpha ≈ 1
# The model gives almost full weight to the most recent value
# Older observations barely influence the fitted values
# Suggests data is changing over time, and SES is trying to keep up

# Coefficients
# a = 593423.6

# Model estimates the average number of births ≈ 593,424
# But because trend exists, this average is not stable over time → SES becomes inappropriate

#===============================================================================
# ---- Model 2 - Holt’s Exponential Smoothing (Additive Trend, No Seasonality) ----
#===============================================================================

# Filter dataset from 1970 onward
recent_births <- births_england_wales %>%
  filter(year >= 1970)

# Convert to time-series object
birth_ts_recent <- ts(
  recent_births$live_births_england_wales,
  start = 1970,
  frequency = 1   # Annual → no seasonality
)

# ==============================================================================
# STEP 1 — Fit Holt’s Exponential Smoothing Model 
# ==============================================================================

# gamma = FALSE → no seasonal component(Data is Annual)
# beta ~ estimated -> the model includes a trend
holt_model <- HoltWinters(birth_ts_recent, gamma=FALSE)
holt_model

#Summary 
# alpha = 1
# The level component responds fully to the newest observation
# Older years contribute almost nothing
# Indicates rapid structural change in the births trend

# beta = 0.680
# Controls trend smoothing
# Small value → trend updates slowly over time
# Suggests trend exists but changes gradually

# gamma = FALSE
# No seasonal smoothing (correct—no seasonality in yearly data)

# a — Level estimate
# Estimated baseline births ≈ 593,422 per year

# b — Trend estimate
# Estimated annual change in births ≈ –4,365
# Negative slope → projected decline in future births
# Matches real-world demographic decline in recent decades

# ==============================================================================
# STEP 2 — Inspect Smoothing Parameters & Model Coefficients
# ==============================================================================

holt_model$SSE

# ==============================================================================
# STEP 3 — Visualize Fitted vs Observed Values (In-Sample Performance)
# ==============================================================================

plot(holt_model)
#Summary
# Fitted values closely match observed births — good in-sample fit.
# Any deviations are small and non-systematic.
# Indicates Holt model captures underlying trend effectively.
# Fit is reliable and suitable for forecasting.

# ==============================================================================
# STEP 4 — Generate 10-Year Ahead Forecasts
# ==============================================================================
holt_forecast <- forecast(holt_model, h = 10)

library(ggplot2)
library(forecast)
library(scales)

autoplot(holt_forecast) +
  labs(
    title = "10-Year Forecast of Annual Live Births – England & Wales",
    subtitle = "Holt’s Exponential Smoothing (Additive Trend)",
    x = "Year",
    y = "Number of Live Births"
  ) +
  scale_y_continuous(
    labels = comma,
    limits = c(
      min(holt_forecast$lower[,2]) * 0.98,
      max(holt_forecast$upper[,2]) * 1.02
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ==============================================================================
# STEP 5 — Residual Diagnostics: Autocorrelation Check (ACF Plot)
# ==============================================================================

acf(na.omit(holt_forecast$residuals), lag.max = 20)
# ACF shows no significant autocorrelation — residuals resemble white noise 

# ==============================================================================
# STEP 6 — Residual Independence Test (Ljung–Box)
# ==============================================================================
Box.test(na.omit(holt_forecast$residuals), lag = 20, type = "Ljung-Box")

# p-value = 0.6648 → residuals independent 
# ACF + Ljung–Box p-value (~0.66) together = strong evidence of model adequacy

# ==============================================================================
# STEP 7 — Plot Residuals Over Time
# ==============================================================================
plot.ts(holt_forecast$residuals)

# ==============================================================================
# STEP 8 — Assess Residual Distribution (Normality Check)
# ==============================================================================

holt_forecast$residuals <-
  holt_forecast$residuals[!is.na(holt_forecast$residuals)]

library(ggplot2)
library(scales)

resid_df <- data.frame(resid = na.omit(holt_forecast$residuals))

ggplot(resid_df, aes(resid)) +
  geom_histogram(bins = 20, fill = "gray70", color = "black") +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Histogram of Forecast Residuals",
    x = "Residuals",
    y = "Frequency"
  ) +
  theme_minimal()

# ==============================================================================
# STEP 9 — Evaluate Forecast Accuracy Metrics
# ==============================================================================

accuracy(holt_forecast)

# ==============================================================================
# STEP 10 — Extract Forecasted Values
# ==============================================================================

holt_forecast$mean

# ==============================================================================
# END OF HOLT MODEL — MODEL VALIDATED AND FORECASTING READY
# ==============================================================================


#===============================================================================
# MODEL 3 - ARIMA MODELS
#===============================================================================

# ==============================================================================
# STEP 1 — Visualise Original Time Series
# ==============================================================================

# For formatting axis labels (e.g., 600,000 instead of 6e+05)
library(scales)   

# --- Plot the original annual births time series ---
autoplot(birthtimeseries) +
  labs(
    title = "Annual Live Births – England & Wales",   
    x = "Year",                                       
    y = "Number of live births"                       
  ) +
  scale_y_continuous(labels = comma) +                
  theme_minimal()                                     

# ==============================================================================
# STEP 2 — Check Stationarity Using ADF Test
# ==============================================================================

library(tseries)     # Contains the Augmented Dickey-Fuller (ADF) stationarity test

adf.test(birthtimeseries)

# Interpretation:
# p-value > 0.05 → NOT stationary → differencing required
# p-value ≤ 0.05 → stationary → ARIMA can be fitted without differencing

# ==============================================================================
# STEP 3 — Apply First-Order Differencing to Remove Trend
# ==============================================================================
birth_diff <- diff(birthtimeseries, differences = 1)

# Plot differenced series to visually confirm stationarity
autoplot(birth_diff)

# ==============================================================================
# STEP 4 — Re-Test Stationarity After Differencing
# ==============================================================================

adf.test(birth_diff)

library(forecast)   # Provides auto.arima() & forecasting functions

# ==============================================================================
# STEP 5 — Automatically Select Best ARIMA(p,d,q) Model
# ==============================================================================

recent <- births_england_wales %>% filter(year >= 1970)
birth_ts_recent <- ts(recent$live_births_england_wales,
                      start = 1970, frequency = 1)

arima_recent <- auto.arima(birth_ts_recent)

# ==============================================================================
# STEP 6 — Generate 10-Year Forecasts
# ==============================================================================

arima_forecast <- forecast(arima_recent, h = 10)

# Visualise forecast with confidence intervals
autoplot(arima_forecast) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "ARIMA Forecast – Annual Live Births",
       x = "Year", y = "Number of live births")


# ==============================================================================
# STEP 7 — Evaluate Forecast Accuracy
# ==============================================================================
# Evaluate forecast accuracy metrics (RMSE, MAE, MAPE, etc.)
accuracy(arima_forecast)

# ==============================================================================
# STEP 8 — Residual Diagnostics: Autocorrelation Check
# ==============================================================================
# Check whether residuals still contain autocorrelation
acf(arima_forecast$residuals)

# ==============================================================================
# STEP 9 — Ljung–Box Test for Residual Independence
# ==============================================================================

Box.test(arima_forecast$residuals, lag = 20, type = "Ljung-Box")
# p-value > 0.05 → residuals are independent → good model fit

# ==============================================================================
# STEP 10 — Inspect Residual Distribution
# ==============================================================================

# Check residual distribution — should be roughly normal & centered around zero
hist(arima_forecast$residuals,
     col="lightgray",
     main="Histogram of ARIMA Residuals",
     xlab="Residuals")

# ==============================================================================
# STEP 11 — Extract Forecasted Values
# ==============================================================================

arima_forecast$mean

# ==============================================================================
# END OF ARIMA MODEL — MODEL VALIDATED AND FORECASTING READY
# ==============================================================================


#===============================================================================
# MODEL 4 - ETS MODELS
#===============================================================================

# ==============================================================================
# STEP 1 — Fit ETS Model (Automatic Exponential Smoothing)
# ==============================================================================
library(forecast)

recent_births <- births_england_wales %>%
  filter(year >= 1970)

birth_ts_recent <- ts(
  recent_births$live_births_england_wales,
  start = 1970,
  frequency = 1   # annual data → no seasonality
)

ets_model <- ets(birth_ts_recent)
ets_model     # Prints model type e.g., ETS(A,A,N)

# ==============================================================================
# STEP 2 — Inspect Selected ETS Model Structure
# ==============================================================================

summary(ets_model)

# ==============================================================================
# STEP 3 — Generate 10-Year Forecasts
# ==============================================================================

ets_forecast <- forecast(ets_model, h = 10)

autoplot(ets_forecast) +
  labs(
    title = "ETS Forecast – Annual Live Births",
    x = "Year",
    y = "Number of live births"
  ) +
  theme_minimal()

# ==============================================================================
# STEP 4 — Evaluate Forecast Accuracy
# ==============================================================================

accuracy(ets_forecast)

# ==============================================================================
# STEP 5 — Check Autocorrelation of Residuals
# ==============================================================================

acf(ets_forecast$residuals, lag.max = 20)

# ==============================================================================
# STEP 6 — Perform Ljung–Box Test (Independence Check)
# ==============================================================================

Box.test(ets_forecast$residuals, lag = 20, type = "Ljung-Box")
# p-value > 0.05 → residuals behave like white noise → valid model

# ==============================================================================
# STEP 7 — Visualise Residuals Over Time
# ==============================================================================

plot.ts(ets_forecast$residuals,
        main = "Residuals from ETS Model",
        ylab = "Residuals")

# ==============================================================================
# STEP 8 — Inspect Residual Distribution
# ==============================================================================

hist(
  ets_forecast$residuals,
  breaks = 20,
  col = "lightgray",
  main = "Histogram of ETS Residuals",
  xlab = "Residuals"
)

# ==============================================================================
# STEP 9 — Extract Forecasted Values
# ==============================================================================

ets_forecast$mean

#===============================================================================
# MODEL 5 - Neural Network Time Series(NNAR)
#===============================================================================

# Filter dataset from 1970 onward
recent_births <- births_england_wales %>%
  filter(year >= 1970)

# Convert to time series
birth_ts_recent <- ts(
  recent_births$live_births_england_wales,
  start = 1970,
  frequency = 1   # Annual → No seasonality
)

# Make NNAR reproducible
set.seed(42)

# Fit Neural Network Time Series Model
nn_model <- nnetar(birth_ts_recent)

# 10-year Forecast
nn_forecast <- forecast(nn_model, h = 10)

# Forecast Accuracy
accuracy(nn_forecast)

# Visualise Forecast
autoplot(nn_forecast) +
  labs(
    title = "NNAR Forecast – Live Births (England & Wales, Post-1970)",
    x = "Year",
    y = "Number of Live Births"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 13)


# Extract Forecasted Values
nn_forecast$mean

#===============================================================================
# 5. MODEL FITTING & FORECASTING COMPARISON (10-year horizon)
#===============================================================================

# Forecast horizon (same for all models)
h <- 10

# --- Holt’s Trend Method (additive trend, no seasonality)
# Fits Holt's linear trend model (no gamma because annual data has no seasonality)
hw_trend    <- HoltWinters(birth_ts_recent, gamma = FALSE)
# Generate 10-year forecast
hw_trend_fc <- forecast(hw_trend, h = h)

# --- ARIMA 
# Automatically selects best ARIMA(p,d,q) model based on AICc
arima_model <- auto.arima(birth_ts_recent)
# Forecast 10 years ahead
arima_fc    <- forecast(arima_model, h = h)

# --- ETS (Error, Trend, Seasonal)
# Automatically selects exponential smoothing model from the ETS family
ets_model   <- ets(birth_ts_recent)
# Forecast 10 years ahead
ets_fc      <- forecast(ets_model, h = h)

# --- NNAR (Neural Network Autoregression)
# Trains a neural network that models nonlinear patterns in the series
nn_model    <- nnetar(birth_ts_recent)
# Forecast 10 years ahead using neural network
nnar_fc     <- forecast(nn_model, h = h)

#===============================================================================
# MODEL ACCURACY COMPARISON
#===============================================================================

library(dplyr)

model_comparison <- bind_rows(
  accuracy(hw_trend_fc) %>% data.frame() %>% mutate(Model = "Holt’s Linear Trend"),
  accuracy(arima_fc)    %>% data.frame() %>% mutate(Model = "ARIMA"),
  accuracy(ets_fc)      %>% data.frame() %>% mutate(Model = "ETS"),
  accuracy(nnar_fc)     %>% data.frame() %>% mutate(Model = "NNAR")
) %>%
  # Keep only the most useful accuracy metrics for comparison
  select(Model, ME, RMSE, MAE, MAPE, MASE, ACF1)

# Display accuracy metrics for all models in one table
model_comparison

#===============================END=============================================








