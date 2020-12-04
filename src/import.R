library(tidyverse)
library(readxl)
library(forecast)
library(janitor)

df_forecast <- read_excel("data/forecast_DATA.xlsx") %>% 
  clean_names()

str(df_forecast)

