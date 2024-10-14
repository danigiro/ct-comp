library(tidyverse)

load("./results/fore_tibble_ite.RData")
data_ite |>
  pivot_wider(names_from = type, values_from = data) |>
  mutate(diff = unlist(Map(function(x, y) sum((x-y)^2), x = tcs, y = cst))) |>
  group_by(name) |>
  summarise(check = all(diff < 1e-6))

load("./results/fore_tibble_ka.RData")

data_ka |>
  pivot_wider(names_from = type, values_from = data) |>
  mutate(diff = unlist(Map(function(x, y) sum((x-y)^2), x = tcs, y = cst))) |>
  group_by(name) |>
  summarise(check = all(diff < 1e-6))

load("./results/fore_tibble_oct.RData")

data_ite |>
  pivot_wider(names_from = type, values_from = data) |>
  mutate(name = recode(name, "ols-ols"="ols", "str-str"="str", "ols-str"="csstr", 
                       "str-ols"="testr", "wlsv-wls"="wlsv")) |>
  full_join(data_oct|>
              rename(oct = data)) |>
  mutate(diff = unlist(Map(function(x, y) sum((x-y)^2), x = tcs, y = oct))) |>
  group_by(name) |>
  summarise(check = all(diff < 1e-6))

data_ka |>
  pivot_wider(names_from = type, values_from = data) |>
  mutate(name = recode(name, "ols-ols"="ols", "str-str"="str", "ols-str"="csstr", 
                       "str-ols"="testr", "wlsv-wls"="wlsv")) |>
  full_join(data_oct |>
              rename(oct = data)) |>
  mutate(diff = unlist(Map(function(x, y) sum((x-y)^2), x = tcs, y = oct))) |>
  group_by(name) |>
  summarise(check = all(diff < 1e-6))
