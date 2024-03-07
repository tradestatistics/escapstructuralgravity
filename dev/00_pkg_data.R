# library(RPostgres)
library(dplyr)
library(purrr)
library(countrycode)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "structural_gravity",
  # host = "shiny.tradestatistics.io",
  host = "localhost",
  user = Sys.getenv("TRADESTATISTICS_SQL_USR"),
  password = Sys.getenv("TRADESTATISTICS_SQL_PWD")
)

agtpa_applications <- tradepolicy::agtpa_applications
use_data(agtpa_applications, overwrite = T)

params <- list()

# params$y <- tbl(con, "usitc_trade_country") %>%
# params$y <- agtpa_applications %>%
#   select(year) %>%
#   distinct() %>%
#   pull() %>%
#   sort()

params$y <- tbl(con, "usitc_trade_country") %>%
  select(year) %>%
  distinct() %>%
  collect() %>%
  pull() %>%
  sort()

# countries <- agtpa_applications %>%
#   select(country_iso = exporter) %>%
#   distinct() %>%
#   bind_rows(
#     agtpa_applications %>%
#       select(country_iso = importer) %>%
#       distinct()
#   ) %>%
#   distinct() %>%
#   mutate(
#     country_name = countrycode::countrycode(country_iso, "iso3c", "country.name"),
#   )

# countries %>%
#   filter(country_iso == "ROM")

# countries <- countries %>%
#   mutate(country_name = ifelse(country_iso == "ROM", "Romania", country_name))

countries <- tbl(con, "usitc_country_names") %>%
  select(country_iso = country_dynamic_code, country_name) %>%
  distinct() %>%
  collect()

params$c <- pull(countries, country_iso)
names(params$c) <- pull(countries, country_name)
params$c <- sort(params$c)

# params$c <- c("World" = "ALL", params$c)

use_data(params, overwrite = T)
