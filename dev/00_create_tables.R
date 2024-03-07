library(RPostgres)
library(dplyr)
library(purrr)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "gravitydatasets", host = "localhost",
  user = Sys.getenv("TRADESTATISTICS_SQL_USR"),
  password = Sys.getenv("TRADESTATISTICS_SQL_PWD")
)

con2 <- dbConnect(
  RPostgres::Postgres(),
  dbname = "structural_gravity",
  # host = "shiny.tradestatistics.io",
  host = "localhost",
  user = Sys.getenv("TRADESTATISTICS_SQL_USR"),
  password = Sys.getenv("TRADESTATISTICS_SQL_PWD")
)

glimpse(
  tradepolicy::agtpa_applications
)

usitc_gravity <- tbl(con, "usitc_gravity") %>%
  select(year, iso3_o, iso3_d, dynamic_code_o, dynamic_code_d,
    dist = distance, cntg = contiguity, lang = common_language,
    clny = colony_ever, rta = agree_fta
  ) %>%
  collect()

usitc_gravity <- usitc_gravity %>%
  arrange(year, iso3_o, iso3_d, dynamic_code_o, dynamic_code_d)

usitc_gravity <- usitc_gravity %>%
  group_by(year, iso3_o, iso3_d, dynamic_code_o, dynamic_code_d) %>%
  mutate(
    rta_lag3 = lag(rta, 3),
    rta_lag4 = lag(rta, 4),
    rta_lag6 = lag(rta, 6),
    rta_lag8 = lag(rta, 8),
    rta_lag9 = lag(rta, 9),
    rta_lag12 = lag(rta, 12),
    rta_lead4 = lead(rta, 4)
  )

usitc_trade <- tbl(con, "usitc_trade") %>%
  select(-c(flag_mirror, flag_zero)) %>%
  collect()

usitc_trade_sector <- usitc_trade %>%
  group_by(year, exporter_iso3, exporter_dynamic_code, importer_iso3, importer_dynamic_code, broad_sector_id) %>%
  summarise(trade = sum(trade, na.rm = T)) %>%
  ungroup()

usitc_trade_country <- usitc_trade %>%
  group_by(year, exporter_iso3, exporter_dynamic_code, importer_iso3, importer_dynamic_code) %>%
  summarise(trade = sum(trade, na.rm = T)) %>%
  ungroup()

usitc_gravity <- usitc_trade_country %>%
  inner_join(
    usitc_gravity,
    by = c("year", "exporter_iso3" = "iso3_o", "importer_iso3" = "iso3_d", "exporter_dynamic_code" = "dynamic_code_o", "importer_dynamic_code" = "dynamic_code_d")
  )

usitc_gravity %>%
  filter(year == 2015, exporter_iso3 == "USA", importer_iso3 == "CAN") %>%
  select(rta)

usitc_gravity %>%
  filter(year == 2015, importer_iso3 == "USA", exporter_iso3 == "CAN") %>%
  select(rta)

usitc_industry_names <- tbl(con, "usitc_industry_names") %>%
  collect()

usitc_sector_names <- tbl(con, "usitc_sector_names") %>%
  collect()

usitc_country_names <- tbl(con, "usitc_country_names") %>%
  collect()

dbWriteTable(con2, "usitc_gravity", usitc_gravity, overwrite = T)
dbSendQuery(con2, "CREATE INDEX idx_usitc_gravity_year ON public.usitc_gravity USING btree (year)")
dbSendQuery(con2, "CREATE INDEX idx_usitc_gravity_exporter ON public.usitc_gravity USING btree (exporter_dynamic_code)")
dbSendQuery(con2, "CREATE INDEX idx_usitc_gravity_importer ON public.usitc_gravity USING btree (importer_dynamic_code)")

dbWriteTable(con2, "usitc_trade", usitc_trade, overwrite = T)
dbSendQuery(con2, "CREATE INDEX idx_usitc_trade_year ON public.usitc_trade USING btree (year)")
dbSendQuery(con2, "CREATE INDEX idx_usitc_trade_exporter ON public.usitc_trade USING btree (exporter_dynamic_code)")
dbSendQuery(con2, "CREATE INDEX idx_usitc_trade_importer ON public.usitc_trade USING btree (importer_dynamic_code)")
dbSendQuery(con2, "CREATE INDEX idx_usitc_trade_sector ON public.usitc_trade USING btree (broad_sector_id)")
dbSendQuery(con2, "CREATE INDEX idx_usitc_trade_industry ON public.usitc_trade USING btree (industry_id)")

dbWriteTable(con2, "usitc_trade_sector", usitc_trade_sector, overwrite = T)
dbSendQuery(con2, "CREATE INDEX idx_usitc_trade_sector_year ON public.usitc_trade_sector USING btree (year)")
dbSendQuery(con2, "CREATE INDEX idx_usitc_trade_sector_exporter ON public.usitc_trade_sector USING btree (exporter_dynamic_code)")
dbSendQuery(con2, "CREATE INDEX idx_usitc_trade_sector_importer ON public.usitc_trade_sector USING btree (importer_dynamic_code)")
dbSendQuery(con2, "CREATE INDEX idx_usitc_trade_sector_sector ON public.usitc_trade_sector USING btree (broad_sector_id)")

dbWriteTable(con2, "usitc_trade_country", usitc_trade_country, overwrite = T)
dbSendQuery(con2, "CREATE INDEX idx_usitc_trade_country_year ON public.usitc_trade_country USING btree (year)")
dbSendQuery(con2, "CREATE INDEX idx_usitc_trade_country_exporter ON public.usitc_trade_country USING btree (exporter_dynamic_code)")
dbSendQuery(con2, "CREATE INDEX idx_usitc_trade_country_importer ON public.usitc_trade_country USING btree (importer_dynamic_code)")

dbWriteTable(con2, "usitc_industry_names", usitc_industry_names, overwrite = T)
dbWriteTable(con2, "usitc_sector_names", usitc_sector_names, overwrite = T)
dbWriteTable(con2, "usitc_country_names", usitc_country_names, overwrite = T)

dbDisconnect(con)

dbDisconnect(con2)

golem::use_utils_ui()
