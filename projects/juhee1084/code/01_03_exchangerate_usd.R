# ============================================================
# 01_03_Daily NK USD/KPW exchange rates 
# Pyongyang, Sinuiju, Hyesan
# -> monthly average
# -> merge with monthly rice price data
# ============================================================


# ------------------------------------------------------------
# 0. Initial settings
# ------------------------------------------------------------

source("code/00_initial.R")


# ------------------------------------------------------------
# 1. File paths
# ------------------------------------------------------------

exchange_rate <- 
  "data/raw/exchange_rate/dailynk_exchange_rates.xls"

rice_file <- 
  "data/processed/rice_price/nk_rice_price_monthly_201301_202607.rds"


# ------------------------------------------------------------
# 2. Read Daily NK exchange-rate file
#    Note: extension is .xls, but internal format is XML
# ------------------------------------------------------------

doc <- xml2::read_xml(exchange_rate)

rows <- xml2::xml_find_all(
  doc,
  "//*[local-name()='Worksheet' and @*[local-name()='Name']='Exchange Rates']
   /*[local-name()='Table']
   /*[local-name()='Row']"
)


# ------------------------------------------------------------
# 3. Extract rows
# ------------------------------------------------------------

extract_row <- function(row) {

  cells <- xml2::xml_find_all(
    row,
    "./*[local-name()='Cell']/*[local-name()='Data']"
  )

  values <- xml2::xml_text(cells)

  length(values) <- 4

  values
}


exchange_matrix <- purrr::map(
  rows,
  extract_row
) |>
  do.call(rbind, args = _)


exchange_obs <- tibble::as_tibble(
  exchange_matrix[-1, ],
  .name_repair = "minimal"
)

names(exchange_obs) <- c(
  "date",
  "usd_kpw_pyongyang",
  "usd_kpw_sinuiju",
  "usd_kpw_hyesan"
)


# ------------------------------------------------------------
# 4. Clean variables
# ------------------------------------------------------------

exchange_obs <- exchange_obs |>
  dplyr::mutate(
    date = lubridate::mdy(date),

    dplyr::across(
      dplyr::starts_with("usd_kpw_"),
      as.numeric
    ),

    month = format(date, "%Y-%m")
  ) |>
  dplyr::arrange(date)


# ------------------------------------------------------------
# 5. Restrict to study period
#    2013-01 through 2026-07
# ------------------------------------------------------------

exchange_obs <- exchange_obs |>
  dplyr::filter(
    date >= lubridate::ymd("2013-01-01"),
    date <= lubridate::ymd("2026-07-31")
  )


# ------------------------------------------------------------
# 6. Create monthly average exchange rates
# ------------------------------------------------------------

exchange_monthly <- exchange_obs |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    usd_kpw_pyongyang = mean(usd_kpw_pyongyang, na.rm = TRUE),
    usd_kpw_sinuiju   = mean(usd_kpw_sinuiju,   na.rm = TRUE),
    usd_kpw_hyesan    = mean(usd_kpw_hyesan,    na.rm = TRUE),
    n_exchange_obs    = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(month)


# Check exchange data
dplyr::glimpse(exchange_monthly)
print(exchange_monthly, n = 20)


# ------------------------------------------------------------
# 7. Save exchange-rate monthly data
# ------------------------------------------------------------

saveRDS(
  exchange_monthly,
  "data/processed/exchange_rate/nk_exchange_rate_monthly_201301_202607.rds"
)

haven::write_dta(
  exchange_monthly,
  "data/processed/exchange_rate/nk_exchange_rate_monthly_201301_202607.dta"
)


# ------------------------------------------------------------
# 8. Read monthly rice-price data
# ------------------------------------------------------------

rice_price_monthly <- readRDS(rice_file)


# If month is not already YYYY-MM, standardize it
rice_price_monthly <- rice_price_monthly |>
  dplyr::mutate(
    month = substr(as.character(month), 1, 7)
  )


# Check rice data
dplyr::glimpse(rice_price_monthly)

# Check uniqueness of month
anyDuplicated(exchange_monthly$month)
anyDuplicated(rice_price_monthly$month)

# ------------------------------------------------------------
# 9. Merge rice prices + exchange rates
# ------------------------------------------------------------

rice_exchangerate_monthly_fx <- rice_price_monthly |>
  dplyr::left_join(
    exchange_monthly,
    by = "month"
  ) |>
  dplyr::arrange(month)


# ------------------------------------------------------------
# 10. Check merged data
# ------------------------------------------------------------

dplyr::glimpse(rice_exchangerate_monthly_fx)

rice_exchangerate_monthly_fx |>
  dplyr::select(
    month,
    rice_pyongyang, usd_kpw_pyongyang,
    rice_sinuiju,   usd_kpw_sinuiju,
    rice_hyesan,    usd_kpw_hyesan,
    n_exchange_obs
  ) |>
  print(n = 30)


# Check that merge did not change the number of rows
nrow(rice_price_monthly)
nrow(rice_price_monthly_fx)

stopifnot(
  nrow(rice_price_monthly) ==
    nrow(rice_price_monthly_fx)
)


# ------------------------------------------------------------
# 11. Check months with missing exchange rates
# ------------------------------------------------------------

rice_exchangerate_monthly_fx |>
  dplyr::filter(
    is.na(usd_kpw_pyongyang) |
    is.na(usd_kpw_sinuiju)   |
    is.na(usd_kpw_hyesan)
  ) |>
  dplyr::select(
    month,
    rice_pyongyang,
    rice_sinuiju,
    rice_hyesan,
    dplyr::starts_with("usd_kpw_")
  ) |>
  print(n = Inf)

summary(rice_exchangerate_monthly_fx)

# ------------------------------------------------------------
# 12. Save merged data
# ------------------------------------------------------------

saveRDS(
  rice_price_monthly_fx,
  "data/processed/rice_exchangerate/nk_rice_exchange_monthly_201301_202607.rds"
)

haven::write_dta(
  rice_price_monthly_fx,
  "data/processed/rice_exchangerate/nk_rice_exchange_monthly_201301_202607.dta"
)