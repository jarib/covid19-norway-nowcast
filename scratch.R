library(tidyverse)
library(gridExtra)
library(wesanderson)
library(httr)
library(zoo)
library(jsonlite)

source("nowcasts.R")
source("utils.R")

df <-
  GET("https://www.vg.no/spesial/2020/coronavirus-data/nowcast/dates.json") %>%
  stop_for_status() %>%
  content(as = "text", encoding = "UTF-8") %>%
  fromJSON() %>%
  .$dates %>%
  mutate(
    reportDate = as.Date(reportDate),
    testDate = as.Date(testDate),
    delay = as.numeric(reportDate - testDate)
  )


colors <- wes_palette("Royal1")
minDate <- min(min(df$testDate), min(df$reportDate)) + 90 + 30
maxDate <- max(max(df$testDate), max(df$reportDate))

date_filter <- function(x) {
  x %>% filter(date >= minDate, date <= maxDate)
}

# set up data
ts_test <- count_by_date(df, testDate) %>% date_filter()
ts_report <- count_by_date(df, reportDate) %>% date_filter()

msis <-
  GET("https://redutv-api.vg.no/corona/v1/msis/total") %>%
  content(as = "text") %>%
  fromJSON() %>%
  as_tibble() %>%
  mutate(
    newReportValue = reportValue - lag(reportValue),
    newTestValue = testValue - lag(testValue),
    date = as.Date(date)
  ) %>%
  mutate(
    reportMavg = roll(newReportValue),
    testMavg = roll(newTestValue)
  ) %>%
  date_filter()

# data for which we want to nowcast
nowcast_input <-
  as.data.frame(df %>% filter(reportDate <= (maxDate)))

nc_hh <- nowcast_hh(nowcast_input)
nc_nobbs <- nowcast_nobbs(nowcast_input)
nc_naive <-
  nowcast_weights(
    nowcast_input,
    # weights calculated elsewhere
    c(1.1462158912326739, 1.48626580621125, 6.043134426689718)
  )

plt_rdate <-
  plot_with_mavg(ts_report, x = date, y = n) + labs(x = "", y = "n") + ggtitle("FHI Github etter registreringsdato")

plt_msis_rdate <-
  plot_with_mavg(msis, x = date, y = newReportValue) +
  labs(x = "", y = "n") +
  ggtitle("MSIS skrapet daglig, etter registreringsdato")

plt_tdate_nowcast_hh <-
  plot_nowcast(combine_nowcast(ts_test, nc_hh)) + ggtitle("FHI Github etter prøvedato", subtitle = "med nowcast (Höhle)")

plt_tdate_nowcast_nobbs <-
  plot_nowcast(combine_nowcast(ts_test, nc_nobbs)) + ggtitle("FHI Github etter prøvedato", subtitle = "med nowcast (NobBS)")

plt_tdate_nowcast_weights <-
  plot_nowcast(combine_nowcast(ts_test, nc_naive)) + ggtitle("FHI Github etter prøvedato", subtitle = "med nowcast (naiv vekting)")

grid.arrange(
  plt_rdate,
  plt_msis_rdate,
  plt_tdate_nowcast_hh,
  plt_tdate_nowcast_nobbs,
  plt_tdate_nowcast_weights,
  nrow = 5
)

ts_test_with_nc <-
  ts_test %>%
  left_join(nc_hh %>% setNames(paste0(names(.), ".hh")), by = c("date" = "date.hh")) %>%
  left_join(nc_nobbs %>% setNames(paste0(names(.), ".nobbs")), by = c("date" = "date.nobbs")) %>%
  left_join(nc_weights %>% setNames(paste0(names(.), ".naive")), by = c("date" = "date.naive")) %>%
  mutate(
    observed = n,
    across(starts_with("predicted."), roll, .names = "{.col}.mavg"),
    # rolling average of CI - probably not correct!
    across(starts_with("low."), roll, .names = "{.col}.mavg"),
    across(starts_with("high."), roll, .names = "{.col}.mavg")
  )

View(ts_test_with_nc %>% select(date, starts_with("completeness")) %>% arrange(desc(date)) %>% head(30))
