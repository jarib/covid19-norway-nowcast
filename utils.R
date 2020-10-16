library(tidyverse)
library(zoo)
library(ggplot2)
library(ggthemes)
library(httr)
library(jsonlite)

plots.theme <- theme_fivethirtyeight
plots.mavg.window <- 7

roll <- function(x) {
  zoo::rollmean(x, plots.mavg.window, na.pad = TRUE, align = "right")
}

plot_with_mavg <- function(data, x, y, line) {
  data %>%
    mutate(mavg = roll({{ y }})) %>%
    ggplot() +
    geom_col(aes(x = {{ x }}, y = {{ y }})) +
    geom_line(aes(x = {{ x }}, y = mavg)) +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
    plots.theme()
}

plot_nowcast <- function(data) {
  data %>%
    mutate(
      predicted.mavg = roll(predicted),
      # rolling average of CI - probably not correct!
      low.mavg = roll(low),
      high.mavg = roll(high)
    ) %>%
    ggplot() +
    geom_col(aes(x = date, y = observed)) +
    geom_line(aes(x = date, y = mavg)) +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
    theme_wsj() +
    geom_col(aes(x = date, y = predicted), alpha = 0.3) +
    geom_errorbar(aes(x = date, ymin = low, ymax = high)) +
    geom_line(aes(x = date, y = predicted.mavg),
      color = "indianred3",
      linetype = "longdash"
    ) +
    geom_ribbon(
      fill = "indianred3",
      aes(x = date, ymin = low.mavg, ymax = high.mavg),
      alpha = 0.3
    ) +
    labs(x = "", y = "n") +
    plots.theme()
}

count_and_roll <- function(x) {
  x %>%
    tally() %>%
    mutate(mavg = roll(n))
}

combine_nowcast <- function(data, nowcast) {
  data %>%
    left_join(nowcast, by = c("date" = "date")) %>%
    mutate(
      observed = n,
    )
}

count_by_date <- function(data, x) {
  data %>%
    group_by({{ x }}) %>%
    rename(date = {{ x }}) %>%
    arrange(date) %>%
    count_and_roll()
}

fetch_latest_linelist <- function() {
  GET("https://www.vg.no/spesial/2020/coronavirus-data/nowcast/dates.json") %>%
    stop_for_status() %>%
    content(as = "text", encoding = "UTF-8") %>%
    fromJSON() %>%
    .$dates %>%
    as_tibble %>%
    mutate(
      reportDate = as.Date(reportDate),
      testDate = as.Date(testDate),
      delay = as.numeric(reportDate - testDate)
    )

}