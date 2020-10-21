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

plot_nowcast <- function(data, mavg = TRUE) {
  colors <- c("cases" = "lightsteelblue4", "observed" = "indianred4", "predicted" = "#7AD2F6")
  
  plt <- data %>%
    mutate(
      predicted.mavg = roll(predicted),
      # rolling average of CI - probably not correct!
      low.mavg = roll(low),
      high.mavg = roll(high)
    ) %>%
    ggplot() +
    geom_col(aes(x = date, y = predicted, fill = "predicted")) +
    geom_col(aes(x = date, y = cases, fill = "cases"), alpha = 0.5) +
    geom_col(aes(x = date, y = observed, fill = "observed"), alpha = 0.5) +
    geom_errorbar(aes(x = date, ymin = low, ymax = high)) +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
    scale_fill_manual(name="", values=colors) +
    labs(x = "", y = "n") +
    plots.theme()

  if (mavg) {
    plt <- plt + geom_line(aes(x = date, y = mavg)) +
      geom_line(aes(x = date, y = predicted.mavg),
        color = "indianred3",
        linetype = "longdash"
      ) +
      geom_ribbon(
        fill = "indianred3",
        aes(x = date, ymin = low.mavg, ymax = high.mavg),
        alpha = 0.3
      )
  }

  plt
}

count_and_roll <- function(x) {
  x %>%
    tally() %>%
    mutate(mavg = roll(n))
}

combine_nowcast <- function(data, nowcast) {
  data %>%
    left_join(nowcast, by = c("date" = "date")) %>%
    rename(
      cases = n,
      predicted_obnyr = obnyr,
    ) %>%
    mutate(
      actual_obnyr = cases - observed
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
    as_tibble() %>%
    mutate(
      reportDate = as.Date(reportDate),
      testDate = as.Date(testDate),
      delay = as.numeric(reportDate - testDate)
    )
}