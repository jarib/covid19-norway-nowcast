library(tidyverse)
library(gridExtra)
library(wesanderson)
library(httr)
library(zoo)
library(jsonlite)

source("nowcasts.R")

roll <- function(x) {
  zoo::rollmean(x, 7, na.pad = TRUE, align = "right")
}

count_and_roll <- function(x) {
  x %>% tally() %>% mutate(mavg = roll(n))
}

plot_nowcast_line <- function(df) {
  list(
    geom_line(
      data = df,
      aes(x = date, y = predicted),
      linetype = 'longdash'
    ),
    geom_ribbon(
      data = df,
      fill = "indianred3",
      aes(x = date, ymin = low, ymax = high),
      alpha = 0.3
    )
  )
}


input <- GET("https://www.vg.no/spesial/2020/coronavirus-data/nowcast/dates.json") %>%
  content(as = "text", encoding = "UTF-8") %>%
  fromJSON() %>%
  .$dates %>%
  as_tibble()

df <-
  input %>% as_tibble() %>% mutate(reportDate = as.Date(reportDate),
                                   testDate = as.Date(testDate),
                                   delay = as.numeric(reportDate - testDate))

colors <- wes_palette("Royal1")
minDate <- min(min(df$testDate), min(df$reportDate)) + 90 + 30
maxDate <- max(max(df$testDate), max(df$reportDate))

date_filter <- function(x) {
  x %>% filter(date >= minDate, date <= maxDate)
}

# set up data
ts_test <-
  df %>% group_by(testDate) %>% rename(date = testDate) %>% count_and_roll() %>% date_filter()

ts_report <-
  df %>% group_by(reportDate) %>% rename(date = reportDate) %>% count_and_roll() %>% date_filter()

msis <- GET("https://redutv-api.vg.no/corona/v1/msis/total") %>%
  content(as = "text") %>%
  fromJSON() %>%
  as_tibble() %>%
  mutate(
    newReportValue = reportValue - lag(reportValue),
    newTestValue = testValue - lag(testValue),
    date = as.Date(date)
  ) %>%
  mutate(reportMavg = roll(newReportValue),
         testMavg = roll(newTestValue)) %>%
  date_filter()

# data for which we want to nowcast
nowcast_input <-
  as.data.frame(df %>% filter(reportDate <= (maxDate)))

nc_hh <- nowcast_hh(nowcast_input)
nc_nobbs <- nowcast_nobbs(nowcast_input)
nc_weights <-
  nowcast_weights(nowcast_input, c(1.1462158912326739,1.48626580621125,6.043134426689718))

ts_test_with_nc <- ts_test %>%
  left_join(nc_hh %>% setNames(paste0(names(.), '.hh')), by = c("date" = "date.hh")) %>%
  left_join(nc_nobbs %>% setNames(paste0(names(.), '.nobbs')), by = c("date" = "date.nobbs")) %>%
  left_join(nc_weights %>% setNames(paste0(names(.), '.naive')), by = c("date" = "date.naive")) %>%
  mutate(
    across(starts_with("predicted."), roll, .names = "{.col}.mavg"),
    # rolling average of CI - probably not correct!
    across(starts_with("low."), roll, .names = "{.col}.mavg"),
    across(starts_with("high."), roll, .names = "{.col}.mavg")
  )

# plots

plt_delay_dist <-
  ggplot(nowcast_input, aes(x=delay, y=..prop..)) +
  stat_count() +
  scale_y_continuous(labels = scales::percent) +
  xlab("Delay (days)") +
  ylab("")

plt_delay_dist
plt_rdate <- ts_report %>%
  ggplot() +
  geom_bar(aes(x = date, y = n), fill = colors[2], stat = "identity") +
  geom_line(aes(x = date, y = mavg)) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  ggtitle("FHI Github etter registreringsdato")

plt_msis_rdate <- msis %>%
  ggplot() +
  geom_bar(aes(x = date, y = newReportValue),
           fill = colors[2],
           stat = "identity") +
  geom_line(aes(x = date, y = reportMavg)) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  ggtitle("MSIS skrapet daglig, etter registreringsdato")


plt_tdate_nowcast_hh <-
  ts_test_with_nc %>%
  ggplot() +
  geom_bar(aes(x = date, y = n), fill = colors[1], stat = "identity") +
  geom_line(aes(x = date, y = mavg), color = "black") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  ggtitle("FHI Github etter prøvedato", subtitle = "med nowcast (HH)") +
  geom_col(aes(x = date, y = predicted.hh), alpha = 0.3) +
  geom_errorbar(aes(x = date, ymin = low.hh, ymax = high.hh)) +
  geom_line(aes(x = date, y = predicted.hh.mavg),
            color = "indianred3",
            linetype = 'longdash') +
  geom_ribbon(
    fill = "indianred3",
    aes(x = date, ymin = low.hh.mavg, ymax = high.hh.mavg),
    alpha = 0.3
  )

plt_tdate_nowcast_nobbs <-
  ts_test_with_nc %>%
  ggplot() +
  geom_bar(aes(x = date, y = n), fill = colors[1], stat = "identity") +
  geom_line(aes(x = date, y = mavg)) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  ggtitle("FHI Github etter prøvedato", subtitle = "med nowcast (NobBS)") +
  geom_col(aes(x = date, y = predicted.nobbs), alpha = 0.3) +
  geom_errorbar(aes(x = date, ymin = low.nobbs, ymax = high.nobbs)) +
  geom_line(aes(x = date, y = predicted.nobbs.mavg),
            color = "indianred3",
            linetype = 'longdash') +
  geom_ribbon(
    fill = "indianred3",
    aes(x = date, ymin = low.nobbs.mavg, ymax = high.nobbs.mavg),
    alpha = 0.3
  )

plt_tdate_nowcast_weights <-
  ts_test_with_nc %>%
  ggplot() +
  geom_bar(aes(x = date, y = n), fill = colors[1], stat = "identity") +
  geom_line(aes(x = date, y = mavg)) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d") +
  theme_minimal() +
  ggtitle("FHI Github etter prøvedato", subtitle = "med nowcast (naiv vekting)") +
  geom_col(aes(x = date, y = predicted.naive), alpha = 0.3) +
  geom_errorbar(aes(x = date, ymin = low.naive, ymax = high.naive)) +
  geom_line(aes(x = date, y = predicted.naive.mavg),
            color = "indianred3",
            linetype = 'longdash') +
  geom_ribbon(
    fill = "indianred3",
    aes(x = date, ymin = low.naive.mavg, ymax = high.naive.mavg),
    alpha = 0.3
  )

grid.arrange(
  plt_rdate,
  plt_msis_rdate,
  plt_tdate_nowcast_hh,
  plt_tdate_nowcast_nobbs,
  plt_tdate_nowcast_weights,
  nrow = 5
)

