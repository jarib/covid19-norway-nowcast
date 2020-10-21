library(tidyverse)
library(gridExtra)
library(wesanderson)
library(httr)
library(zoo)
library(jsonlite)
library(animation)


source("nowcasts.R")
source("utils.R")

df <- fetch_latest_linelist()

last_date <- max(df$reportDate)
nowcast_range <- seq(last_date - 90, last_date, by = "1 day")

nowcasts_hh <- list()
nowcasts_nobbs <- list()
nowcasts_nobbs_nb <- list()
nowcasts_nobbs_poisson <- list()
nowcasts_weights <- list()

test_date_series <- df %>%
  filter(testDate >= first(nowcast_range)) %>%
  count_by_date(testDate)

for (i in 1:length(nowcast_range)) {
  current_date <- as.character(nowcast_range[i])
  date_string <- as.character(current_date)
  print(date_string)

  input_data <- df %>%
    filter(reportDate < current_date) %>%
    as_data_frame()

  nowcasts_hh[[date_string]] <- nowcast_hh(as.data.frame(input_data))
  nowcasts_nobbs_nb[[date_string]] <- nowcast_nobbs(input_data, dist = "NB")
  nowcasts_nobbs_poisson[[date_string]] <- nowcast_nobbs(input_data, dist = "Poisson")
  nowcasts_weights[[date_string]] <- nowcast_weights(input_data)
}

plot_all <- function(nowcasts, title) {
  for (i in 1:length(nowcasts)) {
    date <- as.Date(names(nowcasts[i]))
    combined <- combine_nowcast(test_date_series, nowcasts[[i]])
    plt <- plot_nowcast(combined) + ylim(0, 300) + ggtitle(title, subtitle = format(date, "%Y-%m-%d %a"))
    print(plt)
  }
}

animation::saveVideo(plot_all(nowcasts_hh, title = "nowcast: Höhle"), video.name = "nowcasts-hh.mp4", interval = 0.5, ani.width = 1100, ani.height = 800)
animation::saveVideo(plot_all(nowcasts_nobbs_nb, title = "nowcast: NobBS (nb)"), video.name = "nowcasts-nobbs-nb.mp4", interval = 0.5, ani.width = 1100, ani.height = 800)
animation::saveVideo(plot_all(nowcasts_nobbs_poisson, title = "nowcast: NobBS (poisson)"), video.name = "nowcasts-nobbs-poisson.mp4", interval = 0.5, ani.width = 1100, ani.height = 800)
animation::saveVideo(plot_all(nowcasts_weights, title = "nowcast: naive weights"), video.name = "nowcasts-weights.mp4", interval = 0.5, ani.width = 1100, ani.height = 800)

nowcast_errors <- function(nowcasts) {
  len <- length(nowcasts)
  errors <- list()

  for (i in 1:length(nowcasts)) {
    err <- combine_nowcast(test_date_series, nowcasts[[i]]) %>% 
      filter(!is.na(predicted)) %>%
      summarise(
        mae = mean(abs(cases - predicted)), 
        rmse = sqrt(mean((cases - predicted)**2)), 
        relative_rmse = sqrt(mean(((cases - predicted) / cases)**2))
      ) %>% as.list()
    
    errors[[names(nowcasts[i])]] <- err
  }
  
  errors %>% enframe() %>% unnest_wider(value) %>% rename(date = name) %>% mutate(date = as.Date(date))
}


all_errors <-
  nowcast_errors(nowcasts_hh) %>%
  left_join(nowcast_errors(nowcasts_nobbs_nb), by = c("date" = "date"), suffix = c(".hh", ".nobbs_nb")) %>%
  left_join(nowcast_errors(nowcasts_nobbs_poisson) %>% rename_with(~glue::glue("{.}.nobbs_poisson"), -date), by = c("date" = "date")) %>%
  left_join(nowcast_errors(nowcasts_weights) %>% rename_with(~glue::glue("{.}.weights"), -date), by = c("date" = "date"))

all_errors %>%
  tail(-20) %>%
  ggplot(aes(x = date)) +
  scale_x_date() +
  geom_line(aes(y = relative_rmse.hh, color = "rRMSE.hh")) +
  geom_line(aes(y = relative_rmse.nobbs_nb, color = "rRMSE.nobbs_nb")) +
  geom_line(aes(y = relative_rmse.weights, color = "rRMSE.weights")) +
  geom_line(aes(y = relative_rmse.nobbs_poisson, color = "rRMSE.nobbs_poisson")) +
  scale_color_brewer(name="Nowcast", palette = "Set1")
