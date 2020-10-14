source("nowcasts.R")
source("utils.R")

prepare_data <- function(input) {
  input %>%
    mutate(
      reportDate = as.Date(reportDate),
      testDate = as.Date(testDate)
    )
}

#' Nowcast hh
#' @post /nowcast/hh
#' @serializer unboxedJSON
#'
nowcast_data_hh <-
  function(dates) {
    input <- prepare_data(dates)
    list(nowcast = nowcast_hh(input))
  }

#' Nowcast nobbs
#' @post /nowcast/nobbs
#' @serializer unboxedJSON
#'
nowcast_data_nobbs <-
  function(dates) {
    input <- prepare_data(dates)
    list(nowcast = nowcast_nobbs(input))
  }


#' Nowcast HH plot
#' @post /nowcast/hh/plot
#' @png (width=1000, height=400)
#'
nowcast_hh_plot <-
  function(dates, days=60) {
    data <- prepare_data(dates)
    nc <- nowcast_hh(data)

    data %>%
      count_by_date(testDate) %>%
      tail(days) %>% 
      combine_nowcast(nc) %>%
      plot_nowcast() %>% 
      print()
  }

#' Nowcast NoBBS plot
#' @post /nowcast/nobbs/plot
#' @png (width=1000, height=400)
#'
nowcast_nobbs_plot <-
  function(dates, days=60) {
    data <- prepare_data(dates)
    nc <- nowcast_nobbs(data)

    data %>%
      count_by_date(testDate) %>%
      tail(days) %>% 
      combine_nowcast(nc) %>%
      plot_nowcast() + ggtitle("NobBS nowcast") %>%
      print()
  }
