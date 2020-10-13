source("./nowcasts.R")

prepare_data <- function(input) {
  input %>%
    mutate(reportDate = as.Date(reportDate),
           testDate = as.Date(testDate))
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
#' @serializer png
#'
nowcast_hh_plot <-
  function(dates) {
    nc <- nowcast_hh(prepare_data(dates), raw=TRUE)

    plot(
      nc,
      xaxis.tickFreq = list("%d" = atChange, "%m" = atChange),
      xaxis.labelFreq = list("%d" = at2ndChange),
      xaxis.labelFormat = "%d-%b",
      legend.opts = NULL,
      xlab = "Time (days)",
      lty = c(1, 1, 1, 1)
    )
  }

