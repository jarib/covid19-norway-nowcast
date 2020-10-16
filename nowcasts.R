library(tidyverse)
library(surveillance)
library(NobBS)

config.window <- 21
config.maxDelay <- 21
config.predictDays <- 21

config.naive.weights <- c(1.1462158912326739, 1.48626580621125, 6.043134426689718)

# https://staff.math.su.se/hoehle/blog/2016/07/19/nowCast.html
nowcast_hh <- function(data, raw = FALSE) {
  method = 'bayes.trunc'

  sts <- linelist2sts(data,
                      dateCol = "testDate",
                      aggregate.by = "1 day")

  when <-
    data$testDate %>%
    unique() %>%
    sort() %>%
    tail(config.predictDays)

  now <- when %>% tail(1)

  delays <- data %>%
    filter(testDate > (now - config.window)) %>%
    mutate(delay = as.numeric(reportDate - testDate)) %>%
    summarise(
      mean = mean(delay),
      variance = var(delay),
      min = min(delay),
      max = max(delay)
    )

  if (delays$min < 0) {
    stop("negative delay in input")
  }

  message(glue::glue("delay: mean={delays$mean},variance={delays$variance}"))

  nc <- nowcast(
    now = now,
    when = when,
    dEventCol = "testDate",
    dReportCol = "reportDate",
    data = data,
    D = config.maxDelay,
    m = config.window,
    method = method,
    control = list(
      N.tInf.max = 1000,
      N.tInf.prior = structure(
        "poisgamma",

        # "These data are by definition of the problem incomplete.
        # As a dirty fix we therefore just inflate the prior variance
        # by a factor - as future work this needs to be improved upon
        # by following a proper marginal likelihood approach."
        mean.lambda = 0.8 * mean(observed(sts)),
        var.lambda = 5 * var(observed(sts))
      ),
      nSamples = 1000,
      score = TRUE,
      predPMF = TRUE
    )
  )

  if (raw) {
    return(nc)
  }

  nc %>%
    as_tibble() %>%
    # Add prediction interval
    mutate(low = nc@pi[, , 1],  high =  nc@pi[, , 2]) %>%
    # Return only time points which were nowcasted.
    filter(!is.na(upperbound)) %>%
    select(date = epoch, observed, low, high, predicted = upperbound) %>%
    mutate(obnyr = predicted - observed,
           completeness = observed / predicted)

}


# https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1007735#sec009
nowcast_nobbs <- function(data, raw=FALSE) {
  df <- data.frame(data)

  nc <- NobBS(
    data = df,
    now = max(df$testDate),
    units = "1 day",
    onset_date = "testDate",
    report_date = "reportDate",
    moving_window = config.window,
    quiet = FALSE,
    specs = list(dist = "NB")
  )

  if (raw) {
    return(nc)
  }

  nc$estimates %>%
    as_tibble() %>%
    rename(
      date = onset_date,
      observed = n.reported,
      low = lower,
      high = upper,
      predicted = estimate
    ) %>%
    mutate(completeness = observed / predicted,
           obnyr = predicted - observed)
}

nowcast_weights <- function(data) {
  counts <- data %>%
    as_tibble() %>%
    rename(date = testDate) %>%
    count(date, name = "observed")

  counts$weight <-
    do.call(c, list(rep(1, nrow(counts) - length(config.naive.weights)), config.naive.weights))

  counts %>% mutate(
    predicted = round(observed * weight),
    low = predicted,
    high = predicted,
    completeness = observed / predicted
  ) %>%
    tail(config.predictDays)
}
