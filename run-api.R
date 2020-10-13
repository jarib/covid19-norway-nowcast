library(plumber)

pr("api.R") %>%
  pr_run(port=8000)