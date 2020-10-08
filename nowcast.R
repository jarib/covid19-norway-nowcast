require("surveillance")
require("dplyr")

no_data <- read.csv("./nowcast-input.tsv", sep = "\t")
no_data$reportDate <- as.Date(no_data$reportDate)
no_data$testDate <- as.Date(no_data$testDate)

now <- as.Date("2020-10-05")
when <- seq(now, length.out = 10, by = "-1 day")

N <- 40
lastNDays = no_data[no_data$reportDate > (now - N), ]

nc <- nowcast(
  now = now,
  when = when,
  dEventCol = "testDate",
  dReportCol = "reportDate",
  data = lastNDays,
  D = 10,
  m = 20,
  method = "bayes.trunc",
  control = list(
    N.tInf.prior = structure("poisgamma",
                             mean.lambda = 50,
                             var.lambda = 300),
    nSamples = 1e2
  )
)

plot(
  nc,
  xaxis.tickFreq = list("%d" = atChange, "%m" = atChange),
  xaxis.labelFreq = list("%d" = at2ndChange),
  xaxis.labelFormat = "%d-%b",
  legend.opts = NULL,
  xlab = "Time (days)",
  lty = c(1, 1, 1, 1),
)
