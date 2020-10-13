FROM rstudio/plumber

RUN R -e "install.packages(c('plumber', 'httpuv', 'surveillance', 'tidyverse'))"
RUN apt-get install jags -y
RUN R -e "install.packages(c('runjags', 'NobBS'))"

COPY nowcasts.R /app/nowcasts.R
COPY api.R /app/api.R

CMD ["/app/api.R"]