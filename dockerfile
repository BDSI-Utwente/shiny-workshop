FROM rocker/shiny-verse

COPY renv.lock .
RUN R -e "install.packages('pak')"
RUN R -e "pak::pak('DALEX')"