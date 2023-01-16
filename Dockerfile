FROM rocker/tidyverse:4.2.1
RUN install2.r rsconnect shiny shinyjs lubridate remotes htmltools Rcpp openxlsx readxl reactable
RUN Rscript -e "remotes::install_github('StatistikStadtZuerich/zuericssstyle')"
WORKDIR /home/kali
COPY app.R app.R
COPY R/ssz_download_excel.R R/ssz_download_excel.R
COPY dependencies.R dependencies.R
COPY www/script.js www/script.js
COPY R/get_reactables_candidates.R R/get_reactables_candidates.R
COPY logo_stzh_stat_sw_pos_1.png logo_stzh_stat_sw_pos_1.png
COPY R/get_data.R R/get_data.R
COPY sszThemeShiny.css sszThemeShiny.css
COPY Titelblatt.xlsx Titelblatt.xlsx
COPY deploy.R deploy.R
CMD Rscript deploy.R