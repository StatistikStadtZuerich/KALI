FROM rocker/tidyverse:4.2.1
RUN install2.r rsconnect shiny shinyjs lubridate remotes htmltools Rcpp openxlsx readxl reactable fst
RUN Rscript -e "remotes::install_github('StatistikStadtZuerich/zuericssstyle')"
RUN Rscript -e "remotes::install_github('mitchelloharawild/icons')"
WORKDIR /home/kali
COPY app.R app.R
COPY R/ssz_download_excel.R R/ssz_download_excel.R
COPY R/dependencies.R R/dependencies.R
COPY www/script.js www/script.js
COPY R/get_reactables_candidates.R R/get_reactables_candidates.R
COPY www/logo_stzh_stat_sw_pos_1.png www/logo_stzh_stat_sw_pos_1.png
COPY data/data_KALI.fst data/data_KALI.fst
COPY www/sszThemeShiny.css www/sszThemeShiny.css
COPY www/KALI.css www/KALI.css
COPY www/Titelblatt.xlsx www/Titelblatt.xlsx
COPY www/icons/download.svg www/icons/download.svg
COPY www/icons/external-link.svg www/icons/external-link.svg
COPY deploy.R deploy.R
CMD Rscript deploy.R