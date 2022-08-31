### Required packages
packages <- c("tidyverse",
              "httr",
              "parallel",
              "data.table",
              "lubridate")

### Load packages
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

### Load Data
data_2022 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2022_kandidierende/download/gemeinderatswahlen_2022_kandidierende.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2022) %>% rename(Nachname = X.U.FEFF.Nachname)
data_2018 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2018-alle-kandidierenden/download/GRW-2018-alle-Kandidierenden-OGD.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2018)
data_2014 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2014-alle-kandidierenden/download/GRW-2014-alle-Kandidierenden-OGD.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2014)
data_2010 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2010-alle-kandidierenden/download/GRW-2010-alle-Kandidierenden-OGD.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2010)
data_2006 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2006-alle-kandidierenden/download/GRW-2006-alle-Kandidierenden-OGD.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2006)

## Join data
df <- data_2022 %>% 
  bind_rows(data_2018, data_2014, data_2010, data_2006)

rm(data_2022, data_2018, data_2014, data_2010, data_2006)

