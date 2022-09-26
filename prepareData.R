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

## Join data
# df has information about candidates (incumbent status is only in this df)
data_2022 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2022_kandidierende/download/gemeinderatswahlen_2022_kandidierende.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2022) %>% rename(Nachname = X.U.FEFF.Nachname)
data_2018 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2018-alle-kandidierenden/download/GRW-2018-alle-Kandidierenden-OGD.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2018)
data_2014 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2014-alle-kandidierenden/download/GRW-2014-alle-Kandidierenden-OGD.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2014)
data_2010 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2010-alle-kandidierenden/download/GRW-2010-alle-Kandidierenden-OGD.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2010)
data_2006 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2006-alle-kandidierenden/download/GRW-2006-alle-Kandidierenden-OGD.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2006)

df <- data_2022 %>% 
  bind_rows(data_2018, data_2014, data_2010, data_2006) %>% 
  mutate(G = case_when(
    G == "M" ~ "Männlich",
    G == "W" ~ "Weiblich"
  )) %>% 
  rename(Geschlecht = G)

rm(data_2022, data_2018, data_2014, data_2010, data_2006)


### Load Data
# df with information about result of election
data22 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2022_resultate/download/GRW_2022_resultate_kandidierende_und_herkunft_der_stimmen.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2022)
data18 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2018_resultate/download/GRW_2018_Resultate_und_herkunft_der_stimmen.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2018)
data14 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2014-resultate/download/GRW_2014_Resultate_und_Herkunft_der_Stimmen_Nachzahlung_v2.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2014)
data10 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2010-resultate/download/GRW_2010_Resultate_und_Herkunft_der_Stimmen.csv", sep = ";", encoding = "UTF-8") %>% mutate(Wahljahr = 2010) %>% rename(Liste_Bez_lang = Liste, Wahlresultat = Wahlergebnis)
# data06 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2006-alle-kandidierenden/download/GRW-2006-alle-Kandidierenden-OGD.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2006)


## Function to make data long to wide
data_prep <- function(data){
  data %>% 
    select(Wahljahr, Liste_Bez_lang, Wahlkreis, Nachname, Vorname, Wahlresultat, total_stim, starts_with("part"), starts_with("stim")) %>% 
    gather(Var, Value, -Wahljahr, -Liste_Bez_lang, -Wahlkreis, -Nachname, -Vorname, -Wahlresultat, -total_stim, -starts_with("part")) %>% 
    mutate(Value = as.numeric(Value)) %>% 
    rename(ListeBezeichnung = Liste_Bez_lang)
}


df22 <- data_prep(data22)
df18 <- data_prep(data18)
df14 <- data_prep(data14)
df10 <- data_prep(data10)

df_det <- df22 %>% 
  bind_rows(df18, df14, df10)

rm(data22, data18, data14, data10, df22, df18, df14, df10)


### Join df_det with df
data <- df %>% 
  left_join(df_det, by = c("Wahljahr", "Vorname", "Nachname", "Wahlkreis", "ListeBezeichnung")) %>% 
  mutate(
    Name = paste(Vorname, Nachname, sep = " ")
  ) %>% 
  mutate(
    Wahlkreis = paste("Kreis", Wahlkreis, sep = " ")
  ) %>% 
  mutate(Alter = Wahljahr - GebJ)




