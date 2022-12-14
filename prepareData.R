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
data_2018 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2018_kandidierende/download/gemeinderatswahlen_2018_kandidierende.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2018) %>% rename(Nachname = X.U.FEFF.Nachname)
data_2014 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2014-alle-kandidierenden/download/GRW-2014-alle-Kandidierenden-OGD.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2014)
data_2010 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2010_kandidierende/download/gemeinderatswahlen_2010_kandidierende.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2010)
# data_2006 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2006-alle-kandidierenden/download/GRW-2006-alle-Kandidierenden-OGD.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2006)

df <- data_2022 %>% 
  bind_rows(data_2018, data_2014, data_2010) %>% 
  mutate(G = case_when(
    G == "M" ~ "Männlich",
    G == "W" ~ "Weiblich"
  )) %>% 
  rename(Geschlecht = G) %>% 
  mutate(ListeBezeichnung = trimws(ListeBezeichnung),
         Vorname = trimws(Vorname),
         Nachname = trimws(Nachname),
         Wahlkreis = trimws(Wahlkreis)) %>% 
  mutate(ListeBezeichnung = case_when(
    ListeKurzbez == "EVP/BDP" & Wahljahr == 2018 ~ "EVP/BDP, Evangelische Volkspartei und Bürgerlich-Demokratische Partei",
    TRUE ~ ListeBezeichnung
  ))

rm(data_2022, data_2018, data_2014, data_2010)


### Load Data
# df with information about result of election
data22 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2022_resultate/download/GRW_2022_resultate_kandidierende_und_herkunft_der_stimmen.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2022)
data18 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2018_resultate/download/GRW_2018_resultate_und_herkunft_der_stimmen.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2018)
data14 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2014-resultate/download/GRW_2014_Resultate_und_Herkunft_der_Stimmen_Nachzahlung_v2.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2014)
data10 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2010_resultate/download/GRW_2010_resultate_kandidierende_und_herkunft_der_stimmen.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2010) %>% rename(Liste_Bez_lang = X.U.FEFF.Liste, Wahlresultat = Wahlergebnis)
# data06 <- read.csv("https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2006-alle-kandidierenden/download/GRW-2006-alle-Kandidierenden-OGD.csv", encoding = "UTF-8") %>% mutate(Wahljahr = 2006)

data10$X.U.FEFF.Liste
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
df10 <- data_prep(data10) %>% 
  mutate(ListeBezeichnung = gsub(".*– ", "", ListeBezeichnung))# Was passiert mit Wahlresultat = "rückt nach"?

df_det <- df22 %>% 
  bind_rows(df18, df14, df10) %>% 
  mutate(ListeBezeichnung = trimws(ListeBezeichnung),
         Vorname = trimws(Vorname),
         Nachname = trimws(Nachname),
         Wahlkreis = trimws(Wahlkreis)) 


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
  mutate(WahlkreisSort = case_when(
    Wahlkreis == "Kreis 1 + 2" ~ 1,
    Wahlkreis == "Kreis 3" ~ 2,
    Wahlkreis == "Kreis 4 + 5" ~ 3,
    Wahlkreis == "Kreis 6" ~ 4,
    Wahlkreis == "Kreis 7 + 8" ~ 5,
    Wahlkreis == "Kreis 9" ~ 6,
    Wahlkreis == "Kreis 10" ~ 7,
    Wahlkreis == "Kreis 11" ~ 8,
    Wahlkreis == "Kreis 12" ~ 9
  )) %>% 
  arrange(Wahljahr, Wahlkreis) %>% 
  mutate(Wahlresultat = case_when(
    Wahlresultat == "nicht gewaehlt" ~ "nicht gewählt",
    Wahlresultat == "gewaehlt" ~ "gewählt",
    TRUE ~Wahlresultat
  )) %>% 
  mutate(Alter = Wahljahr - GebJ)

data_cand <- data %>% 
  select(-total_stim, -part_eig_stim, -part_eig_stim_unv_wl, -part_frmd_stim, -Var, -Value) %>% 
  distinct()

# matched <- data %>%  group_by(Wahljahr, ListeKurzbez) %>% mutate(nomatch = sum(is.na(total_stim)), count = n()) %>%  select(Wahljahr, ListeKurzbez, count, nomatch) %>% unique()
# ss <- data %>% filter(Wahljahr == 2018 & is.na(total_stim)) 
