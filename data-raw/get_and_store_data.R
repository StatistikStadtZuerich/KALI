library(tidyverse)
library(httr)
library(data.table)
library(lubridate)
library(furrr)
library(fst)

get_data <- function() {
  
  ### Candidates
  ## Years
  years <- c(2022, 2018, 2014, 2010)
  
  ## URLs
  URLs_cand <- c(
    "https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2022_kandidierende/download/gemeinderatswahlen_2022_kandidierende.csv",
    "https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2018_kandidierende/download/gemeinderatswahlen_2018_kandidierende.csv",
    "https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2014-alle-kandidierenden/download/GRW-2014-alle-Kandidierenden-OGD.csv",
    "https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2010_kandidierende/download/gemeinderatswahlen_2010_kandidierende.csv"
  )
  
  ## Function to download the data from Open Data Zürich
  data_download <- function(link, year) {
    data <- data.table::fread(link,
                              encoding = "UTF-8") %>% 
      mutate(Wahljahr = year)
  }
  
  ## Download and Rename and wrangle as required
  data_cand <- furrr::future_map2_dfr(URLs_cand, years, data_download) %>% 
    mutate(G = case_when(
      G == "M" ~ "Männlich",
      G == "W" ~ "Weiblich"
    )) %>% 
    rename(Geschlecht = G) %>% 
    mutate(ListeBezeichnung = trimws(ListeBezeichnung),
           Vorname = trimws(Vorname),
           Nachname = trimws(Nachname),
           Wahlkreis = trimws(Wahlkreis)) %>% 
    select(-A, -Kand, -Liste) %>%
    mutate(
      Name = paste(Vorname, Nachname, sep = " "),
      Wahlkreis = paste("Kreis", Wahlkreis, sep = " ")
    ) %>% 
    select(-Vorname, -Nachname) 
  
  ### Results
  URLs_result <- c(
    "https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2022_resultate/download/GRW_2022_resultate_kandidierende_und_herkunft_der_stimmen.csv",
    "https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2018_resultate/download/GRW_2018_resultate_und_herkunft_der_stimmen.csv",
    "https://data.stadt-zuerich.ch/dataset/politik-gemeinderatswahlen-2014-resultate/download/GRW_2014_Resultate_und_Herkunft_der_Stimmen_Nachzahlung_v2.csv"
  )
  
  ## Function to make data long to wide, and wrangle data
  data_prep <- function(data){
    data %>% 
      select(Wahljahr, Liste_Bez_lang, Wahlkreis, Nachname, Vorname, 
             Wahlresultat, total_stim, starts_with("part"), starts_with("stim")) %>% 
      gather(Var, Value, -Wahljahr, -Liste_Bez_lang, -Wahlkreis, -Nachname, 
             -Vorname, -Wahlresultat, -total_stim, -starts_with("part")) %>% 
      mutate(Value = as.numeric(Value)) %>% 
      rename(ListeBezeichnung = Liste_Bez_lang) %>% 
      mutate(StimmeVeraeListe = str_remove_all(Var, "stim_verae_wl_")) %>% 
      mutate(Wahlresultat = stringr::str_replace(Wahlresultat, "ae", "ä")) %>% 
      mutate(ListeBezeichnung = trimws(ListeBezeichnung),
             Vorname = trimws(Vorname),
             Nachname = trimws(Nachname),
             Wahlkreis = trimws(Wahlkreis)) %>%
      mutate(
        Name = paste(Vorname, Nachname, sep = " "),
        Wahlkreis = paste("Kreis", Wahlkreis, sep = " ")
      ) %>% 
      select(-Vorname, -Nachname) %>% 
      mutate(StimmeVeraeListe = case_when(
        StimmeVeraeListe == "Gruene" ~ "Grüne",
        StimmeVeraeListe == "evp_bdp" ~ "EVP/BDP",
        StimmeVeraeListe == "DieMitte" ~ "Die Mitte",
        StimmeVeraeListe == "ILoveZH" ~ "I Love ZH",
        TRUE ~ StimmeVeraeListe
      )) %>% 
      mutate(
        `Anteil Stimmen aus veränderten Listen` = as.character(
          round(100*(1 - (part_eig_stim_unv_wl/total_stim)), 1))
      ) %>% 
      mutate(
        `Anteil Stimmen aus veränderten Listen` = paste(`Anteil Stimmen aus veränderten Listen`, 
                                                        "%", sep = " ")
      ) %>% 
      rename(`Anzahl Stimmen` = total_stim,
             `Parteieigene Stimmen` = part_eig_stim,
             `Parteifremde Stimmen` = part_frmd_stim)
  }
  
  ## Function to download the data from Open Data Zürich
  data_download_prep <- function(link, year) {
    data_download(link, year) %>% 
      data_prep()
  }
  
  ## Parallelisation Download (3 out of 4 Datasets)
  data_result_22_14 <- furrr::future_map2_dfr(URLs_result, years[1:3], data_download_prep)
  
  # Separate Download as Columns have different names
  data10 <- data.table::fread(
    "https://data.stadt-zuerich.ch/dataset/politik_gemeinderatswahlen_2010_resultate/download/GRW_2010_resultate_kandidierende_und_herkunft_der_stimmen.csv", 
    encoding = "UTF-8") %>% 
    mutate(Wahljahr = 2010) %>% 
    rename(Liste_Bez_lang = Liste, Wahlresultat = Wahlergebnis) %>% 
    data_prep() %>% 
    mutate(ListeBezeichnung = gsub(".*– ", "", ListeBezeichnung))
  
  # Combine Downloads
  df_details <- bind_rows(data_result_22_14, data10) 
  
  rm(data_result_22_14, data10)
  
  ### Data Manipulation
  
  # to avoid duplicates in main table, just select a subset to join
  df_details_for_join <- df_details %>% 
    select(Name, Wahljahr, Wahlkreis, ListeBezeichnung, Wahlresultat,
           `Anzahl Stimmen`, `Parteieigene Stimmen`, 
           `Parteifremde Stimmen`,
           `Anteil Stimmen aus veränderten Listen`) %>% 
    unique()
  
  df_main <- data_cand %>%
    left_join(df_details_for_join, by = c("Wahljahr", 
                             "Name",
                             "Wahlkreis", 
                             "ListeBezeichnung")) %>% 
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
    arrange(Wahljahr, WahlkreisSort) %>% 
    mutate(Alter = Wahljahr - GebJ)   %>% 
    rename(Liste = ListeKurzbez) 
  
  # with updated data (2026): check if this joined df has NA, in that case
  # there is probably a mismatch in names or someone missing
  # currently known missing results: Jürg Nef, EVP, 2018
  
  return(list("df_main" = df_main, "df_details" = df_details))
}

data <- get_data()

write.fst(data[["df_main"]], here::here("data", "data_KALI_main.fst"))
write.fst(data[["df_details"]], here::here("data", "data_KALI_details.fst"))

