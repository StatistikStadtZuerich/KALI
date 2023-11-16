# Kandidierndenlisten Gemeinderatswahlen (KALI)
Shiny app for KALI Tool

The KALI application on the [website of Statistik Stadt Zürich](https://www.stadt-zuerich.ch/prd/de/index/statistik/publikationen-angebote/datenbanken-anwendungen/kandidierendenliste.html) shows the results of all elections for the municipal council of Zurich since 2010.

The data is obtained from the Open Data portal of the city of Zurich and is available [here](https://data.stadt-zuerich.ch/dataset?q=Kandidierende&sort=score+desc%2C+date_last_modified+desc).

# Architektur
```mermaid
flowchart LR;
  f1[F1 select name]:::filter --> button[button start]:::button
  f2[F2 select gender]:::filter --> button
  f3[F3 select year of elections]:::filter --> button
  f4[F4 select electoral district]:::filter --> button
  f5[F5 select party list]:::filter --> button
  f6[F6 select electoral status]:::filter --> button
  button --> output1[(filtered data = \nF1 + F2 + F3 + F4 + F5 + F6)]:::data
  output1 --> results1[[Resultat candidates]]:::result
  results1 --> f7[F7 select candidate]:::filter
  f7 --> output2[(output1 + F7)]:::data
  output2 --> results2[["Result candidate \n(show and display \ndetails on votes \nreceived)"]]:::result
  output2 --> downloads{Downloads}:::download
  
  classDef filter fill:#ffff2f,stroke:#ffff2f,color:#000000;
  classDef button fill:#695eff,stroke:#695eff,color:#000000;
  classDef data fill:#edade6,stroke:#acb0b0,color:#000000;
  classDef result fill:#59e6f0,stroke:#acb0b0,color:#000000;
  classDef download fill:#43cc4c,stroke:#43cc4c,color:#000000;
```

