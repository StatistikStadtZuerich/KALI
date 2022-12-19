# Load Library
library(shinyjs)
library(magrittr)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)
library(reactable)
library(icons)
library(shiny)
library(htmltools)
library(zuericssstyle)
#get_shiny_css()

# Source Prepared Data
source("prepareData.R", encoding = "UTF-8")

# Source Export Excel
source("exportExcel.R", encoding = "UTF-8")

# Source Dependencies
source("dependencies.R", encoding = "UTF-8")

dependencies <- getDependencies()
chart_container <- tags$div(id="sszvis-chart")

# Set the Icon path
icon <- icon_set("icons/")


# Define UI
ui <- fluidPage(
    
    # Include CSS
    includeCSS("sszThemeShiny.css"),
    
    # To conveniently disable select element from the server side.
    useShinyjs(),
    dependencies,
    
    # Application Title 
    titlePanel("Kandidierendenlisten App"),
    
    # Sidebar: Input widgets are placed here
    sidebarLayout(
        sidebarPanel(
            
            # Example textInput()
            sszTextInput("suchfeld", "Name:"),
            
        
            # Example radioButtons() vertical
            sszRadioButtons("ButtonGroupLabel",
                            label = "Geschlecht:",
                            choices = c("Alle", "Männlich", "Weiblich"),
                            selected = "Alle" # default value
                            ),
            
            # Example selectInput()
            sszSelectInput("select", "Gemeinderatswahlen:", 
                        choices = unique(data$Wahljahr)),
            
            # Example selectInput()
            sszSelectInput("select2", "Wahlkreis:", 
                        choices = c("Ganz Stadt", "Kreis 1 + 2", "Kreis 3", "Kreis 4 + 5", "Kreis 6",
                                    "Kreis 7 + 8", "Kreis 9", "Kreis 10", "Kreis 11", "Kreis 12"),
                        selected = "Ganz Stadt"),
            
            
            conditionalPanel(
                condition = 'input.select == "2022"',
                
                # Example selectInput()
                sszSelectInput("select31", "Liste:", 
                            choices = c("Alle Listen", unique(data[data$Wahljahr == 2022,]$ListeBezeichnung)),
                            selected = "Alle Listen"),
            ),
            conditionalPanel(
                condition = 'input.select == "2018"',
                
                # Example selectInput()
                sszSelectInput("select32", "Liste:", 
                            choices = c("Alle Listen", unique(data[data$Wahljahr == 2018,]$ListeBezeichnung)),
                            selected = "Alle Listen"),
            ),
            conditionalPanel(
                condition = 'input.select == "2014"',

                # Example selectInput()
                sszSelectInput("select33", "Liste:", 
                            choices = c("Alle Listen", unique(data[data$Wahljahr == 2014,]$ListeBezeichnung)),
                            selected = "Alle Listen"),
            ),
            conditionalPanel(
                condition = 'input.select == "2010"',

                # Example selectInput()
                sszSelectInput("select34", "Liste:", 
                            choices = c("Alle Listen", unique(data[data$Wahljahr == 2010,]$ListeBezeichnung)),
                            selected = "Alle Listen"),
                
            ),
            
            # Example radioButtons() vertical
            sszRadioButtons("ButtonGroupLabel2",
                            label = "Status:",
                            choices = c("Alle", "gewählt", "nicht gewählt"),
                            selected = "Alle" 
                            ),
            
            
            # Action Button
            conditionalPanel(
                condition = 'input.ActionButtonId==0',
                
                sszActionButton("ActionButtonId",
                                "Abfrage starten")
            ),
            conditionalPanel(
                condition = 'input.ActionButtonId>0',
                
            ),
            
            # Downloads
            conditionalPanel(
                condition = 'output.tableCand',
                h3("Daten herunterladen"),
                
                # Download Panel
                tags$div(
                    id = "downloadWrapperId",
                    class = "downloadWrapperDiv",
                    
                    sszDownload("csvDownload",
                                label = "csv"
                    ),
                    sszDownload("excelDownload",
                                label = "xlsx"
                    ),
                    sszOgdDownload(inputId = "ogdDown",
                                   label = "OGD",
                                   onclick ="window.open('https://data.stadt-zuerich.ch/dataset?q=Kandidierende&sort=score+desc%2C+date_last_modified+desc', '_blank')"
                    )
                )
            )
        ),
        
        
        # Mail Panel: Outputs are placed here
        mainPanel(
            
            conditionalPanel(
                condition = 'input.ActionButtonId>0',
                
                # Title for table
                h1("Die untenstehenden Vorlagen entsprechen Ihren Suchkriterien"),
                hr(),
                # Define subtitle
                tags$div(
                    class = "infoDiv",
                    p("Für Detailinformationen zur Stimmbeteiligung und zum Ergebnis einer Abstimmung wählen Sie eine Zeile aus.")
                )
            ),
            conditionalPanel(
                condition = 'input.ActionButtonId==0',
                
            ),
            
            # Example Table Output 
            reactableOutput("table"),
            
            # Name of selected candidate
            htmlOutput("nameCandidate"),
            
            reactableOutput("tableCand"),
            
            
            # Only show plot if tableCand is also shown
            conditionalPanel(
              condition = 'output.tableCand',
            
              # Name of selected candidate
              # Title for table
              hr(),
              h4("Stimmen aus veränderten Listen"),
              hr(),
              
              chart_container
              
            )

        )
    )
)


# Server function
server <- function(input, output, session) {
    
    # First button click to activate search, after not necessary anymore
    global <- reactiveValues(activeButton = FALSE)
    
    observeEvent(input$ActionButtonId, {
        req(input$ActionButtonId)
        global$activeButton <- TRUE
    })
    
    # send updated data to json for D3 chart
    update_data <- function(data) {
        session$sendCustomMessage(
            type="update_data",
            message=jsonlite::toJSON(data)
        )
    }
    
    # Filter data according to inputs
    filteredData <- reactive({
        
        # Filter: No Search
        if(input$select == "2022") {
            filtered <- data %>%
                filter(Wahljahr == input$select) %>%
                filter(if(input$suchfeld != "") grepl(input$suchfeld, Name, ignore.case=TRUE) else TRUE) %>%
                filter(if(input$ButtonGroupLabel != "Alle") Geschlecht == input$ButtonGroupLabel else TRUE) %>%
                filter(if(input$select2 != "Ganz Stadt") Wahlkreis == input$select2 else TRUE)  %>%
                filter(if(input$select31 != "Alle Listen") ListeBezeichnung == input$select31 else TRUE) %>%
                filter(if(input$ButtonGroupLabel2 != "Alle") Wahlresultat == input$ButtonGroupLabel2 else TRUE)
            filtered
            
                # Filter: With Search
            } else if(input$select == "2018") {
                filtered <- data %>%
                    filter(Wahljahr == input$select) %>%
                    filter(if(input$suchfeld != "") grepl(input$suchfeld, Name, ignore.case=TRUE) else TRUE) %>%
                    filter(if(input$ButtonGroupLabel != "Alle") Geschlecht == input$ButtonGroupLabel else TRUE) %>%
                    filter(if(input$select2 != "Ganz Stadt") Wahlkreis == input$select2 else TRUE)  %>%
                    filter(if(input$select32 != "Alle Listen") ListeBezeichnung == input$select32 else TRUE) %>%
                    filter(if(input$ButtonGroupLabel2 != "Alle") Wahlresultat == input$ButtonGroupLabel2 else TRUE)
                filtered
            
            } else if(input$select == "2014") {
                filtered <- data %>%
                    filter(Wahljahr == input$select) %>%
                    filter(if(input$suchfeld != "") grepl(input$suchfeld, Name, ignore.case=TRUE) else TRUE) %>%
                    filter(if(input$ButtonGroupLabel != "Alle") Geschlecht == input$ButtonGroupLabel else TRUE) %>%
                    filter(if(input$select2 != "Ganz Stadt") Wahlkreis == input$select2 else TRUE)  %>%
                    filter(if(input$select33 != "Alle Listen") ListeBezeichnung == input$select33 else TRUE) %>%
                    filter(if(input$ButtonGroupLabel2 != "Alle") Wahlresultat == input$ButtonGroupLabel2 else TRUE)
                filtered
            
            } else if(input$select == "2010") {
                filtered <- data %>%
                    filter(Wahljahr == input$select) %>%
                    filter(if(input$suchfeld != "") grepl(input$suchfeld, Name, ignore.case=TRUE) else TRUE) %>%
                    filter(if(input$ButtonGroupLabel != "Alle") Geschlecht == input$ButtonGroupLabel else TRUE) %>%
                    filter(if(input$select2 != "Ganz Stadt") Wahlkreis == input$select2 else TRUE)  %>%
                    filter(if(input$select34 != "Alle Listen") ListeBezeichnung == input$select34 else TRUE) %>%
                    filter(if(input$ButtonGroupLabel2 != "Alle") Wahlresultat == input$ButtonGroupLabel2 else TRUE)
                filtered
            
            }
    }) 
    # %>%
    #     # necessary to get reactive data in D3 chart
    #     bindCache(input$ActionButtonId, input$select, input$suchfeld, input$ButtonGroupLabel,
    #               input$select2, input$select31, input$select32, input$select33,
    #               input$select34, input$ButtonGroupLabel2) %>%
    #     bindEvent(input$ActionButtonId, input$select, input$suchfeld, input$ButtonGroupLabel,
    #               input$select2, input$select31, input$select32, input$select33,
    #               input$select34, input$ButtonGroupLabel2)
    
    
    # Reactable Output
    output$table <- renderReactable({
        
        req(global$activeButton == TRUE)
        
        tableOutput <- reactable(filteredData() %>%
                                     select(Name, Alter, Geschlecht, Beruf, Wahlkreis, Liste) %>% 
                                     unique()
                                  ,
                                  paginationType = "simple",
                                  language = reactableLang(
                                      noData = "Keine Einträge gefunden",
                                      pageNumbers = "{page} von {pages}",
                                      pageInfo = "{rowStart} bis {rowEnd} von {rows} Einträgen",
                                      pagePrevious = "\u276e",
                                      pageNext = "\u276f",
                                      pagePreviousLabel = "Vorherige Seite",
                                      pageNextLabel = "Nächste Seite"
                                      
                                  ),
                                  theme = reactableTheme(
                                      borderColor = "#DEDEDE"
                                  ),
                                 defaultColDef = colDef(
                                     align = "left",
                                     minWidth = 50
                                 ),
                                  outlined = TRUE,
                                  highlight = TRUE,
                                  defaultPageSize = 5,
                                  onClick = "select",
                                  selection = "single",
                                  rowClass = JS("function(rowInfo) {return rowInfo.selected ? 'selected' : ''}"),
                                  rowStyle = JS("function(rowInfo) {if (rowInfo.selected) { return { backgroundColor: '#F2F2F2'}}}")
        )
        tableOutput
    })
    
    # Prepare data for second Output
    rowNumber <- reactive({
        getReactableState("table", "selected")
    })
    
    
    dataPerson <- reactive({
        req(nrow(filteredData())>0)
        req(rowNumber())
        
        person <- filteredData() %>%
            select(Name, Wahlkreis, ListeBezeichnung, Wahlresultat, 
                   `Anzahl Stimmen`, `Parteieigene Stimmen`, `Parteifremde Stimmen`,
                   `Anteil Stimmen aus veränderten Listen`) %>%
            unique() %>%
            mutate(ID = row_number()) %>%
            filter(ID == rowNumber()) %>% 
            select(-ID)
        person

    })
    
    dataDownload <- reactive({
        req(nrow(filteredData())>0)
        req(rowNumber())
        
        person <- filteredData() %>%
            select(Wahljahr, Name, Alter, Geschlecht, Beruf, Wahlkreis, Liste, Wahlresultat, 
                   `Anzahl Stimmen`, `Parteieigene Stimmen`, `Parteifremde Stimmen`,
                   `Anteil Stimmen aus veränderten Listen`) %>%
            unique() %>%
            mutate(ID = row_number()) %>%
            filter(ID == rowNumber()) %>% 
            select(-ID) %>% 
            gather(`Result der Wahl`, Wert, -Wahljahr, -Name, -Alter, -Geschlecht, 
                   -Beruf, -Wahlkreis, -Liste)
        person
        
    })
    
    namePerson <- reactive({
        req(nrow(dataPerson())>0)
        
        person <- dataPerson()
        
        print(person$Name)
    })
    
    nameWahlkreis <- reactive({
        req(nrow(dataPerson())>0)
        
        person <- dataPerson()
        
        print(person$Wahlkreis)
    })
    nameListe <- reactive({
        req(nrow(dataPerson())>0)
        
        person <- dataPerson()
        
        print(person$ListeBezeichnung)
    })
    
    dataBarchart <- reactive({
        req(dataPerson())
        
        person <- filteredData() %>%
            filter(Name == namePerson()) %>% 
            filter(Wahlkreis == nameWahlkreis()) %>% 
            filter(ListeBezeichnung == nameListe()) %>% 
            select(Name, StimmeVeraeListe, Value) %>% 
            filter(!is.na(Value) & Value > 0) %>% 
            arrange(desc(Value))
        person
        
    })

  
    output$nameCandidate <- renderText({
        req(namePerson())
        
        if(!is.null(namePerson())){
        paste("<br><h2>", print(namePerson()), "</h2><hr>")
        }else{}
    })
    
    output$tableCand <- renderReactable({
        req(nrow(dataPerson())>0)

        CandInfo <- dataPerson() %>%
            select(-Name, -Wahlkreis, -ListeBezeichnung) %>% 
            gather(`Detailinformationen zu den erhaltenen Stimmen`, Wert)


        tableOutput <- reactable(CandInfo,
                                 paginationType = "simple",
                                 theme = reactableTheme(
                                     borderColor = "#DEDEDE"
                                 ),
                                 defaultColDef = colDef(
                                     align = "left",
                                     minWidth = 50
                                 ),
                                 outlined = TRUE,
                                 highlight = TRUE
        )
        tableOutput
    })
    
    observe({ update_data(dataBarchart()) })
    
    
    ## Write Download Table
    # CSV
    output$csvDownload <- downloadHandler(
        filename = function(vote) {
            
            suchfeld <- gsub(" ", "-", namePerson(), fixed = TRUE) 
            paste0("Gemeinderatswahlen_", input$select, "_", suchfeld, ".csv")
            
        },
        content = function(file) {
            write.csv(dataDownload(), file, fileEncoding = "UTF-8", row.names = FALSE, na = " ")
        }
    )
    
    # Excel
    output$excelDownload <- downloadHandler(
        filename = function(vote) {
            
            suchfeld <- gsub(" ", "-",  namePerson(), fixed = TRUE)
            paste0("Gemeinderatswahlen_", input$select, "_", suchfeld, ".xlsx")
            
        },
        content = function(file) {
            sszDownloadExcel(dataDownload(), file, namePerson())
        }
    )
    
    output$titleVote <- renderText({
        req(nameVote())
        
        paste("<br><h2>", print(nameVote()), "</h2><hr>")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

