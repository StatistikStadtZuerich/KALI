# Load Library
library(shinyjs)
library(magrittr)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)
library(reactable)
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

# source functions to prepare reactables
source("get_reactables_candidates.R")

dependencies <- getDependencies()
chart_container <- tags$div(id="sszvis-chart")

# Define UI
ui <- fluidPage(
    
    # Include CSS
    includeCSS("sszThemeShiny.css"),
    
    # include appropriate dependencies
    dependencies,
    
    # Application Title 
    titlePanel("Kandidierendenlisten App"),
    
    # Sidebar: Input widgets are placed here
    sidebarLayout(
        sidebarPanel(
            
            # Suchfeld: Namensuche
            sszTextInput("suchfeld", "Name:"),
            
        
            # radioButtons() vertical for gender
            sszRadioButtons("gender_radio_button",
                            label = "Geschlecht:",
                            choices = c("Alle", "Männlich", "Weiblich"),
                            selected = "Alle" # default value
                            ),
            
            # selectInput() for year of election
            sszSelectInput("select_year", "Gemeinderatswahlen:", 
                        choices = unique(data$Wahljahr)),
            
            # selectInput() for Stadtkreis
            sszSelectInput("select_kreis", "Wahlkreis:", 
                        choices = c("Ganz Stadt", "Kreis 1 + 2", "Kreis 3", "Kreis 4 + 5", "Kreis 6",
                                    "Kreis 7 + 8", "Kreis 9", "Kreis 10", "Kreis 11", "Kreis 12"),
                        selected = "Ganz Stadt"),
            
            
            # selectInput() for party
            sszSelectInput("select_liste", "Liste:", 
                          choices = c("Alle Listen"),
                          selected = "Alle Listen"
                          ),
            
            # radioButtons() vertical for whether the person was elected
            sszRadioButtons("wahlstatus_radio_button",
                            label = "Status:",
                            choices = c("Alle", "gewählt", "nicht gewählt"),
                            selected = "Alle" 
                            ),
            
            # Action Button to start the query and show the resulting table
            conditionalPanel(
                condition = 'input.ActionButtonId==0',
                
                sszActionButton("ActionButtonId",
                                "Abfrage starten")
            ),
            
            # Downloads - only show these when one person is selected to 
            # download details about this person
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
                                   onclick = "window.open('https://data.stadt-zuerich.ch/dataset?q=Kandidierende&sort=score+desc%2C+date_last_modified+desc', '_blank')"
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
                ),
                
                # Example Table Output 
                reactableOutput("table"),
            ),
            
            # initialise hidden variable for row selection, to be used with JS function in reactable
            conditionalPanel("false",
                             numericInput(label = NULL, inputId = 'show_details', value = 0)),
            
            # Name of selected candidate - requires show_details > 0
            htmlOutput("nameCandidate"),
            
            # table with info about selected candidate - requires show_details > 0
            reactableOutput("tableCand"),
            
            # Only show plot if tableCand is also shown
            conditionalPanel(
              condition = 'output.tableCand',
            
              # Name of selected candidate
              # Title for table
              hr(),
              h4("Stimmen aus veränderten Listen"),
              
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
    
    # function to send updated data to json for D3 chart
    update_data <- function(data) {
      print(glue::glue("sending message {jsonlite::toJSON(data)}"))
        session$sendCustomMessage(
            type = "update_data",
            message = jsonlite::toJSON(data)
        )
    }
    
    # update selection of lists based on selected year
    observeEvent(input$select_year, {
      new_choices <-  c('Alle Listen',
                        unique(data[data$Wahljahr == input$select_year,]$ListeBezeichnung))
      print(glue::glue("update selection with {new_choices}"))
      updateSelectInput(session = session,
                        inputId = 'select_liste',
                        choices = new_choices,
                        selected = new_choices[[1]])
    })
    
    # Filter data according to inputs
    filteredData <- reactive({
      data %>%
        filter(Wahljahr == input$select_year) %>%
        filter(if(input$suchfeld != "") grepl(input$suchfeld, Name, ignore.case=TRUE) else TRUE) %>%
        filter(if(input$gender_radio_button != "Alle") Geschlecht == input$gender_radio_button else TRUE) %>%
        filter(if(input$select_kreis != "Ganz Stadt") Wahlkreis == input$select_kreis else TRUE)  %>%
        filter(if(input$select_liste != "Alle Listen") ListeBezeichnung == input$select_liste else TRUE) %>%
        filter(if(input$wahlstatus_radio_button != "Alle") Wahlresultat == input$wahlstatus_radio_button else TRUE)
       
    }) 
    # %>%
    #     bindCache(input$ActionButtonId, input$select_year, input$suchfeld, input$gender_radio_button,
    #               input$select_kreis, input$select_liste, input$wahlstatus_radio_button) %>%
    #     bindEvent(input$ActionButtonId, input$select_year, input$suchfeld, input$gender_radio_button,
    #               input$select_kreis, input$select_liste, input$wahlstatus_radio_button)
    
    
    # main Reactable Output
    output$table <- renderReactable({
        
        req(global$activeButton == TRUE)
        
        tableOutput <- get_reactable_candidates(filteredData())
        tableOutput
    })
    
    # Prepare data for second Output
    
    # update the show_details to zero when any of the inputs are changed
    observeEvent(eventExpr = list(input$suchfeld,input$select_year,
                                  input$gender_radio_button,
                                  input$select_kreis, input$select_liste,
                                  input$wahlstatus_radio_button),
                 handlerExpr = {
                   print("setting to zero")
                   updateNumericInput(session, "show_details", value = 0)},
                 ignoreNULL = FALSE)
    
    # turn the show_details into a reactive so it can be used further
    rowNumber <- reactive({
      print(glue::glue("row number selected: {input$show_details}"))
      input$show_details
    })
    
    
    dataPerson <- reactive({
      req(rowNumber() > 0)
        
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
      req(rowNumber() > 0)
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

    # Render title of selected person
    output$nameCandidate <- renderText({
      req(rowNumber() > 0)
      paste("<br><h2>", print(dataPerson()$Name), "</h2><hr>")
    })
    
    # table for selected person
    output$tableCand <- renderReactable({
      req(rowNumber() > 0)
        
        CandInfo <- dataPerson() %>%
            select(-Name, -Wahlkreis, -ListeBezeichnung) %>% 
            gather(`Detailinformationen zu den erhaltenen Stimmen`, Wert)


        tableOutput <- get_reactable_details(CandInfo)
        tableOutput
    })
    
    # create and send data for bar chart
    observe({ 
      req(global$activeButton == TRUE)
      req(rowNumber() > 0)
      person <- filteredData() %>%
        filter(Name == dataPerson()$Name) %>% 
        filter(Wahlkreis == dataPerson()$Wahlkreis) %>% 
        filter(ListeBezeichnung == dataPerson()$ListeBezeichnung) %>% 
        select(Name, StimmeVeraeListe, Value) %>% 
        filter(!is.na(Value) & Value > 0) %>% 
        arrange(desc(Value))
      
      update_data(person) 
      })
    
    
    ## Write Download Table
    # CSV
    output$csvDownload <- downloadHandler(
        filename = function(vote) {
            
            suchfeld <- gsub(" ", "-", dataPerson()$Name, fixed = TRUE) 
            paste0("Gemeinderatswahlen_", input$select_year, "_", suchfeld, ".csv")
            
        },
        content = function(file) {
            write.csv(dataDownload(), file, fileEncoding = "UTF-8", row.names = FALSE, na = " ")
        }
    )
    
    # Excel
    output$excelDownload <- downloadHandler(
        filename = function(vote) {
            
            suchfeld <- gsub(" ", "-",  dataPerson()$Name, fixed = TRUE)
            paste0("Gemeinderatswahlen_", input$select_year, "_", suchfeld, ".xlsx")
            
        },
        content = function(file) {
            sszDownloadExcel(dataDownload(), file, dataPerson()$Name)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

