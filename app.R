# Load Library
library(shinyjs)
library(tidyverse)
library(lubridate)
library(openxlsx)
library(readxl)
library(reactable)
library(shiny)
library(htmltools)
library(icons)
library(zuericssstyle)

# Set the Icon path
ssz_icons <- icon_set("www/icons/")

# Source Prepared Data
source("R/get_data.R", encoding = "UTF-8")
data <- get_data()
unique_wj <- sort(unique(data$Wahljahr))

# Source Export Excel
source("R/ssz_download_excel.R", encoding = "UTF-8")

# source functions to prepare reactables
source("R/get_reactables_candidates.R")

# Source Dependencies
source("R/dependencies.R", encoding = "UTF-8")

dependencies <- getDependencies()

# Define UI
ui <- fluidPage(
    
    # Include CSS
    includeCSS("www/sszThemeShiny.css"),
    
    # include appropriate dependencies
    dependencies,
    
    # Sidebar: Input widgets are placed here
    sidebarLayout(
        sidebarPanel(
            
            # Suchfeld: Namensuche
            sszTextInput("suchfeld", "Name"),
            
        
            # radioButtons() vertical for gender
            sszRadioButtons("gender_radio_button",
                            label = "Geschlecht",
                            choices = c("Alle", "Männlich", "Weiblich"),
                            selected = "Alle" # default value
                            ),
            
            # selectInput() for year of election
            sszSelectInput("select_year", "Gemeinderatswahlen", 
                        choices = unique_wj,
                        selected = unique_wj[[length(unique_wj)]]
                        ),
            
            # selectInput() for Stadtkreis
            sszSelectInput("select_kreis", "Wahlkreis", 
                        choices = c("Ganz Stadt", "Kreis 1 + 2", "Kreis 3", 
                                    "Kreis 4 + 5", "Kreis 6", "Kreis 7 + 8", 
                                    "Kreis 9", "Kreis 10", "Kreis 11", 
                                    "Kreis 12"),
                        selected = "Ganz Stadt"),
            
            
            # selectInput() for party
            sszSelectInput("select_liste", "Liste", 
                          choices = c("Alle Listen"),
                          selected = "Alle Listen"
                          ),
            
            # radioButtons() vertical for whether the person was elected
            sszRadioButtons("wahlstatus_radio_button",
                            label = "Status",
                            choices = c("Alle", "gewählt", "nicht gewählt"),
                            selected = "Alle" 
                            ),
            
            # Action Button to start the query and show the resulting table
            conditionalPanel(
                condition = "input.ActionButtonId==0",
                sszActionButton("ActionButtonId",
                                "Abfrage starten")
            ),
            
            # Downloads - only show these when one person is selected to 
            # download details about this person
            conditionalPanel(
                condition = "output.tableCand",
                h3("Daten herunterladen"),
                
                # Download Panel
                tags$div(
                    id = "downloadWrapperId",
                    class = "downloadWrapperDiv",
                    
                    sszDownloadButton(
                      outputId = "csvDownload",
                      label = "csv",
                      image = img(ssz_icons$download)
                    ),
                    sszDownloadButton(
                      outputId = "excelDownload",
                      label = "xlsx",
                      image = img(ssz_icons$download)
                    ),
                    sszOgdDownload(outputId = "ogdDown",
                                   label = "OGD",
                                   image = img(ssz_icons("external-link")),
                                   href = "https://data.stadt-zuerich.ch/dataset?q=Kandidierende&sort=score+desc%2C+date_last_modified+desc"
                    )
                )
            )
        ),
        
        
        # Mail Panel: Outputs are placed here
        mainPanel(
            
            conditionalPanel(
                condition = "input.ActionButtonId>0",
                
                # Title for table
                h1("Die untenstehenden Kandidierenden entsprechen Ihren Suchkriterien"),
                hr(),
                # Define subtitle
                p("Für Detailinformationen zu den Ergebnissen einzelner Kandidierenden wählen Sie eine Zeile aus."),
                
                # Example Table Output 
                reactableOutput("table"),
            ),
            
            # initialise hidden variable for row selection, to be used with JS function in reactable
            conditionalPanel("false",
                             numericInput(label = NULL, 
                                          inputId = "show_details", 
                                          value = 0)),
            
            # Name of selected candidate - requires show_details > 0
            htmlOutput("nameCandidate"),
            
            # table with info about selected candidate - requires show_details > 0
            reactableOutput("tableCand"),
            
            # Only show plot if tableCand is also shown
            conditionalPanel(
              condition = "output.tableCand",
            
              # Name of selected candidate
              # Title for table
              hr(),
              h4("Stimmen aus veränderten Listen"),
              
            ),

            # Chart container; can't be used in a conditional panel as when the
            # update_data function is called, the UI is not ready yet when JS tries
            # to target container id.
            # ID must be "sszvis-chart", as this is what the JS is looking to fill
            htmlOutput("sszvis-chart")

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
      new_choices <-  c(
        "Alle Listen",
        unique(data[data$Wahljahr == input$select_year, ]$ListeBezeichnung)
        )
      print(glue::glue("update selection with {new_choices}"))
      updateSelectInput(session = session,
                        inputId = "select_liste",
                        choices = new_choices,
                        selected = new_choices[[1]])
    })
    
    # Filter data according to inputs
    filtered_data <- reactive({
      data %>%
        filter(Wahljahr == input$select_year) %>%
        filter(if (input$suchfeld != "") grepl(input$suchfeld, Name, ignore.case = TRUE) else TRUE) %>%
        filter(if (input$gender_radio_button != "Alle") Geschlecht == input$gender_radio_button else TRUE) %>%
        filter(if (input$select_kreis != "Ganz Stadt") Wahlkreis == input$select_kreis else TRUE)  %>%
        filter(if (input$select_liste != "Alle Listen") ListeBezeichnung == input$select_liste else TRUE) %>%
        filter(if (input$wahlstatus_radio_button != "Alle") Wahlresultat == input$wahlstatus_radio_button else TRUE)
       
    }) 
    
    # main Reactable Output
    output$table <- renderReactable({
        
        req(global$activeButton == TRUE)
        
        table_output <- get_reactable_candidates(filtered_data())
        table_output
    })
    
    # Prepare data for second Output
    
    # update the show_details to zero when any of the inputs are changed
    observeEvent(eventExpr = list(input$suchfeld, input$select_year,
                                  input$gender_radio_button,
                                  input$select_kreis, input$select_liste,
                                  input$wahlstatus_radio_button),
                 handlerExpr = {
                   print("setting to zero")
                   updateNumericInput(session, "show_details", value = 0)},
                 ignoreNULL = FALSE)
    
    data_person <- reactive({
      req(input$show_details > 0)
        
        person <- filtered_data() %>%
            select(Name, Wahlkreis, ListeBezeichnung, Wahlresultat, 
                   `Anzahl Stimmen`, `Parteieigene Stimmen`, 
                   `Parteifremde Stimmen`,
                   `Anteil Stimmen aus veränderten Listen`) %>%
            unique() %>%
            mutate(ID = row_number()) %>%
            filter(ID == input$show_details) %>% 
            select(-ID)
        person

    })
    
    data_download <- reactive({
      req(input$show_details > 0)
        person <- filtered_data() %>%
            select(Wahljahr, Name, Alter, Geschlecht, Beruf, Wahlkreis, Liste, 
                   Wahlresultat, `Anzahl Stimmen`, `Parteieigene Stimmen`, 
                   `Parteifremde Stimmen`,
                   `Anteil Stimmen aus veränderten Listen`) %>%
            unique() %>%
            mutate(ID = row_number()) %>%
            filter(ID == input$show_details) %>% 
            select(-ID) %>% 
            gather(`Result der Wahl`, Wert, -Wahljahr, -Name, -Alter, 
                   -Geschlecht, -Beruf, -Wahlkreis, -Liste)
        person
        
    })

    # Render title of selected person
    output$nameCandidate <- renderText({
      req(input$show_details > 0)
      paste("<br><h2>", print(data_person()$Name), "</h2><hr>")
    })
    
    # table for selected person
    output$tableCand <- renderReactable({
      req(input$show_details > 0)
        
        candidate_info <- data_person() %>%
            select(-Name, -Wahlkreis, -ListeBezeichnung) %>% 
            gather(`Detailinformationen zu den erhaltenen Stimmen`, Wert)


        table_output <- get_reactable_details(candidate_info)
        table_output
    })
    
    # create and send data for bar chart
    # observeEvent rather than observe to avoid race condition between sending
    # the data and setting the input$show_details/the selected row number
    observeEvent(input$show_details,
                 { 
                   req(global$activeButton == TRUE)
                   
                   if (input$show_details > 0) {
                     person <- filtered_data() %>%
                       filter(Name == data_person()$Name) %>% 
                       filter(Wahlkreis == data_person()$Wahlkreis) %>% 
                       filter(ListeBezeichnung == data_person()$ListeBezeichnung) %>% 
                       select(Name, StimmeVeraeListe, Value) %>% 
                       filter(!is.na(Value) & Value > 0) %>%
                       arrange(desc(Value))
                     
                     update_data(person)   
                   } else {
                    # sendCustomMessage requires a message argument to be defined,
                    # even though it's not needed in this case.
                     session$sendCustomMessage(type = "clear_chart", message="")
                   }
                  })
    
    
    ## Write Download Table
    # CSV
    output$csvDownload <- downloadHandler(
        filename = function(vote) {
            
            suchfeld <- gsub(" ", "-", data_person()$Name, fixed = TRUE) 
            paste0("Gemeinderatswahlen_", input$select_year, "_", suchfeld, ".csv")
            
        },
        content = function(file) {
            write.csv(data_download(), file, fileEncoding = "UTF-8", row.names = FALSE, na = " ")
        }
    )
    
    # Excel
    output$excelDownload <- downloadHandler(
        filename = function(vote) {
            
            suchfeld <- gsub(" ", "-",  data_person()$Name, fixed = TRUE)
            paste0("Gemeinderatswahlen_", input$select_year, "_", suchfeld, ".xlsx")
            
        },
        content = function(file) {
          ssz_download_excel(data_download(), file, data_person()$Name)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
