# Load Library
library(fst)
library(htmltools)
library(icons)
library(lubridate)
library(openxlsx)
library(reactable)
library(readxl)
library(shiny)
library(shinyjs)
library(tidyverse)
library(zuericssstyle)

# Set the Icon path
ssz_icons <- icon_set("www/icons/")

# Source all functions - done automatically in the newer shiny versions
# purrr::map(list.files("R/", full.names = TRUE), source)

# get data and make Data Frames
data <- get_data()
df_main <- data[["df_main"]]
df_details <- data[["df_details"]]
unique_wj <- sort(unique(df_main$Wahljahr))

dependencies <- getDependencies()

# Define UI
ui <- fluidPage(
    
    # Include CSS
    includeCSS("www/sszThemeShiny.css"),
    includeCSS("www/KALI.css"),
    
    # include appropriate dependencies
    dependencies,
    
    useShinyjs(),
    
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
                h3("Detailinformationen herunterladen"),
                
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
                shinycssloaders::withSpinner(
                  reactableOutput("table"),
                  type = 7,
                  color = "#0F05A0"
                ),
                
            ),
            
            # initialise hidden variable for row selection, to be used with JS function in reactable
            conditionalPanel("false",
                             numericInput(label = NULL, 
                                          inputId = "show_details", 
                                          value = 0)),
            
            # Name of selected candidate - requires show_details > 0
            htmlOutput("nameCandidate"),
            
            # table with info about selected candidate - requires show_details > 0
            shinycssloaders::withSpinner(
              reactableOutput("tableCand"),
              type = 7,
              color = "#0F05A0"
            ),
            
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
    
    # update selection of lists based on selected year
    observeEvent(input$select_year, {
      new_choices <-  c(
        "Alle Listen",
        unique(df_main[df_main$Wahljahr == input$select_year, ]$ListeBezeichnung)
        )
      updateSelectInput(session = session,
                        inputId = "select_liste",
                        choices = new_choices,
                        selected = new_choices[[1]])
    })
    
    # Filter data according to inputs
    filtered_data <- reactive({
      df_main %>%
        filter(Wahljahr == input$select_year) %>%
        filter(if (input$suchfeld != "") grepl(input$suchfeld, Name, ignore.case = TRUE) else TRUE) %>%
        filter(if (input$gender_radio_button != "Alle") Geschlecht == input$gender_radio_button else TRUE) %>%
        filter(if (input$select_kreis != "Ganz Stadt") Wahlkreis == input$select_kreis else TRUE)  %>%
        filter(if (input$select_liste != "Alle Listen") ListeBezeichnung == input$select_liste else TRUE) %>%
        filter(if (input$wahlstatus_radio_button != "Alle") Wahlresultat == input$wahlstatus_radio_button else TRUE)
       
    }) 
    
    # main Reactable Output
    output$table <- renderReactable({
        
        req(input$ActionButtonId > 0)
        
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
                   updateNumericInput(session, "show_details", value = 0)},
                 ignoreNULL = FALSE)
    
    data_person <- reactive({
      req(input$show_details > 0)
        
        person <- filtered_data() %>%
            select(Name, Wahlkreis, ListeBezeichnung, Liste, Wahlresultat, 
                   `Anzahl Stimmen`, `Parteieigene Stimmen`, 
                   `Parteifremde Stimmen`,
                   `Anteil Stimmen aus veränderten Listen`) %>%
            mutate(ID = row_number()) %>%
            filter(ID == input$show_details) %>% 
            select(-ID)
        person

    }) %>% 
      bindEvent(input$show_details)
    
    data_download <- reactive({
      req(input$show_details > 0)
        person <- filtered_data() %>%
            select(Wahljahr, Name, Alter, Geschlecht, Beruf, Wahlkreis, Liste, 
                   Wahlresultat, `Anzahl Stimmen`, `Parteieigene Stimmen`, 
                   `Parteifremde Stimmen`,
                   `Anteil Stimmen aus veränderten Listen`) %>%
            mutate(ID = row_number()) %>%
            filter(ID == input$show_details) %>% 
            select(-ID) %>% 
            gather(`Result der Wahl`, Wert, -Wahljahr, -Name, -Alter, 
                   -Geschlecht, -Beruf, -Wahlkreis, -Liste)
        person
        
    }) %>% 
      bindEvent(input$show_details)

    # Render title of selected person
    output$nameCandidate <- renderText({
      req(input$show_details > 0)
      paste0("<br><h2>", data_person()$Name, " (", data_person()$Liste, ")", "</h2><hr>")
    })
    
    # table for selected person
    output$tableCand <- renderReactable({
      req(input$show_details > 0)
        
        candidate_info <- data_person() %>%
            select(-Name, -Wahlkreis, -ListeBezeichnung, -Liste) %>% 
            gather(`Detailinformationen zu den erhaltenen Stimmen`, Wert)


        table_output <- get_reactable_details(candidate_info)
        table_output
    })
    
    # create and send data for bar chart
    # observeEvent rather than observe to avoid race condition between sending
    # the data and setting the input$show_details/the selected row number
    observeEvent(input$show_details,
                 { 
                   req(input$ActionButtonId > 0)
                   
                   if (input$show_details > 0) {
                     shinyjs::show("sszvis-chart")
                     
                     person <- df_details %>%
                       #filter the equivalent of filtered_dat that is not also filtered below
                       filter(Wahljahr == input$select_year) %>%
                       #filter(if (input$suchfeld != "") grepl(input$suchfeld, Name, ignore.case = TRUE) else TRUE) %>%
                       filter(if (input$wahlstatus_radio_button != "Alle") Wahlresultat == input$wahlstatus_radio_button else TRUE) %>% 
                       
                       filter(Name == data_person()$Name) %>% 
                       filter(Wahlkreis == data_person()$Wahlkreis) %>% 
                       filter(ListeBezeichnung == data_person()$ListeBezeichnung) %>% 
                       select(Name, StimmeVeraeListe, Value) %>% 
                       filter(!is.na(Value) & Value > 0) %>%
                       arrange(desc(Value))
                     
                     update_chart(person, "update_data", session)

                   } else {
                    # hide the chart (sending empty custom message does not work with iframe resizer on ssz website)
                     shinyjs::hide("sszvis-chart")
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
