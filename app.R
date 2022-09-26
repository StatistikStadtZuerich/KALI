# Load Library
library(shiny)
library(reactable)
library(dplyr)
library(lubridate)
library(icons)
library(xlsx)
library(htmltools)

# Source Donwload Function
source("sszDownload.R", local = TRUE)

# Source Prepared Data
source("prepareData.R", local = TRUE, encoding = "UTF-8")

# Set the Icon path
icon <- icon_set("icons/")


# Define UI
ui <- fluidPage(
    
    # Include CSS
    includeCSS("sszTheme.css"),
    
    # Application Title 
    titlePanel("Kandidierendenlisten App"),
    
    # Sidebar: Input widgets are placed here
    sidebarLayout(
        sidebarPanel(
            
            # Example textInput()
            textInput("suchfeld", "Name:"),
            
        
            # Example radioButtons() vertical
            tags$div(
                class = "Status",
                radioButtons(inputId = "ButtonGroupLabel",
                             # inline = TRUE,
                             label = "Geschlecht:",
                             choices = c("Alle", "Männlich", "Weiblich"),
                             selected = "Alle" # default value
                )
            ),
            
            # Example selectInput()
            selectInput("select", "Gemeinderatswahlen:", 
                        choices = unique(df$Wahljahr)),
            
            # Example selectInput()
            selectInput("select2", "Wahlkreis:", 
                        choices = c("Ganz Stadt", unique(df$Wahlkreis)),
                        selected = "Ganz Stadt"),
            
            
            conditionalPanel(
                condition = 'input.select == "2022"',
                
                # Example selectInput()
                selectInput("select3", "Liste:", 
                            choices = c("Alle Listen", unique(df[df$Wahljahr == 2022,]$ListeBezeichnung)),
                            selected = "Alle Listen"),
                
                # Example radioButtons() vertical
                tags$div(
                    class = "Status",
                    radioButtons(inputId = "ButtonGroupLabel2",
                                 label = "Status:",
                                 choices = c("Alle", "gewählt", "nicht gewählt"),
                                 selected = "Alle" # default value
                    )
                ),
            ),
            conditionalPanel(
                condition = 'input.select == "2018"',
                
                # Example selectInput()
                selectInput("select3", "Liste:", 
                            choices = c("Alle Listen", unique(df[df$Wahljahr == 2018,]$ListeBezeichnung)),
                            selected = "Alle Listen"),
                
                # Example radioButtons() vertical
                tags$div(
                    class = "Status",
                    radioButtons(inputId = "ButtonGroupLabel2",
                                 label = "Status:",
                                 choices = c("Alle", "gewählt", "nicht gewählt"),
                                 selected = "Alle" # default value
                    )
                ),
            ),
            conditionalPanel(
                condition = 'input.select == "2014"',

                # Example selectInput()
                selectInput("select3", "Liste:", 
                            choices = c("Alle Listen", unique(df[df$Wahljahr == 2014,]$ListeBezeichnung)),
                            selected = "Alle Listen"),
                
                # Example radioButtons() vertical
                tags$div(
                    class = "Status",
                    radioButtons(inputId = "ButtonGroupLabel21",
                                 label = "Status:",
                                 choices = c("Alle", "gewählt", "nicht gewählt"),
                                 selected = "Alle" # default value
                    )
                ),
            ),
            conditionalPanel(
                condition = 'input.select == "2010"',

                # Example selectInput()
                selectInput("select3", "Liste:", 
                            choices = c("Alle Listen", unique(df[df$Wahljahr == 2010,]$ListeBezeichnung)),
                            selected = "Alle Listen"),
                
                # Example radioButtons() vertical
                tags$div(
                    class = "Status",
                    radioButtons(inputId = "ButtonGroupLabel22",
                                 label = "Status:",
                                 choices = c("Alle", "gewählt", "nicht gewählt"),
                                 selected = "Alle" # default value
                    )
                ),
            ),
            
            
            
            # Action Button
            conditionalPanel(
                condition = 'input.ActionButtonId==0',
                
                actionButton("ActionButtonId",
                             "Abfrage starten")
            ),
            conditionalPanel(
                condition = 'input.ActionButtonId>0',
                
            ),
            
            # Example Download Button
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
                actionButton(inputId = "ogdDown",
                             label = "OGD",
                             onclick ="window.open('https://data.stadt-zuerich.ch/', '_blank')"
                )
                
                
            )
        ),
        
        
        # Mail Panel: Outputs are placed here
        mainPanel(
            
            conditionalPanel(
                condition = 'output.table',
                
                # Define subtitle
                tags$div(
                    class = "infoDiv",
                    p("Die untenstehenden Kandidierenden entsprechen Ihren Suchkriterien. Für Detailinformationen wählen Sie eine Person aus.")
                ),
                hr(),
            ),
            
            # Example Table Output 
            reactableOutput("table"),
            
            # Name of selected person
            htmlOutput("text"),
            
            # Details about selected person
            reactableOutput("table2")
            
            
        )
    )
)


# Server function
server <- function(input, output) {
    
    # First button click to activate search, after not necessary anymore
    global <- reactiveValues(activeButton = FALSE)
    
    observeEvent(input$ActionButtonId, {
        req(input$ActionButtonId)
        global$activeButton <- TRUE
    })
    
    # Filter data according to inputs
    filteredData <- reactive({
        req(global$activeButton == TRUE)
        
        # Filter: No Search
        if(input$suchfeld == "") {
            filtered <- data %>%
                filter(if(input$ButtonGroupLabel != "Alle") Geschlecht == input$ButtonGroupLabel else TRUE) %>%
                filter(Wahljahr == input$select) %>% 
                filter(if(input$select2 != "Ganz Stadt") Wahlkreis == input$select2 else TRUE) %>% 
                filter(if(input$select3 != "Alle Listen") ListeBezeichnung == input$select3 else TRUE) %>% 
                filter(if(input$ButtonGroupLabel2 != "Alle") Wahlresultat == input$ButtonGroupLabel2 else TRUE)
            
            filtered
            
            # Filter: With Search   
        } else {
            filtered <- data %>%
                filter(grepl(input$suchfeld, Name, ignore.case=TRUE)) %>%
                filter(if(input$ButtonGroupLabel != "Alle") Geschlecht == input$ButtonGroupLabel else TRUE) %>%
                filter(Wahljahr == input$select) %>% 
                filter(if(input$select2 != "Ganz Stadt") Wahlkreis == input$select2 else TRUE)  %>% 
                filter(if(input$select3 != "Alle Listen") ListeBezeichnung == input$select3 else TRUE) %>% 
                filter(if(input$ButtonGroupLabel2 != "Alle") Wahlresultat == input$ButtonGroupLabel2 else TRUE)
            
            filtered
            
        }
    })
    
    
    # Reactable Output
    output$table <- renderReactable({
        tableOutput <- reactable(filteredData() %>%
                                     select(Name, Geschlecht, GebJ, Beruf, Wahlkreis) %>% 
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
    rowNumber <- reactive( {
        getReactableState("voteList", "selected")
    })
    
    
    namePerson <- reactive({
        req(rowNumber())
        
        person <- filteredData() %>%
            select(Name, Wahlkreis, GebJ) %>%
            unique() %>%
            mutate(ID = row_number()) %>%
            filter(ID == rowNumber())
        
        print(person$Name)
    })
    
    nameWahlkreis <- reactive({
        req(rowNumber())
        
        person <- filteredData() %>%
            select(Name, Wahlkreis, GebJ) %>%
            unique() %>%
            mutate(ID = row_number()) %>%
            filter(ID == rowNumber())
        
        print(person$Wahlkreis)
    })
    
    nameGebJ <- reactive({
        req(rowNumber())
        
        person <- filteredData() %>%
            select(Name, Wahlkreis, GebJ) %>%
            unique() %>%
            mutate(ID = row_number()) %>%
            filter(ID == rowNumber())
        
        print(person$GebJ)
    })
    
    
    # voteData <- reactive({
    #     req(namePerson())
    #     
    #     vote <- filteredData() %>%
    #         filter(Name == namePerson() & GebJ == nameGebJ() & Wahlkreis == nameWahlkreis()) %>%
    #         select(ListeBezeichnung, Wahlresultat, total_stim, part_eig_stim, part_eig_stim_unv_wl, part_frmd_stim)
    # })
    
    output$text <- renderText({
        req(namePerson())
        
        paste("<h3>", print(rowNumber()), "</h3>")
    })
    
    # output$table <- renderReactable({
    #     req(namePerson())
    #     
    #     tableOutput <- reactable(voteData(),
    #                              paginationType = "simple",
    #                              language = reactableLang(
    #                                  noData = "Keine Einträge gefunden",
    #                                  pageNumbers = "{page} von {pages}",
    #                                  pageInfo = "{rowStart} bis {rowEnd} von {rows} Einträgen",
    #                                  pagePrevious = "\u276e",
    #                                  pageNext = "\u276f",
    #                                  pagePreviousLabel = "Vorherige Seite",
    #                                  pageNextLabel = "Nächste Seite"
    #                              ),
    #                              defaultColDef = colDef(
    #                                  align = "left",
    #                                  minWidth = 50
    #                              ),
    #                              theme = reactableTheme(
    #                                  borderColor = "#DEDEDE"
    #                              ),
    #                              outlined = TRUE,
    #                              highlight = FALSE,
    #                              defaultPageSize = 5,
    #                              onClick = "select",
    #                              selection = "single",
    #                              rowClass = JS("function(rowInfo) {return rowInfo.selected ? 'selected' : ''}"),
    #                              rowStyle = JS("function(rowInfo) {if (rowInfo.selected) { return { backgroundColor: '#F2F2F2'}}}")
    #     )
    # })
    
    # Render data download
    # CSV
    output$csvDownload <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(filteredData(), file)
        }
    )
    
    # Excel
    output$excelDownload <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".xlsx", sep="")
        },
        content = function(file) {
            write.xlsx(filteredData(), file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
