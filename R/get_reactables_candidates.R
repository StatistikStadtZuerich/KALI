#' get_reactable_candidates
#' 
#' @description function to generate the main table with a list of candidates
#'
#' @param df filtered data to be shown
#'
#' @return a reactable
get_reactable_candidates <- function(df) {
  reactable(df %>%
              select(Name, Alter, Geschlecht, Beruf, Wahlkreis, Liste) %>% 
              unique(),
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
            onClick = JS("function(rowInfo, column) {
            
            // Send the click event to Shiny, which will be available in input$show_details
            // Note that the row index starts at 0 in JavaScript, so we add 1
            if (window.Shiny) {
            Shiny.setInputValue('show_details', rowInfo.index + 1, { priority: 'event' })
            }
            }")
  )
}

#' get_reactable_details
#' 
#' @description function to generate the reactable for the details about the candidate
#'
#' @param candidate 
#'
#' @return a reactable
get_reactable_details <- function(candidate) {
  reactable(candidate,
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
}