#' update_chart
#' 
#' @description sends data as json as a custom message, which can then be
#' received by JS/the sszvis chart; needs an appropriate message handler
#' function specific to each chart
#'
#' @param data data as tibble to be sent
#' @param type string, specifies which message handler/chart receives the data
#' @param session 
#'
#' @return na
update_chart <- function(data, type, session) {
  # print(glue::glue("sending message {data}"))
  session$sendCustomMessage(
    type = type,
    message = jsonlite::toJSON(data)
  )
}
