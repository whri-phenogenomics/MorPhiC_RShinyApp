box::use(
  shiny[moduleServer, NS]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns('plots'))
}

#' @export
server <- function(id, rda_obj) {
  moduleServer(id, function(input, output, session) {
    output$plots <- renderUI({
      
    })
  })
}
