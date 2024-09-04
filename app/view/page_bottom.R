box::use(
  shiny[moduleServer, NS, uiOutput, renderUI, div, column, fluidRow]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns('page_bottom'))
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$page_bottom <- renderUI({
      fluidRow(
        column(
          12,
          div(style = "height:200px;background-color: #89ced9;")
        )
      )
    })
  })
}
