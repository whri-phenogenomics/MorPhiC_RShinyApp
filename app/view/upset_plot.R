box::use(
  shiny[moduleServer, NS, fluidPage, fluidRow, uiOutput, renderUI, selectInput,
        reactive, observe, req, renderPlot, plotOutput, em],
  htmltools[p],
  bslib[card]
)

box::use(
  app/logic/plots_utils
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      uiOutput(ns('plot'))
    )
  )
}

#' @export
server <- function(id, gene_lists) {
  moduleServer(id, function(input, output, session) {

    genes <- reactive({
      if (is.null(input$select_lists)) {
        gene_lists()[1:4]
      } else {
        gene_lists()[input$select_lists]
      }
    })

    # observe({
    #   input$select_lists
    #   print(genes())
    #   print(gene_lists())
    # })

    output$plot <- renderUI({
      genes <- genes()
      fluidRow(
        selectInput(
          session$ns('select_lists'),
          'Select gene lists',
          choices = names(gene_lists()),
          selected = names(genes),
          multiple = TRUE
        ),
        card(
          full_screen = TRUE,
          plotOutput(session$ns('plot_id'))
        )
      )
    })
    
    observe({
      genes <- genes()
      set_size_max <- max(sapply(genes, function(x) length(x$gene_list))) + 50
      output$plot_id <- renderPlot({
        plots_utils$generateUpsetR(genes, set_size_max)
      })
    })
  })
}
