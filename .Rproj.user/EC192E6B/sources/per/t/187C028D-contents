box::use(
  shiny[moduleServer, NS, fluidPage, fluidRow, uiOutput, renderUI, observe,
        req, selectInput, em],
  htmltools[tags, HTML],
  plotly[renderPlotly, plotlyOutput],
  bslib[card]
)

box::use(
  app/logic/plots_utils
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(HTML("
      .bslib-full-screen-enter.badge.rounded-pill {
        color: red !important;
        font-size: 20px !important;
        display: block !important; /* Ensure the button is always visible */
        visibility: visible !important;
      }
       .badge.rounded-pill {
        border-radius: 50px !important; /* Ensure proper rounded pill shape */
      display: block !important; /* Ensure the button is always visible */
        visibility: visible !important;
       }
        .modal-button-container {
        display: flex;
        justify-content: flex-end;
        align-items: center;
        width: 100%;
      }
    "))
    ),
    fluidRow(
      # div(class = "modal-button-container", uiOutput(ns('modal'))),
      uiOutput(ns('plots'))
    )
  )
}

#' @export
server <- function(id, gene_lists, data, data_col_options, title) {
  moduleServer(id, function(input, output, session) {
    output$plots <- renderUI({
      req(length(gene_lists()) > 0)
      fluidRow(
        selectInput(
          session$ns('select_lists'),
          'Select gene lists',
          choices = names(gene_lists()),
          selected = names(gene_lists()),
          multiple = TRUE
          ),
        selectInput(
          session$ns('select_axis'),
          'Select x-axis',
          choices = data_col_options,
          selected = data_col_options[[1]],
          multiple = FALSE
        ),
        fluidRow(
          em('Note: Hover over a plot and click the expand button at the bottom right to display the plot full screen')
        ),
        card(
          full_screen = TRUE,
          plotlyOutput(session$ns('plot_id'))
        )
      )
    })

    observe({
      gene_lists()
      gene_lists2 <- gene_lists()[input$select_lists]
      req(length(gene_lists2 > 0))
      output$plot_id <- renderPlotly({
        plots_utils$getTopTenPlot(
          data = data,
          gene_lists = gene_lists2,
          data_col = input$select_axis,
          title = title
          )
      })
    })
  })
}
