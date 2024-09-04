# bar_chart_mod.R
box::use(
  shiny[moduleServer, NS, uiOutput, renderUI, selectInput, reactive,
        observe, req, fluidRow, fluidPage, observeEvent, showModal,
        modalDialog, actionButton, HTML, div],
  purrr[map],
  htmltools[p, tags]
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
server <- function(id, saved_lists_and_filters, data, plot_ids, footers, titles, plots_info) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # output$modal <- renderUI({
    #   req(length(saved_lists_and_filters()) > 0)
    #   actionButton(session$ns("modal"), "Plots info")
    # })
    
    observeEvent(input$modal, {
      showModal(modalDialog(
        title = "Plots information",
        plots_info,
        easyClose = TRUE,
      ))
    })
    
    output$plots <- renderUI({
      req(length(saved_lists_and_filters()) > 0)
      plots_utils$barChartUI(
        plot_ids = plot_ids,
        footers = footers,
        titles = titles,
        select_args = saved_lists_and_filters(),
        id = id,
        session = session
      )
    })
    
    gene_lists <- reactive({
      req(length(saved_lists_and_filters()) > 0)
      gene_lists <- map(saved_lists_and_filters(), ~ .x[['gene_list']])
      names(gene_lists) <- names(saved_lists_and_filters())
      gene_lists <- gene_lists[input$gene_list_picker]
    })
    
    yaxis_option <- reactive({
      input$percentage_counts_picker
    })
    
    observe({
      req(length(saved_lists_and_filters()) > 0)
      gene_lists()
      yaxis_option()
      plots_utils$barChartServer(
        plot_ids = plot_ids,
        data = data,
        gene_lists = gene_lists(),
        plot_func = plots_utils$barChart,
        output = output,
        yaxis_option = yaxis_option()
      )
    })
  })
}
