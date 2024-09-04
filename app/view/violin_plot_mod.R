# violin_chart_mod.R
box::use(
  shiny[moduleServer, NS, uiOutput, renderUI, selectInput, reactive,
        observe, req, fluidRow, fluidPage, observeEvent, showModal,
        modalDialog, actionButton],
  purrr[map],
  htmltools[p]
)

box::use(
  app/logic/plots_utils
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      # uiOutput(ns('modal')),
      uiOutput(ns('plots'))
    )
  )
}

#' @export
server <- function(id, saved_lists_and_filters, data, plot_ids, threshold_values, footers, titles, plots_info) {
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
      plots_utils$violinPlotUI(
        plot_ids = plot_ids,
        select_args = saved_lists_and_filters(),
        footers = footers,
        titles = titles,
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
    highlighted_genes <- reactive({
      if (is.null(input$highlight_genes) || input$highlight_genes == "") {
        character(0)  # Return an empty character vector if no genes are entered
      } else {
        unlist(strsplit(input$highlight_genes, ";\\s*"))  # Split the input string into genes
      }
    })
    toggle_points <- reactive({
      input$toggle_points
    })
    
    observe({
      req(length(saved_lists_and_filters()) > 0)
      gene_lists()
      highlighted_genes()
      toggle_points()
      plots_utils$violinPlotServer(
        plot_ids = plot_ids,
        data = data,
        gene_lists = gene_lists(),
        plot_func = plots_utils$violinPlot,
        genes_to_highlight = highlighted_genes(),
        threshold_values = threshold_values,
        toggle_option = toggle_points(),
        output = output
      )
    })
  })
}
