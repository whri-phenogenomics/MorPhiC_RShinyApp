# Load necessary libraries
library(DT)
library(htmltools)
library(shiny)
library(shinyjs)

data_info_tables2 <- readRDS("./rda_data/data_info_tables2.rda")

# UI ----
metadataInfoUI <- function(id) {
  tagList(
    fluidRow(
      column(width = 12, box(width = 12, solidHeader = TRUE, status = "primary", title = "Meta Data Information",
                             br(),
                             fluidRow(
                               column(width = 12, box(DTOutput(NS(id, "table_gene_identifiers")), width = 12, solidHeader = FALSE, status = "primary", title = "Gene Identifiers"))
                             ),
                             fluidRow(
                               column(width = 12, box(DTOutput(NS(id,"table_center_info")), width = 12, solidHeader = FALSE, status = "primary", title = "Data Production centers"))
                             ),
                             fluidRow(
                               column(width = 12, box(DTOutput(NS(id,"table_impc")), width = 12, solidHeader = FALSE, status = "primary", title = "Mouse Model data"))
                             ),
                             fluidRow(
                               column(width = 12, box(DTOutput(NS(id,"table_constraint")), width = 12, solidHeader = FALSE, status = "primary", title = "Gene constraint metrics"))
                             ),
                             fluidRow(
                               column(width = 12, box(DTOutput(NS(id,"table_disease")), width = 12, solidHeader = FALSE, status = "primary", title = "Disease data"))
                             ),
                             fluidRow(
                               column(width = 12, box(DTOutput(NS(id,"table_go")), width = 12, solidHeader = FALSE, status = "primary", title = "Gene Ontology data"))
                             ),
                             fluidRow(
                               column(width = 12, box(DTOutput(NS(id,"table_panther")), width = 12, solidHeader = FALSE, status = "primary", title = "PANTHERdb data"))
                             ),
                             fluidRow(
                               column(width = 12, box(DTOutput(NS(id,"table_reactome")), width = 12, solidHeader = FALSE, status = "primary", title = "Reactome data"))
                             )))
    )
  )
}
# SERVER ----
metadataInfoServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    #Load data
    data_info_tables2 <- readRDS("./rda_data/data_info_tables2.rda")

    # Remove column headers
    headerCallback <- c(
      "function(thead, data, start, end, display){",
      "  $('th', thead).css('display', 'none');",
      "}"
    )

    # Render each table using DT::renderDataTable and pass it to the UI
    output$table_gene_identifiers <- DT::renderDataTable({
      datatable(data_info_tables2[[1]], rownames = FALSE, class = "display", options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE, headerCallback = JS(headerCallback)))
    })

    output$table_center_info <- DT::renderDataTable({
      datatable(data_info_tables2[[2]], rownames = FALSE, class = "display", options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE, headerCallback = JS(headerCallback)))
    })

    output$table_impc <- DT::renderDataTable({
      datatable(data_info_tables2[[3]], rownames = FALSE, class = "display",  options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE, headerCallback = JS(headerCallback)))
    })

    output$table_constraint <- DT::renderDataTable({
      datatable(data_info_tables2[[4]], rownames = FALSE, class = "display",  options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE, headerCallback = JS(headerCallback)))
    })

    output$table_disease <- DT::renderDataTable({
      datatable(data_info_tables2[[6]], rownames = FALSE, class = "display",  options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE, headerCallback = JS(headerCallback)))
    })

    output$table_go <- DT::renderDataTable({
      datatable(data_info_tables2[[8]], rownames = FALSE, class = "display",  options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE, headerCallback = JS(headerCallback)))
    })

    output$table_panther <- DT::renderDataTable({
      datatable(data_info_tables2[[9]], rownames = FALSE, class = "display",  options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE, headerCallback = JS(headerCallback)))
    })

    output$table_reactome <- DT::renderDataTable({
      datatable(data_info_tables2[[10]], rownames = FALSE, class = "display",  options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE, headerCallback = JS(headerCallback)))
    })
  })}
