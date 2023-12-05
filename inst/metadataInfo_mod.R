# Load necessary libraries
library(DT)
library(htmltools)
library(shiny)
library(shinyjs)
library(shinydashboard)

# UI ----
library(bslib)

metadataInfoUI <- function(id) {
  tagList(
    h4("Table columns descriptions"),
    hr(),
    div(
      class = "row",
      card(
        height = "200px",
        card_header(
          "Gene Identifiers"
        ),
        card_body(
          DTOutput(NS(id, "table_gene_identifiers"))
        )
      )
    ),
    
    div(
      class = "row",
      card(
        height = "250px",
        card_header(
          "Data Production centers"
        ),
        card_body(
          DTOutput(NS(id, "table_center_info"))
        )
      )
    ),
    
    div(
      class = "row",
      card(
        height = "300px",
        card_header(
          "Mouse Model data"
        ),
        card_body(
          DTOutput(NS(id, "table_impc"))
        )
      )
    ),
    
    div(
      class = "row",
      card(
        card_header(
          "Gene constraint metrics"
        ),
        card_body(
          DTOutput(NS(id, "table_constraint"))
        )
      )
    ),
    
    div(
      class = "row",
      card(
        card_header(
          "Disease data"
        ),
        card_body(
          DTOutput(NS(id, "table_disease"))
        )
      )
    ),
    
    div(
      class = "row",
      card(
        height = "300px",
        card_header(
          "Gene Ontology data"
        ),
        card_body(
          DTOutput(NS(id, "table_go"))
        )
      )
    ),
    
    div(
      class = "row",
      card(
        height = "300px",
        card_header(
          "PANTHERdb data"
        ),
        card_body(
          DTOutput(NS(id, "table_panther"))
        )
      )
    ),
    
    div(
      class = "row",
      card(
        height = "150px",
        card_header(
          "Reactome data"
        ),
        card_body(
          DTOutput(NS(id, "table_reactome"))
        )
      )
    )
  )
}

# SERVER ----
metadataInfoServer <- function(id) {
  moduleServer(id, function(input, output, session) {

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
