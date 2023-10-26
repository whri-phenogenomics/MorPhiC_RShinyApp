library(shiny)
library(DT)
library(plotly)
library(tools)
library(stringr)
library(dplyr)

source("../R/aux_app_funcs.R")

genesMetaDataDf_data <<- readRDS("../annotated_all_protein_coding_genes_191023.rda")

#---- ui
genesMetaDataTableUI <- function(id) {
  fluidPage(
    # File input + table
    fluidRow(
      width = 12,
      column(
        width = 6,
        fileInput(
          NS(id, "geneList_fileUpload"),
          "Upload gene list as file",
          multiple = TRUE
        ),
        textOutput(
          NS(id, "files_uploaded")),
        verbatimTextOutput(
          NS(id, "file_names"))
      ),
      column(
        width = 6,
        textAreaInput(
          NS(id, "geneList_textInput"),
          "Input gene list as text"
        )
      ),
      column(
        width = 12,
        DTOutput(
          NS(id, "genesMetaDataDf"))
      )
    )
  )
}

#---- server
genesMetaDataTableServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$files_uploaded <- renderText('Files uploaded:')
    output$file_names <- renderText({
      file <- input$geneList_fileUpload
      if (!is.null(file)) {
        file$name
      } else {
        paste('none')
      }
    })
    
    # This will prefer the File Upload over the Text Input
    # Need to handle headers in files
    # Need to handle multiple file uplaods 
    # Add selection from morphic DPC gene lists
    tableData <- reactive({
      if(is.null(input$geneList_fileUpload) && input$geneList_textInput == '')
      {
        genesMetaDataDf_data[1:5,1:10]
      }
      else if (!is.null(input$geneList_fileUpload))
      {
        file <- input$geneList_fileUpload
        path <- file$name
        if (length(path) > 1) {
          unified_and_list <- getGeneList_multiFileInput(file)
          gene_list <- unified_and_list[[1]]
          genesMetaDataDf_data <- subsetGenesMetaDataDf_rowsOnly(gene_list)
        } else {
          gene_list <- getGeneList_FromFile(file)
          genesMetaDataDf_data <- subsetGenesMetaDataDf_rowsOnly(gene_list)
        }
      }
      else if (input$geneList_textInput != '') {
        gene_list <- parseTextAreaInput(input$geneList_textInput)
        genesMetaDataDf_data <- subsetGenesMetaDataDf_rowsOnly(gene_list)
      }
    })
    
    # Add columns for Morphic DPCs to see their genes of interest
    output$genesMetaDataDf <- renderDT({
      localFileNameDownload <- 'local_file_download'
      datatable(
        tableData(),
        class = 'cell-border stripe nowrap',
        plugins = "ellipsis",
        filter = 'top',
        selection = 'none',
        extensions = c("Buttons", "Select"),
        options = list(
          dom = "Bfrtip",
          columnDefs = list(
            list(
              targets = "_all",
              render = JS("$.fn.dataTable.render.ellipsis(17, true)")
            )),
          buttons =  list(
            list(
              extend = 'collection',
              buttons = list(list(extend = "excel", text = "excel", filename = localFileNameDownload,
                                  exportOptions = list(
                                    modifier = list(page = "all", search = "applied"),
                                    orthogonal = "export"
                                  )),
                             list(extend='csv', 
                                  text = "csv",
                                  filename = localFileNameDownload,
                                  exportOptions = list(
                                    modifier = list(page = "all", search = "applied"),
                                    orthogonal = "export"
                                  )),
                             list(extend='pdf',
                                  text = "pdf", 
                                  filename= localFileNameDownload,
                                  exportOptions = list(
                                    modifier = list(page = "all", search = "applied"),
                                    orthogonal = "export"
                                  ))),
              text = 'Download current view'
            )),
          pageLength = 20,
          lengthMenu = list(c(10, 20, 50, 100), c('10', '20', '50','100')),
          scrollX = TRUE
        )
      )
    }, server = FALSE)
})}

