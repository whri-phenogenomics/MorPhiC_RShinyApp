library(shiny)
library(DT)
library(plotly)
library(tools)
library(stringr)
library(dplyr)
library(shinyjs)
library(shinyWidgets)

source("../R/aux_app_funcs.R")

#genesMetaDataDf_data <<- readRDS("../annotated_all_protein_coding_genes_191023.rda")

#---- ui
genesMetaDataTableUI <- function(id) {
  tagList(
    div(
      textOutput(
        NS(id, "select_data_table")
      ),
      dropdown(label = "Select MorPhiC Data",
               icon = icon("table"),
               style = "bordered", 
               status = "primary", 
               width = "300px",
               size =  "sm",
               animate = animateOptions(
                 enter = animations$fading_entrances$fadeInLeftBig,
                 exit = animations$fading_exits$fadeOutLeft
               ),
               selectInput(
                 NS(id, "select_dpcs"), 
                 "Select Data Production Center", 
                 c("JAX", "MSK", "NWU", "UCSF"),
                 multiple = TRUE,
                 selected = "JAX"
               )
      ),
      dropdown(label = "Custom Data",
               icon = icon("table"),
               style = "bordered", 
               status = "primary", 
               width = "300px",
               size =  "sm",
               animate = animateOptions(
                 enter = animations$fading_entrances$fadeInLeftBig,
                 exit = animations$fading_exits$fadeOutLeft
               ),
               fileInput(
                 NS(id, "geneList_fileUpload"),
                 "Upload gene list as file",
                 multiple = TRUE
               ),
               textOutput(
                 NS(id, "files_uploaded")
               ),
               verbatimTextOutput(
                 NS(id, "file_names")
               ),
               br(),
               textOutput(
                 NS(id, "or")
               ),
               br(),
               textAreaInput(
                 NS(id, "geneList_textInput"),
                 "Input gene list as text"
               ),
               br(),
               checkboxInput(
                 NS(id, 'view_all_protein_coding_genes'),
                 "View all protein coding genes (reduces performance)"
               )
      ),
      textOutput(
        NS(id,'show_selected_dpcs_header')
      ),
      textOutput(
        NS(id,'show_selected_dpcs')
      ),
    ),
    div(
      DTOutput(
        NS(id, "genesMetaDataDf")
      )
    )
   
    
  )
}

#---- server
genesMetaDataTableServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$show_selected_dpcs_header <- renderText({
      dpc_selected <- input$select_dpcs
      if (length(dpc_selected) >= 1) {
        header <- 'Viewing DPCs:'
      } else {
        header <- 'Viewing custom data'
      }
    })
    
    output$show_selected_dpcs <- renderText({
      input$select_dpcs
    })
    
    output$select_data_table <- renderText('Select data table view')
    
    # Custom data
    output$files_uploaded <- renderText('Files uploaded:')
    output$file_names <- renderText({
      file <- input$geneList_fileUpload
      if (!is.null(file)) {
        file$name
      } else {
        paste('none')
      }
    })
    output$or <- renderText('Or')
    
    # morphicGenesMetaDataTable <- reactive({
    #   dpc_selected <- input$select_dpcs
    #   if (dpc_selected == 'JAX') {
    #     dpc_df <- readRDS("../rda_data/JAX_geneList.rda")
    #   } else if (dpc_selected == 'MSK') {
    #     dpc_df <- readRDS("../rda_data/MSK_geneList.rda")
    #   } else if (dpc_selected == 'NWU') {
    #     dpc_df <- readRDS("../rda_data/NWU_geneList.rda")
    #   } else if (dpc_selected == 'UCSF') {
    #     dpc_df <- readRDS("../rda_data/UCSF_geneList.rda")
    #   } else if (dpc_selected == NULL) {
    #     
    #   }
    #   dpc_df <- dpc_df %>%
    #     rename(genes = 1)
    #   subsetGenesMetaDataDf_rowsOnly(dpc_df$genes)
    # })
    
    morphicGenesMetaDataTable <- reactive({
      dpc_selected <- input$select_dpcs
      dpc_gene_lists <- getGeneListsFromSelect(dpc_selected)
      genesMetaDataDf_data <- subsetGenesMetaDataDf_rowsOnly(dpc_gene_lists)
    })
    
    # This will prefer the File Upload over the Text Input
    # Need to handle headers in files
    # Need to handle multiple file uplaods 
    # Add selection from morphic DPC gene lists
    # tableData <- reactive({
    #   if(is.null(input$geneList_fileUpload) && input$geneList_textInput == '')# && input$select_dpcs == 'none')
    #   {
    #     genesMetaDataDf_data[1:5,1:10]
    #   }
    #   else if (!is.null(input$geneList_fileUpload))
    #   {
    #     file <- input$geneList_fileUpload
    #     path <- file$name
    #     if (length(path) > 1) {
    #       unified_and_list <- getGeneList_multiFileInput(file)
    #       gene_list <- unified_and_list[[1]]
    #       genesMetaDataDf_data <- subsetGenesMetaDataDf_rowsOnly(gene_list)
    #     } else {
    #       gene_list <- getGeneList_FromFile(file)
    #       genesMetaDataDf_data <- subsetGenesMetaDataDf_rowsOnly(gene_list)
    #     }
    #   }
    #   else if (input$geneList_textInput != '') 
    #   {
    #     gene_list <- parseTextAreaInput(input$geneList_textInput)
    #     genesMetaDataDf_data <- subsetGenesMetaDataDf_rowsOnly(gene_list)
    #   } 
    #   else if (input$select_dpcs != 'none') 
    #   {
    #     # ALlow multiple choice
    #     morphicGenesMetaDataTable()
    #   }
    # })
    
    tableData <- reactive({
      dpc_selected <- input$select_dpcs
      if (length(dpc_selected) >= 1) {
        morphicGenesMetaDataTable()
      } else {
        # Display custom files or text
        if (!is.null(input$geneList_fileUpload))
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
        else if (input$geneList_textInput != '')
        {
          gene_list <- parseTextAreaInput(input$geneList_textInput)
          genesMetaDataDf_data <- subsetGenesMetaDataDf_rowsOnly(gene_list)
        }
        else if (input$view_all_protein_coding_genes == TRUE) {
          genesMetaDataDf_data
        }
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
          dom = "Bflrtip",
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

# Example app
# library(shiny)
# library(DT)
# library(plotly)
# library(tools)
# library(stringr)
# library(dplyr)
# 
# # Auxiliary 
# source("../R/aux_app_funcs.R")
# 
# # Modules
# source("./genesMetaData_mod.R")
# 
# #genesMetaDataDf_data <<- readRDS("../annotated_all_protein_coding_genes_191023.rda")
# genesMetaDataDf_data <<- readRDS("../rda_data/271023_all_protein_coding_genes.rda")
# 
# #---- ui
# ui <- fluidPage(
#   genesMetaDataTableUI("tables")
# )
# 
# #---- server
# server <- function(input, output, session) {
#   genesMetaDataTableServer("tables")
# }
# 
# shinyApp(ui, server)
# 
