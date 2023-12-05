library(shiny)
library(DT)
library(plotly)
library(tools)
library(stringr)
library(dplyr)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)

#---- ui
genesMetaDataTableUI <- function(id) {
  page_sidebar(
    sidebar = sidebar(
      width = 340,
      open = c('open'),
      uiOutput(
        NS(id,'current_table_view')
      ),
      hr(),
      textOutput(
        NS(id, "select_data_table")
      ),
      dropdown(label = "MorPhiC Gene lists",
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
      dropdown(label = "Input Gene list",
               icon = icon("table"),
               style = "bordered", 
               status = "primary", 
               width = "300px",
               size =  "sm",
               animate = animateOptions(
                 enter = animations$fading_entrances$fadeInLeftBig,
                 exit = animations$fading_exits$fadeOutLeft
               ),
               textOutput(
                 NS(id, "clear_morphic_msg")
               ),
               br(),
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
                 "Input gene list as text (separate gene names by , or ; or newline)"
               ),
               br(),
               textOutput(
                 NS(id, "or_2")
               ),
               br(),
               checkboxInput(
                 NS(id, 'view_all_protein_coding_genes'),
                 "View all protein coding genes (reduces performance)"
               )
      ),
      hr(),
      checkboxGroupInput(
        NS(id, "show_cols"),
        "Select meta data to display:",
        c('DPCs studying Gene', 'Gene IDs', 'Mouse data', 'Disease data', 'Cell line data - gene constraint metrics', 
          'Sequencing data - gene constraint metrics', 'Pantherdb protein data', 'Pathway data', 'Gene Ontology data'), 
        selected = c('DPCs studying Gene', 'Gene IDs', 'Mouse data', 'Disease data'),
        inline = FALSE
      )
    ),
    withSpinner(
      DTOutput(
        NS(id, "genesMetaDataDf")
      ),
      type = getOption("spinner.type", default = 1),
      color = getOption("spinner.color", default = "#0275D8"),
      size = getOption("spinner.size", default = 1),
      color.background = getOption("spinner.color.background"),
      custom.css = FALSE,
      proxy.height = NULL,
      id = NULL,
      image = NULL,
      image.width = NULL,
      image.height = NULL,
      hide.ui = TRUE
    )
  )
}

#---- server
genesMetaDataTableServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$current_table_view_header <- renderUI({
      header <- 'Current view:'
    })
    
    output$current_table_view_body <- renderUI({
      dpc_selected <- input$select_dpcs
      if (length(dpc_selected) >= 1) {
        body <- dpc_selected
      } 
      else {
        # Display custom files or text
        if (!is.null(input$geneList_fileUpload))
        {
          file <- input$geneList_fileUpload
          body <- file$name
        }
        else if (input$geneList_textInput != '')
        {
          body <- 'Text input gene list'
        }
        else if (input$view_all_protein_coding_genes == TRUE) {
          body <- 'All protein coding genes'
          # add loading bar
        } 
        else {
          body <- ''
        }
      }
      HTML(glue("<p>{body}</p>"))
    })
    
    output$current_table_view <- renderUI({
      header <- 'Current view: '
      
      dpc_selected <- input$select_dpcs
      if (length(dpc_selected) >= 1) {
        body <- dpc_selected
        body <- paste(body, collapse = ", ")
        
      } 
      else {
        # Display custom files or text
        if (!is.null(input$geneList_fileUpload))
        {
          file <- input$geneList_fileUpload
          body <- file$name
          body <- paste(body, collapse = ", ")
          
        }
        else if (input$geneList_textInput != '')
        {
          body <- 'Text input gene list'
        }
        else if (input$view_all_protein_coding_genes == TRUE) {
          body <- 'All protein coding genes'
          # add loading bar
        } 
        else {
          body <- 'No genes selected'
        }
      }
      HTML(glue("<p>{header} <br> {body}</p>"))
    })
    
    output$clear_morphic_msg <- renderText({
      msg <- 'Note: unselect all Morphic Data before uploading custom data'
    })
    
    output$show_selected_dpcs_header <- renderText({
      dpc_selected <- input$select_dpcs
      if (length(dpc_selected) >= 1) {
        header <- 'Currently viewing: Gene Lists for MorPhiC DPCs:'
      } 
      else {
        # Display custom files or text
        if (!is.null(input$geneList_fileUpload))
        {
          header <- 'Currently viewing: Custom gene list'
        }
        else if (input$geneList_textInput != '')
        {
          header <- 'Currently viewing: Custom gene list'
        }
        else if (input$view_all_protein_coding_genes == TRUE) {
          header <- 'Currently viewing: All protein coding genes'
          # add loading bar
        } 
        else {
          header <- ''
        }
      }
    })
    
    output$show_selected_dpcs <- renderText({
      input$select_dpcs
    })
    
    output$select_data_table <- renderText('Select or upload Gene list:')
    
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
    
    output$or <- renderText('or')
    output$or_2w <- renderText('or')
    
    getHiddenColumns_shiny <- reactive({
      visible_columns <- getHiddenColumns_dpc_col(input$show_cols)
    })
    
    morphicGenesMetaDataTable <- reactive({
      dpc_selected <- input$select_dpcs
      dpc_gene_lists <- getGeneListsFromSelect(dpc_selected)
      genesMetaDataDf_data <- subsetGenesMetaDataDf_rowsOnly(dpc_gene_lists)
    })
    
    tableData <- reactive({
      dpc_selected <- input$select_dpcs
      if (length(dpc_selected) >= 1)
      {
        morphicGenesMetaDataTable()
      }
      else
      {
        # Display custom files or text
        if (!is.null(input$geneList_fileUpload))
        {
          file <- input$geneList_fileUpload
          path <- file$name
          if (length(path) > 1)
          {
            unified_and_list <- getGeneList_multiFileInput(file)
            gene_list <- unified_and_list[[1]]
            genesMetaDataDf_data <- subsetGenesMetaDataDf_rowsOnly(gene_list)
          } else
          {
            gene_list <- getGeneList_FromFile(file)
            genesMetaDataDf_data <- subsetGenesMetaDataDf_rowsOnly(gene_list)
          }
        }
        else if (input$geneList_textInput != '')
        {
          gene_list <- parseTextAreaInput(input$geneList_textInput)
          genesMetaDataDf_data <- subsetGenesMetaDataDf_rowsOnly(gene_list)
        }
        else if (input$view_all_protein_coding_genes == TRUE)
        {
          genesMetaDataDf_data
        }
        else
        {
          #This is to produce a message from DT without throwing an error
          genesMetaDataDf_data <- subsetGenesMetaDataDf_rowsOnly(c('FAKE'))
        }
      }
    })
    
    
    
    # Add columns for Morphic DPCs to see their genes of interest
    output$genesMetaDataDf <- renderDT({
      localFileNameDownload <- 'local_file_download'
      datatable(
        tableData(),
        class = 'cell-border stripe nowrap',
        container = headers,
        rownames = FALSE,
        plugins = "ellipsis",
        filter = 'top',
        selection = 'none',
        extensions = c("Buttons"),
        options = list(
          dom = "Bfrtip",
          # Unsure if this actually speeds up the tooltip
          initComplete = JS(
            "function(settings, json) {",
            "$('[data-toggle=\"tooltip\"]').tooltip({ delay: { show: 0, hide: 0 } });",
            "}"
          ),
          columnDefs = list(
            list(
              targets = "_all",
              render = JS("$.fn.dataTable.render.ellipsis(17, true)")
            ),
            list(
              targets = getHiddenColumns_shiny(),
              visible = FALSE
            )
          ),
          buttons =  list(
            list(
              extend = 'collection',
              buttons = list(list(extend = "excel", text = "excel", filename = localFileNameDownload,
                                  exportOptions = list(
                                    modifier = list(page = "all", search = "applied"),
                                    orthogonal = "export",
                                    columns = ":visible"
                                  )
              ),
              list(extend='csv', 
                   text = "csv",
                   filename = localFileNameDownload,
                   exportOptions = list(
                     modifier = list(page = "all", search = "applied"),
                     orthogonal = "export",
                     columns = ":visible"
                   )
              ),
              list(extend='pdf',
                   text = "pdf", 
                   filename= localFileNameDownload,
                   exportOptions = list(
                     modifier = list(page = "all", search = "applied"),
                     orthogonal = "export",
                     columns = ":visible"
                   )
              )
              ),
              text = 'Download current view'
            )),
          pageLength = 100,
          scrollX = TRUE
        )
      )
    }, server = TRUE)
    
    
    # New df 27/11/23
    headers <- htmltools::withTags(table(
      class = 'display',
      thead(
        class = 'table_entire_header',
        tr(
          class = 'table_top_header',
          th(colspan = 1, 'DPCs studying Gene'),
          th(colspan = 10, 'Gene Identifiers'),
          th(colspan = 6, 'Mouse data (IMPC & MGI)'),
          th(colspan = 4, 'Disease Data: OMIM'),
          th(colspan = 4, 'Disease Data: DDGene2Phenotype'),
          th(colspan = 5, 'Constraint Metrics: Cell Line'),
          th(colspan = 15, 'Constraint Metrics: Sequencing Data'),
          th(colspan = 5, 'Panther DB'),
          th(colspan = 6, 'Gene Ontology'),
          th(colspan = 2, 'Reactome')
        ),
        tr(
          class = 'table_bottom_header',
          th('DPCs'),
          th('Gene Symbol'),
          th('Gene Name'),
          th('Alias Symbol'),
          th('HGNC ID'),
          th('UniProt ID'),
          th('Entrez ID'),
          th('Ensembl ID'),
          th('OMIM Gene ID'),
          th('OMIM Phenotype ID'),
          th('MGI ID'),
          
          th('MGI Viability'),
          th('IMPC Viability'),
          th('Phenotypes Homo'),
          th('Phenotypes Hetero'),
          th('Phenotypes Hemiz'),
          th('1:1 Ortholog'),
          
          th('Phenotype'),
          th('Mode of inheritance'),
          th('Gene lethality'),
          th('Earliest lethality Category'),
          th('Phenotype'),
          th('Confidence Category'),
          th('Allelic Requirement'),
          th('Organ Specificity List'),
          
          th('DepMap Mean Gene Effect Score'),
          th('MEF Bayes Factor'),
          th('MEF FDR'),
          th('Laminin Bayes Factor'),
          th('Laminin FDR'),
        
          th('OE LOF'),
          th('OE LOF Lower'),
          th('OE LOF Upper'),
          th('OE Missense'),
          th('OE Missense Lower'),
          th('OE Missense Upper'),
          th('Shet RGCME Mean'),
          th('Shet RGCME Lower'),
          th('Shet RGCME Upper'),
          th('Shet Posterior Mean'),
          th('Shet Posterior Lower 95'),
          th('Shet Posterior Upper 95'),
          th('Constraint Metrics: DOMINO'),
          th('Constraint Metrics: SCoNeS'),
          th('Mean AM Pathogenicity'),
          
          th('PANTHER Class ID'),
          th('PANTHER Class Term'),
          th('PANTHER Family ID'),
          th('PANTHER Family Term'),
          th('PANTHER Subfamily Term'),
          
          th('GO IDs: Biological Process'),
          th('GO Terms: Biological Process'),
          th('GO IDs: Molecular Function'),
          th('GO Terms: Molecular Function'),
          th('GO IDs: Cellular Component'),
          th('GO Terms: Cellular Component'),
          
          th('Reactome Pathway ID'),
          th('Reactome Pathway')
        )
      )
    ))
  })}
