if (!require(shiny)) install.packages(c("shiny", "shinydashboard", "DT", "plotly", "UpSetR", "ggplot2", "BiocManager", "enrichplot"))
# install.packages("BiocManager")
# BiocManager::install(version = "3.17")
library(BiocManager)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(UpSetR)
library(ggplot2)
library(rrvgo)
library(shinyjs)
library(enrichplot)
# LOAD TABLES-----------------------------------------------------------------------------------

app_data_table <- readRDS("./rda_data/app_data_table.rda")
app_data_table[app_data_table == "-"] <- ""
#colnames(app_data_table) <- gsub("_", " ", colnames(app_data_table))
data_info_tables <- readRDS("./rda_data/data_info_tables.rda")


# APP CODE -----------------------------------------------------------------------------------

## UI - HEADER -----------------------------------------------------------------------------------
header <- dashboardHeader(
  title = tags$a(href='https://morphic.bio/',
                tags$img(src='morphiclogo.png',style = "margin-left: -20px;"),
                )
)

## UI - SIDEBAR-----------------------------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("MorPhiC Genes", tabName = "genes", icon = icon("table")),
    menuItem("Visualisations", tabName = "visualisations", icon = icon("chart-bar")),
    menuItem("Data info", tabName = "data_info", icon = icon("info")),
    menuItem("About MorPhiC", tabName = "about_morphic", icon = icon("magnifying-glass"))

  )
  # disable = TRUE
)

## UI - BODY-----------------------------------------------------------------------------------
body <- dashboardBody(
  
  fillPage = TRUE,

  tabItems(
    tabItem(
      ## GENE TABLE-----------------------------------------------------------------------------------

      tabName = "genes",
      fluidRow(
        # UI part with the new component
        box(
          width = 12,
          solidHeader = TRUE,
          title = "Search Gene",
          status = "primary",
          textInput("gene_search", label = NULL, placeholder = "DLX3", width = 300),
          textOutput("validate_gene"),
          checkboxInput("view_summary", "View gene summary"),
          conditionalPanel(
            condition = "input.view_summary",
            fluidRow(
              column(
                width = 12,
                box(
                  width =12,
                  title = textOutput("gene_name_title"),
                  status = "info",
                  # Display invalid gene message if the gene is not valid
                  uiOutput("invalid_gene_message"),
                  fluidRow(
                    column(width = 6, uiOutput("centers_sumtab")),
                    column(width = 6, uiOutput("gene_aliases_sumtab"))
                  ),
                  fluidRow(
                    column(width = 6, uiOutput("omim_sumtab")),
                    column(width = 6, uiOutput("gene_constraint_metrics_sumtab"))
                  ),
                  fluidRow(
                    column(width = 6, uiOutput("panther_sumtab")),
                    column(width = 6, uiOutput("viability_sumtab"))
                  )
                )
              ),

            )
          )
        )

      ),
      fluidRow(
        box(
          width = 12,
          title = "Select Data Sources",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          fluidRow(
            column(width = 12,
                   checkboxGroupInput("data_sources", label = NULL, inline = TRUE,
                                      choices = c("Gene Identifiers", "Data Production Centers", "IMPC Mouse Model Data",
                                                  "Gene Constraint Metrics", "Disease Data",
                                                  "Gene Ontology", "PANTHERdb Protein Data", "Reactome Pathway Data"),
                                      selected = c("Gene Identifiers", "Data Production Centers")),
            )
          )
        )),

      fluidRow(
        
        box(
          solidHeader = TRUE,
          width = 12,
          title = "Browse MorPhiC Gene list Data",
          status = "primary",
          DTOutput("genes_table")
        )
      ),
    ),
    tabItem(
      ##DATA INFORMATION TAB-----------------------------------------------------------------------------------


      tabName = "data_info",
      tags$div(class = "banner",
               h2("Meta Data Information"),
      ),
      fluidRow(
        column(width = 12, h2("Gene Identifiers"), DTOutput("table_gene_identifiers"))
      ),
      fluidRow(
        column(width = 12, h2("Center Info"), DTOutput("table_center_info"))
      ),
      fluidRow(
        column(width = 12, h2("IMPC"), DTOutput("table_impc"))
      ),
      fluidRow(
        column(width = 12, h2("DepMap"), DTOutput("table_depmap"))
      ),
      fluidRow(
        column(width = 12, h2("ITV Metrics"), DTOutput("table_itv_metrics"))
      ),
      fluidRow(
        column(width = 12, h2("DDG2P"), DTOutput("table_ddg2p"))
      ),
      fluidRow(
        column(width = 12, h2("OMIM"), DTOutput("table_omim"))
      ),
      fluidRow(
        column(width = 12, h2("GO"), DTOutput("table_go"))
      ),
      fluidRow(
        column(width = 12, h2("Panther"), DTOutput("table_panther"))
      ),
      fluidRow(
        column(width = 12, h2("Reactome"), DTOutput("table_reactome"))
      )
    ),
    tabItem(
      # ABOUT MORPHIC TAB-----------------------------------------------------------------------------------

      tabName = "about_morphic",
      # fluidRow(
      #   box(
      #     width = 12,
      #     title = "What is MorPhiC",
      #     status = "info",
      #     solidHeader = TRUE,
      #     uiOutput("morphic_description")
      #   )
      # ),

      tags$style(HTML("
        .banner {
          background-color: #001F3F;
          color: white;
          padding: 10px;
          text-align: center;
          font-weight: bold;
        }
      ")),
      tags$div(class = "banner",
               h2("What is MorPhiC")
               ),
               hr(),
      fluidRow(
      box(
        width = 12,
        uiOutput("morphic_description")
      )),


      tags$div(class = "banner",
               h2("Data Production Centers (DPCs)"),
      ),
      hr(),
      fluidRow(
        box(
          width = 3,
          title = "JAX",
          solidHeader = TRUE,
          uiOutput("jax_description")
        ),
        box(
          width = 3,
          title = "MSK",
          solidHeader = TRUE,
          uiOutput("msk_description")
        ),
        box(
          width = 3,
          title = "NWU",
          solidHeader = TRUE,
          uiOutput("nwu_description")
        ),
        box(
          width = 3,
          title = "UCSF",
          solidHeader = TRUE,
          uiOutput("ucsf_description")
        )
      )
      ),
    ## DATA VISUALISATIONS TAB-----------------------------------------------------------------------------------
    tabItem(
      tabName = "visualisations",
      ##### PUT IN COLUMN OF WIDTH 9
      tabsetPanel(
        tabPanel(HTML("DPCs Genes of Interest<br><span style='font-size: 14px;'>&nbsp;</span>"), id = "upset_plot_tab",
                 fluidRow(
                   column(
                     width = 9,
                     uiOutput("upset_text")
                   )
                 ),
                 hr(),
                 fluidRow(
                   column(
                     width =9,
                     plotOutput("upsetplot", height = "60vh")
                   )
                 )
        ),

        tabPanel(HTML("Mouse Model Data<br><span style='font-size: 14px;'>&nbsp;</span>"), id = "viability_tab",
                 fluidRow(
                   column(
                     width = 9,
                     uiOutput("viability_text")
                   )
                 ),
                 hr(),
                 fluidRow(
                   column(width = 9, plotlyOutput("viability_plot", height = "60vh")),
                   column(
                     width = 3,
                     box(
                       width = 12,
                       title = "Options",
                       status = "primary",
                       selectInput(
                         "center_viability",
                         "Select gene list:",
                         choices = c("all morphic", "jax", "msk", "nwu", "ucsf", "all centers"),
                         selected = "all morphic"  # Set the default selected option
                       )
                     )
                   )
                 ),
                 hr(),
                 fluidRow(
                   box(
                     width = 9,
                     title = 'IMPC Mouse Model Data',
                     DTOutput('impc_data_table')
                   )
                 )
        ),

        tabPanel(HTML("Disease Data<br><span style='font-size: 14px;'>&nbsp;</span>"), id = "omim_tab",
                 fluidRow(
                   column(
                     width = 9,
                     uiOutput("disease_text")
                   )
                 ),
                 hr(),
                 fluidRow(
                   column(width = 9, plotlyOutput("omim_plot", height = "60vh")),
                   column(
                     width = 3,
                     box(
                       width = 12,
                       title = "Options",
                       status = "primary",
                       selectInput(
                         "center_omim",
                         "Select gene list:",
                         choices = c("all morphic", "jax", "msk", "nwu", "ucsf", "all centers"),
                         selected = "all morphic"  # Set the default selected option
                       )
                     )
                   )
                 ),
                 hr(),
                 fluidRow(
                   box(
                     width = 9,
                     title = 'OMIM Disease Data',
                     DTOutput('omim_data_table')
                   )
                 )
        ),

        tabPanel(HTML("PANTHERdb<br><span style='font-size: 14px;'>(Protein Data)</span>"), id = "panther_tab",
                 fluidRow(
                   column(
                     width = 9,
                     uiOutput("panther_text")
                   )
                 ),
                 hr(),
                 fluidRow(
                   conditionalPanel(
                     condition = "input.center_panther != 'all centers'",
                     column(width = 9, plotlyOutput("panther_plot", height = "60vh"))
                     ),
                   conditionalPanel(
                     condition = "input.center_panther == 'all centers'",
                     column( width = 9,
                       fluidRow(width = 12,
                              splitLayout(cellWidths = c("50%", "50%"),
                                          plotlyOutput("panther_jax_plot"), plotlyOutput("panther_msk_plot"))),
                       fluidRow(width = 12,
                              splitLayout(cellWidths = c("50%", "50%"),
                                          plotlyOutput("panther_nwu_plot"), plotlyOutput("panther_ucsf_plot")))
                     )
                   ),
                   column(
                     width = 3,
                     box(
                       width = 12,
                       title = "Options",
                       status = "primary",
                       selectInput(
                         "center_panther",
                         "Select gene list:",
                         choices = c("all morphic", "jax", "msk", "nwu", "ucsf", "all centers"),
                         selected = "all morphic"  # Set the default selected option
                       )
                     )
                   )
                 ),
                 hr(),
                 fluidRow(
                   box(
                     width = 9,
                     title = 'PANTHERdb Data',
                     DTOutput('panther_data_table')
                   )
                 )
        ),

        tabPanel(HTML("Cellular Core Essentiality Data<br><span style='font-size: 14px;'>&nbsp;</span>"), id = "cell_essentiality_plot_tab",
                 fluidRow(
                   column(
                     width = 9,
                     uiOutput("cell_essential_text")
                   )
                 ),
                 hr(),
                 fluidRow(
                   column(width = 9, plotlyOutput("cell_essentiality_plot", height = "60vh")),
                   column(
                     width = 3,
                     box(
                       width = 12,
                       title = "Options",
                       status = "primary",
                       selectInput(
                         "center_cell_essentiality_barchart",
                         "Select gene list:",
                         choices = c("all morphic", "jax", "msk", "nwu", "ucsf", "all centers"),
                         selected = "all morphic"  # Set the default selected option
                       )
                     )
                   )
                 ),
                 hr(),
                 fluidRow(
                   box(
                     width = 9,
                     title = 'Cellular Essentiality Data',
                     DTOutput('cell_essentiality_data_table')
                   )
                 )
        ),

        tabPanel(HTML("Gene Onotology<br><span style='font-size: 14px;'>(Semantic Similarity Analysis)</span>"), id = "go_scatter_tab",
                 fluidRow(
                   column(
                     width = 9,
                     uiOutput("go_text")
                   )
                 ),
                 hr(),
                 fluidRow(
                   conditionalPanel(
                     condition = "input.center_go != 'all centers'",
                     column(width = 9, div(style='overflow-x: scroll', plotlyOutput("go_scatter", height = "60vh")))
                   ),
                   conditionalPanel(
                     condition = "input.center_go == 'all centers'",
                     column( width = 9,
                             fluidRow(width = 12,
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  div(style='overflow-x: scroll', plotlyOutput("go_jax_plot")), plotlyOutput("go_msk_plot"))),
                             fluidRow(width = 12,
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  plotlyOutput("go_nwu_plot"), plotlyOutput("go_ucsf_plot")))
                     )
                   ),
                   column(
                     width = 3,
                     box(
                       width = 12,
                       title = "Options",
                       status = "primary",
                       selectInput(
                         "center_go",
                         "Select gene list:",
                         choices = c("all morphic", "jax", "msk", "nwu", "ucsf", "all centers"),
                         selected = "all morphic"  # Set the default selected option
                       )
                     )
                   )
                 ),
                 hr(),
                 fluidRow(
                   box(
                     width = 9,
                     title = 'Gene Ontology Data',
                     DTOutput('go_data_table')
                   )
                 )
        ),

        tabPanel(HTML("Reactome<br><span style='font-size: 14px;'>(Enrichment Analysis)</span>"), id = "reactome_emapplot_tab",
                 fluidRow(
                   column(
                     width = 9,
                     uiOutput("reactome_text")
                   )
                 ),
                 hr(),
                 fluidRow(
                   conditionalPanel(
                     condition = "input.center_reactome != 'all centers'",
                     column(width = 9, plotOutput("reactome_emapplot", height = "60vh"))
                   ),
                   conditionalPanel(
                     condition = "input.center_reactome == 'all centers'",
                     column( width = 9,
                             fluidRow(width = 12,
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  plotOutput("jax_reactome_emapplot"), plotOutput("msk_reactome_emapplot"))),
                             fluidRow(width = 12,
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  plotOutput("nwu_reactome_emapplot"), plotOutput("ucsf_reactome_emapplot")))
                     )
                   ),
                   column(
                     width = 3,
                     box(
                       width = 12,
                       title = "Options",
                       status = "primary",
                       selectInput(
                         "center_reactome",
                         "Select gene list:",
                         choices = c("all morphic", "jax", "msk", "nwu", "ucsf", "all centers"),
                         selected = "all morphic"  # Set the default selected option
                       )
                     )
                   )
                 ),
                 hr(),
                 fluidRow(
                   box(
                     width = 9,
                     title = 'Reactome Pathway Data',
                     DTOutput('reactome_data_table')
                   )
                 )
        ),


        tabPanel(HTML("Cellular Core Essentiallity<br><span style='font-size: 14px;'>(Cell Line Data)</span>"), id = "cell_essentiality_boxplots_tab",
                 fluidRow(
                   column(
                     width = 9,
                     uiOutput("cell_line_text")
                   )
                 ),
                 hr(),
                 fluidRow(
                   column(
                     width = 9,
                     fluidRow(
                       column(12, box(plotlyOutput("cell_essentiality_boxplot_depmap", height = "40vh"), title = "DepMap Data", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12)) # Adjust height as needed
                     ),
                     hr(),
                     fluidRow(
                       column(12, box(plotlyOutput("cell_essentiality_boxplot_mef", height = "40vh"), title = "h1 hpsc mef", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12))    # Adjust height as needed
                     ),
                     hr(),
                     fluidRow(
                       column(12, box(plotlyOutput("cell_essentiality_boxplot_laminin", height = "40vh"), title = "h1 hpsc laminin", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12))    # Adjust height as needed
                     ),
                     hr()


                   ),
                   column(
                     width = 3,
                     box(
                       width = 12,
                       title = "Options",
                       status = "primary",
                       selectInput(
                         "center_cell_essentiality_boxplots",
                         "Select gene list:",
                         choices = c("all morphic", "jax", "msk", "nwu", "ucsf", "all centers"),
                         selected = "all morphic"  # Set the default selected option
                       ),
                       selectInput(
                         "cell_essential_depmap_significance_threshold",
                         "Select significance threshold for DepMap data:",
                         choices = c("-0.5", "-1"),
                         selected = "-0.5"  # Set the default selected option
                       ),
                       textInput("search_gene_symbol_cell", "Search Gene symbol:", placeholder = "DLX3"),
                       checkboxInput(inputId = "compare_protein_coding_genes", label = "Compare to all protein coding genes")
                     )
                   )
                   )

        ),

        tabPanel(HTML("Cellular Core Essentiallity<br><span style='font-size: 14px;'>(Sequencing Data)</span>"), id = "gene_constraint_metrics_tab",
                 fluidRow(
                   column(
                     width = 9,
                     uiOutput("cell_sequencing_text")
                   )
                 ),
                 hr(),
                 fluidRow(
                   column(
                     width = 9,
                     fluidRow(column(12, box(plotlyOutput("gene_constraint_oe_lof", height = "40vh"), title = "Loss-of-function metrics (O/E score)", solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, width = 12))),
                     hr(),
                     fluidRow(column(12, box(plotlyOutput("gene_constraint_oe_mis", height = "40vh"), title = "Missense metrics (Z-score)", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12))),
                     hr(),
                     fluidRow(column(12, box(plotlyOutput("gene_constraint_shet_rgcme", height = "40vh"), title = "Shet score", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12))),
                     hr(),
                     fluidRow(column(12, box(plotlyOutput("gene_constraint_shet_posterior", height = "40vh"), title = "Shet posterior score", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12))),
                     hr(),
                     fluidRow(column(12, box(plotlyOutput("gene_constraint_domino", height = "40vh"), title = "DOMINO score", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12))),
                     hr(),
                     fluidRow(column(12, box(plotlyOutput("gene_constraint_scones", height = "40vh"), title = "SCoNeS score", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 12))),
                     hr(),
                   ),
                   column(
                     width = 3,
                     box(
                       width = 12,
                       title = "Options",
                       status = "primary",
                       selectInput(
                         "center_gene_constraint_seq",
                         "Select gene list:",
                         choices = c("all morphic", "jax", "msk", "nwu", "ucsf", 'all centers'),
                         selected = "all morphic"  # Set the default selected option
                       ),
                       textInput("search_gene_symbol_seq", "Search Gene:", placeholder = "DLX3"),
                       checkboxInput(inputId = "compare_protein_coding_genes_gene_constraint_seq", label = "Compare to all protein coding genes")
                     )
                   )
                 )

        )
      )


    )

  )
)

## DEFINE UI-----------------------------------------------------------------------------------
ui <- dashboardPage(header, sidebar, body)


## DEFINE SERVER-----------------------------------------------------------------------------------
server <- function(input, output) {

  # FILTER TABLE BASED ON SEARCH-----------------------------------------------------------------------------------

  # Reactive expression to filter data based on gene_search input
  filtered_data <- reactive({
    search_term <- toupper(toupper(input$gene_search))
    #data_subset <- app_data_table
    data_subset <- app_data_table

    # Check if the search term is empty or NULL
    if (is.null(search_term) || nchar(trimws(search_term)) == 0) {
      return(data_subset)  # Return the entire dataframe
    }

    # Check if the search term is an HGNC ID (e.g., HGNC:1234)
    if (grepl("^HGNC:\\d+$", search_term)) {
      data_subset <- data_subset[data_subset$hgnc_id == search_term, ]
    } else {
      # Check if the search term is an Entrez ID (e.g., ENSG12345)
      if (grepl("^ENSG\\d+$", search_term)) {
        data_subset <- data_subset[data_subset$ensembl_id == search_term, ]
      } else {
        # Check if the search term is an MGI ID (e.g., MGI:12345)
        if (grepl("^MGI:\\d+$", search_term)) {
          data_subset <- data_subset[data_subset$mgi_id == search_term, ]
        } else {
          # Check if the search term is a gene symbol (e.g., Symbol_ABC)
          data_subset <- data_subset[data_subset$gene_symbol == search_term, ]
        }
      }
    }
    data_subset
  })

  #RENDER TABLE WITH HEADERS-----------------------------------------------------------------------------------

  headers = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(colspan = 3, 'Gene Identifiers'),
        th(colspan = 4, 'MorPhiC DPCs'),
        th(colspan = 5, 'IMPC'),
        th(colspan = 3, 'Constraint Metrics: DepMap'),
        th(colspan = 3, 'Constraint Metrics: MEF hPSCs'),
        th(colspan = 3, 'Constraint Metrics: Laminin hPSCs'),
        th(colspan = 3, 'Constraint Metrics: O/E LOF'),
        th(colspan = 3, 'Constraint Metrics: O/E Missense'),
        th(colspan = 3, 'Constraint Metrics: Shet RGC-ME'),
        th(colspan = 3, 'Constraint Metrics: Shet posterior'),
        th(colspan = 1, 'Constraint Metrics: DOMINO'),
        th(colspan = 1, 'Constraint Metrics: SCoNeS'),
        th(colspan = 4, 'Disease Data: DDGene2Phenotype'),
        th(colspan = 4, 'Disease Data: OMIM'),
        th(colspan = 2, 'GO: Biological Process'),
        th(colspan = 2, 'GO: Molecular Function'),
        th(colspan = 2, 'GO: Cellular Component'),
        th(colspan = 2, 'PANTHER Class'),
        th(colspan = 2, 'PANTHER Family'),
        th(colspan = 1, 'PANTHER Subfamily'),
        th(colspan = 2, 'Reactome')
      ),
      tr(
        th('Gene Symbol'), th('HGNC ID'), th('MGI ID'),
        th('JAX'), th('MSK'), th('NWU'), th('UCSF'),
        th('Viability'), th('One2one ortholog'), th('Phenotypes homozygote'), th('Phenotypes heterozygote'), th('Phenotypes hemizygote'),
        th('Mean gene effect score'), th('Cellular core essential (< -0.5)'), th('Cellular core essential (< -1)'), th('Bayes Factor'), th('False Discovery Rate'),  th('Core essential/Core non-essential'), th('Bayes Factor'), th('False Discovery Rate'),  th('Core essential/Core non-essential'), th('Score'), th('Lower score'), th('Upper score'), th('Score'), th('Lower score'), th('Upper score'), th('Mean'), th('Lower'), th('Upper'), th('Mean'), th('Lower 95'), th('Upper 95'), th('Score'), th('Score'),
        th('Disease name'), th('Confidence category'), th('Allelic requirement'), th('Organ specificity'),
        th('Phenotype'), th('Mode of inheritance'), th('Gene lethality'), th('Earliest lethality category'),
        th('ID'), th('Term'), th('ID'), th('Term'), th('ID'), th('Term'),
        th('ID'), th('Term'), th('ID'), th('Term'), th('Term'),
        th('Path ID'), th('Path name')
      )
    )
  ))

  # Function to get hidden column indices based on selected data sources
  getHiddenColumns <- function(selected_sources) {
    source_to_columns <- list(
      "Gene Identifiers" = 0:2,
      "Data Production Centers" = 3:6,
      "IMPC Mouse Model Data" = 7:11,
      "Gene Constraint Metrics" = 12:34,
      "Disease Data" = 35:42,
      "Gene Ontology" = 43:48,
      "PANTHERdb Protein Data" = 49:53,
      "Reactome Pathway Data" = 54:55
    )

    hidden_columns <- unlist(lapply(selected_sources, function(source) {
      source_to_columns[[source]]
    }))

    all_columns <- 0:55
    visible_columns <- setdiff(all_columns, hidden_columns)
    return(visible_columns)
  }

  observe({
    # Update the table based on selected data sources
    visible_columns <- getHiddenColumns(input$data_sources)

    output$genes_table <- renderDT(server=FALSE,{
      datatable(
        filtered_data(),
        plugins = "ellipsis",
        extensions = 'Buttons',
        class = "display nowrap",
        container = headers,
        filter = "top",
        rownames = FALSE,
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE,
          searching = TRUE,
          dom = 'Bfrtip',
          buttons = list(
            list(extend = "csv", text = "Download Full Table", filename = "Full_data",
                 exportOptions = list(
                   modifier = list(page = "all"),
                   orthogonal = "export"
                 )
            )
          ),
          columnDefs = list(
            list(
              targets = visible_columns,
              visible = FALSE
            ),
            list(
              targets = "_all",
              render = JS("$.fn.dataTable.render.ellipsis(17, false)")
            )
          )
        )
      )
    })
  })


  #  # GENE CARDS-----------------------------------------------------------------------------------

  # gene_name = searched gene
  gene_name <- reactive({
    toupper(toupper(input$gene_search))
  })

  # Function to detect input ID type and get the appropriate column name
  detect_input_type <- function(gene_search) {
    hgnc_id_regex <- "^HGNC:\\d+$"
    gene_symbol_regex <- "^[A-Za-z0-9]+$"

    if (grepl(hgnc_id_regex, gene_search)) {
      return("hgnc_id")
    } else if (grepl(gene_symbol_regex, gene_search)) {
      return("gene_symbol")
    } else {
      return(NULL)
    }
  }

  # Function to check if the gene exists in the dataframe
  gene_exists <- function(gene_search, input_type) {
    if (!is.null(input_type)) {
      gene_data <- app_data_table
      return(toupper(toupper(input$gene_search)) %in% gene_data[[input_type]])
    }
    return(FALSE)
  }

  # Valid gene reactive variable
  validGene <- reactive({
    gene_exists(toupper(input$gene_search), detect_input_type(toupper(input$gene_search)))
  })

  # Generate title
  output$gene_name_title <- renderText({
    paste(gene_name(), "Summary Information")
  })

  # UI component to display a message until a valid gene is entered
  output$invalid_gene_message <- renderUI({
    if (!validGene()) {
      box(
        width = 12,
        title = "Invalid Gene",
        status = "warning",
        "This gene is not contained within the MorPhiC gene list."
      )
    }
  })

  output$viability_sumtab <- renderUI({
    app_data_table <- readRDS("./rda_data/app_data_table.rda")
    
    if (validGene()) {
      renderValueBox({
        input_type <- detect_input_type(toupper(input$gene_search))
        select_row <- app_data_table[
          app_data_table[[input_type]] == toupper(input$gene_search), ]

        viability <- select_row$viability_impc
        ortholog <- select_row$one2one_ortholog
        phenotype_het <- select_row$impc_phenotypes_heterozygote
        phenotype_homo <- select_row$impc_phenotypes_homozygote

        viability_sumtab_data <- viability

        valueBox(
          value = "IMPC Mouse Data",
          subtitle = viability_sumtab_data,
          icon = icon("shield-cat"),
          color = "blue",
        )
      })
    }
  })


  output$centers_sumtab <- renderUI({
    app_data_table <- readRDS("./rda_data/app_data_table.rda")
    
    if (validGene()) {
      renderValueBox({
        input_type <- detect_input_type(toupper(input$gene_search))
        select_row <- app_data_table[
          app_data_table[[input_type]] == toupper(input$gene_search), ]
        select_row_centers <- select_row[, c("JAX", "NWU", "MSK", "UCSF")]
        gene_list_result <- list()
        for (i in seq_along(1:4)) {
          if (select_row_centers[, i] != "-") {
            gene_list_result <- append(gene_list_result, colnames(select_row_centers[i]))
          }
        }
        valueBox(
          "DPCs with gene",
          gene_list_result,
          icon = icon("vials"),
          color = "blue",
        )
      })
    }
  })


  output$omim_sumtab <- renderUI({
    app_data_table <- readRDS("./rda_data/app_data_table.rda")
    
    if (validGene()) {
      renderValueBox({
        input_type <- detect_input_type(toupper(input$gene_search))
        select_row <- app_data_table[
          app_data_table[[input_type]] == toupper(input$gene_search), ]

        omim_phenotype <- select_row$omim_phenotype
        omim_moi <- select_row$omim_mode_of_inheritance
        omim_lethailty <- select_row$omim_gene_lethality
        organ <- select_row$ddd_organ_specificity

        disease_sumtab_data <- paste(
          omim_phenotype,
          omim_moi,
          omim_lethailty,
          organ,
          sep = "<br>"
        )

        valueBox(
          "Disease Data",
          HTML(disease_sumtab_data),
          icon = icon("hand-dots"),
          color = "blue",
        )
      })
    }
  })

  output$gene_aliases_sumtab <- renderUI({
    app_data_table <- readRDS("./rda_data/app_data_table.rda")
    
    if (validGene()) {
      renderValueBox({
        input_type <- detect_input_type(toupper(input$gene_search))
        select_row <- app_data_table[
          app_data_table[[input_type]] == toupper(input$gene_search), ]
        valueBox(
          "Gene synonyms",
          "under construction",
          icon = icon("hammer"),
          color = "blue",
        )
      })
    }
  })

  output$gene_constraint_metrics_sumtab <- renderUI({
    app_data_table <- readRDS("./rda_data/app_data_table.rda")
    
    if (validGene()) {
      renderValueBox({
        input_type <- detect_input_type(toupper(input$gene_search))
        select_row <- app_data_table[
          app_data_table[[input_type]] == toupper(input$gene_search), ]

        essential_label <- select_row$depmap_essential_1
        shet_rgcme_mean <- as.numeric(select_row$shet_rgcme_mean)
        oe_lof_upper <- as.numeric(select_row$oe_lof_upper)
        #
        # metrics <- append(metrics, paste("RGCME Mean:", signif(shet_rgcme_mean, 3)))
        # metrics <- append(metrics, paste("OE LOF Upper:", signif(oe_lof_upper, 3)))

        constraint_sumtab_data <- paste(
          "<p>Cellular core essential: ", essential_label, "</p>",
          "<p>Shet RGC-ME mean: ", shet_rgcme_mean, "</p>",
          "<p>gnomAD o/e lof upper: ", oe_lof_upper, "</p>",
          sep = ""
        )

        valueBox(
          "Gene constraint metrics",
          HTML(constraint_sumtab_data),
          icon = icon("flask"),
          color = "blue",
        )
      })
    }
  })

  output$panther_sumtab <- renderUI({
    app_data_table <- readRDS("./rda_data/app_data_table.rda")
    
    if (validGene()) {
      renderValueBox({
        input_type <- detect_input_type(toupper(input$gene_search))
        select_row <- app_data_table[
          app_data_table[[input_type]] == toupper(input$gene_search), ]

        class_term <- select_row$CLASS_TERM
        family_term <- select_row$FAMILY_TERM


        panther_sumtab_data <- paste(
          class_term,
          family_term,
          sep = "<br>"
        )

        valueBox(
          "Protein data",
          HTML(panther_sumtab_data),
          icon = icon("certificate"),
          color = "blue",
        )
      })
    }
  })


  #  # VISUALISATIONS-----------------------------------------------------------------------------------

  # UPSET
  gene_upset_plot <- readRDS("./rda_data/gene_upset_plot.rda")
  output$upsetplot <- renderPlot(gene_upset_plot)

  # VIABILITY
  # Render the Viability plot based on the selected center
  output$viability_plot <- renderPlotly({
    center <- input$center_viability

    if (center == "jax") {
      # Load JAX viability plot object (jax_plot_via_all.rda)
      jax_plot <- readRDS("./rda_data/jax_plot_via_all.rda")
      jax_plot
      # Return the plot object
    } else if (center == "msk") {
      # Load MSK viability plot object (msk_plot_via_all.rda)
      msk_plot <- readRDS("./rda_data/msk_plot_via_all.rda")
      # Replace with code to customize the MSK viability plot
      msk_plot  # Return the plot object
    } else if (center == "nwu") {
      # Load NWU viability plot object (nwu_plot_via_all.rda)
      nwu_plot <- readRDS("./rda_data/nwu_plot_via_all.rda")
      # Replace with code to customize the NWU viability plot
      nwu_plot  # Return the plot object
    } else if (center == "all morphic") {
      all_plot <- readRDS("./rda_data/all_morphic_plot_via_all.rda")
      # Replace with code to customize the NWU viability plot
      all_plot  # Return the plot object
    } else if (center == "all centers") {
      # all_centers_plot <- readRDS("./rda_data/impc_viability_all_centers_plot.rda")
      # 
      # all_centers_plot
      
      all_centers_plot <- readRDS("./rda_data/new_all_dpcs_viability_plot.rda")
      all_centers_plot
      
    } else {
      # Load UCSF viability plot object (ucsf_plot_via_all.rda)
      ucsf_plot <- readRDS("./rda_data/ucsf_plot_via_all.rda")
      # Replace with code to customize the UCSF viability plot
      ucsf_plot  # Return the plot object
    }
  })

  # Render the Cellular Essentiality plot based on the selected center
  output$cell_essentiality_plot <- renderPlotly({
    center <- input$center_cell_essentiality_barchart
    if (center == "jax") {
      # Load JAX cellular essentiality plot object (jax_plot_cells_all.rda)
      jax_plot <- readRDS("./rda_data/jax_plot_cells_all.rda")
      # Replace with code to customize the JAX cellular essentiality plot
      jax_plot  # Return the plot object
    } else if (center == "msk") {
      # Load MSK cellular essentiality plot object (msk_plot_cells_all.rda)
      msk_plot <- readRDS("./rda_data/msk_plot_cells_all.rda")
      # Replace with code to customize the MSK cellular essentiality plot
      msk_plot  # Return the plot object
    } else if (center == "nwu") {
      # Load NWU cellular essentiality plot object (nwu_plot_cells_all.rda)
      nwu_plot <- readRDS("./rda_data/nwu_plot_cells_all.rda")
      # Replace with code to customize the NWU cellular essentiality plot
      nwu_plot  # Return the plot object
    } else if (center == "all morphic") {
      all_plot <- readRDS("./rda_data/all_morphic_plot_cells_all.rda")
      # Replace with code to customize the NWU viability plot
      all_plot  # Return the plot object
    } else if (center == "all centers") {
      # all_centers_plot <- readRDS("./rda_data/impc_viability_all_centers_plot.rda")
      # 
      # all_centers_plot
      
      all_centers_plot <- readRDS("./rda_data/new_all_dpcs_essentiality_plotly.rda")
      
      all_centers_plot
      
    } else {
      # Load UCSF cellular essentiality plot object (ucsf_plot_cells_all.rda)
      ucsf_plot <- readRDS("./rda_data/ucsf_plot_cells_all.rda")
      # Replace with code to customize the UCSF cellular essentiality plot
      ucsf_plot  # Return the plot object
    }
  })

  # Render the OMIM plot based on the selected center
  output$omim_plot <- renderPlotly({
    center <- input$center_omim
    if (center == "jax") {
      # Load JAX OMIM plot object (jax_plot_omim_all_jax.rda)
      jax_plot <- readRDS("./rda_data/jax_plot_omim_all.rda")
      # Replace with code to customize the JAX OMIM plot
      jax_plot  # Return the plot object
    } else if (center == "msk") {
      # Load MSK OMIM plot object (msk_plot_omim_all_msk.rda)
      msk_plot <- readRDS("./rda_data/msk_plot_omim_all.rda")
      # Replace with code to customize the MSK OMIM plot
      msk_plot  # Return the plot object
    } else if (center == "nwu") {
      # Load NWU OMIM plot object (nwu_plot_omim_all_nwu.rda)
      nwu_plot <- readRDS("./rda_data/nwu_plot_omim_all.rda")
      # Replace with code to customize the NWU OMIM plot
      nwu_plot  # Return the plot object
    } else if (center == "all morphic") {
      all_plot <- readRDS("./rda_data/all_morphic_plot_omim_all.rda")
      # Replace with code to customize the NWU viability plot
      all_plot  # Return the plot object
    } else if (center == "all centers") {
      # all_centers_plot <- readRDS("./rda_data/omim_all_centers_plot.rda")
      # 
      # all_centers_plot
      
      all_centers_plot <- readRDS('./rda_data/new_all_dpcs_disease_plot.rda')
      all_centers_plot
    } else {
      # Load UCSF OMIM plot object (ucsf_plot_omim_all_ucsf.rda)
      ucsf_plot <- readRDS("./rda_data/ucsf_plot_omim_all.rda")
      # Replace with code to customize the UCSF OMIM plot
      ucsf_plot  # Return the plot object
    }
  })

  # Render the Panther plot based on the selected center
  output$panther_plot <- renderPlotly({
    center <- input$center_panther
    jax_plot <- readRDS("./rda_data/jax_plot_panther_all.rda")
    msk_plot <- readRDS("./rda_data/msk_plot_panther_all.rda")
    nwu_plot <- readRDS("./rda_data/nwu_plot_panther_all.rda")
    all_plot <- readRDS("./rda_data/all_morphic_plot_panther_all.rda")
    ucsf_plot <- readRDS("./rda_data/ucsf_plot_panther_all.rda")

    if (center == "jax") {
      # Load JAX Panther plot object (jax_plot_panther_all.rda)
      # Replace with code to customize the JAX Panther plot
      jax_plot  # Return the plot object
    } else if (center == "msk") {
      # Load MSK Panther plot object (msk_plot_panther_all.rda)
      # Replace with code to customize the MSK Panther plot
      msk_plot  # Return the plot object
    } else if (center == "nwu") {
      # Load NWU Panther plot object (nwu_plot_panther_all.rda)
      # Replace with code to customize the NWU Panther plot
      nwu_plot  # Return the plot object
    } else if (center == "all morphic") {
      # Replace with code to customize the NWU viability plot
      all_plot  # Return the plot object
    } else {
      # Load UCSF Panther plot object (ucsf_plot_panther_all.rda)
      # Replace with code to customize the UCSF Panther plot
      ucsf_plot  # Return the plot object
    }
  })

  output$panther_jax_plot <- renderPlotly({
    center <- input$center_panther
    if (center == "all centers") {
      jax_plot <- readRDS("./rda_data/jax_plot_panther_all.rda")
      jax_plot
    }
  })

  output$panther_msk_plot <- renderPlotly({
    center <- input$center_panther
    if (center == "all centers") {
      msk_plot <- readRDS("./rda_data/msk_plot_panther_all.rda")
      msk_plot
    }
  })

  output$panther_nwu_plot <- renderPlotly({
    center <- input$center_panther
    if (center == "all centers") {
      nwu_plot <- readRDS("./rda_data/nwu_plot_panther_all.rda")
      nwu_plot
    }
  })

  output$panther_ucsf_plot <- renderPlotly({
    center <- input$center_panther
    if (center == "all centers") {
      ucsf_plot <- readRDS("./rda_data/ucsf_plot_panther_all.rda")
      ucsf_plot
    }
  })

  # Render the Go plot based on the selected center
  output$go_scatter <- renderPlotly({
    center <- input$center_go
    if (center == "jax") {
      jax_plot <- readRDS("./rda_data/go_scatter_plots.rda")
      jax_plot <- ggplotly(jax_plot[[1]]) %>%
        layout(showlegend = FALSE)
    } else if (center == "nwu") {
      nwu_plot <- readRDS("./rda_data/go_scatter_plots.rda")
      nwu_plot <- ggplotly(nwu_plot[[2]]) %>%
        layout(showlegend = FALSE)
      }
    else if (center == "msk") {
      msk_plot <- readRDS("./rda_data/go_scatter_plots.rda")
      msk_plot <- ggplotly(msk_plot[[3]]) %>%
        layout(showlegend = FALSE)
      }
    else if (center == "all morphic") {
      all_plot <- readRDS("./rda_data/all_morphic_go_scatter_plot.rda")
      all_plot <- ggplotly(all_plot) %>%
        layout(showlegend = FALSE)
      # Replace with code to customize the NWU viability plot
    } else {
      ucsf_plot <- readRDS("./rda_data/go_scatter_plots.rda")
      ucsf_plot <- ggplotly(ucsf_plot[[4]]) %>%
        layout(showlegend = FALSE)
      }
  })

  output$go_jax_plot <- renderPlotly({
    center <- input$center_go
    go_plots <- readRDS("./rda_data/go_scatter_plots.rda")
    jax_plot <- go_plots[[1]]
    # GET SIM MATRIX
    jax_plot$plot_env$simMatrix

    # GET REDUCED TERMS
    jax_plot$plot_env$reducedTerms

    # Create a scatter plot object
    go_scatterplot_obj <- scatterPlot(jax_plot$plot_env$simMatrix, jax_plot$plot_env$reducedTerms, addLabel = TRUE)

    go_scatterplot_obj <- go_scatterplot_obj + geom_text(aes(label=jax_plot$plot_env$reducedTerms$parentTerm))
    if (center == "all centers") {
      ggplotly(go_scatterplot_obj, autosize = TRUE, width = 3000, height = 1000, size = 3) %>%
        layout(showlegend = FALSE)
    }
  })

  output$go_msk_plot <- renderPlotly({
    center <- input$center_go
    go_plots <- readRDS("./rda_data/go_scatter_plots.rda")
    msk_plot <- go_plots[[2]]
    # GET SIM MATRIX
    msk_plot$plot_env$simMatrix

    # GET REDUCED TERMS
    msk_plot$plot_env$reducedTerms

    # Create a scatter plot object
    go_scatterplot_obj <- scatterPlot(msk_plot$plot_env$simMatrix, msk_plot$plot_env$reducedTerms, addLabel = TRUE)

    go_scatterplot_obj <- go_scatterplot_obj + geom_text(aes(label=msk_plot$plot_env$reducedTerms$parentTerm))
    if (center == "all centers") {
      ggplotly(go_scatterplot_obj, autosize = TRUE, width = 3000, height = 1000, size = 3) %>%
        layout(showlegend = FALSE)
    }
  })

  output$go_nwu_plot <- renderPlotly({
    center <- input$center_go
    go_plots <- readRDS("./rda_data/go_scatter_plots.rda")
    nwu_plot <- go_plots[[3]]
    # GET SIM MATRIX
    nwu_plot$plot_env$simMatrix

    # GET REDUCED TERMS
    nwu_plot$plot_env$reducedTerms

    # Create a scatter plot object
    go_scatterplot_obj <- scatterPlot(nwu_plot$plot_env$simMatrix, nwu_plot$plot_env$reducedTerms, addLabel = TRUE)

    go_scatterplot_obj <- go_scatterplot_obj + geom_text(aes(label=nwu_plot$plot_env$reducedTerms$parentTerm))
    if (center == "all centers") {
      ggplotly(go_scatterplot_obj, autosize = TRUE, width = 3000, height = 1000, size = 3) %>%
        layout(showlegend = FALSE)
    }
  })

  output$go_ucsf_plot <- renderPlotly({
    center <- input$center_go
    go_plots <- readRDS("./rda_data/go_scatter_plots.rda")
    ucsf_plot <- go_plots[[4]]
    # GET SIM MATRIX
    ucsf_plot$plot_env$simMatrix

    # GET REDUCED TERMS
    ucsf_plot$plot_env$reducedTerms

    # Create a scatter plot object
    go_scatterplot_obj <- scatterPlot(ucsf_plot$plot_env$simMatrix, ucsf_plot$plot_env$reducedTerms, addLabel = TRUE)

    go_scatterplot_obj <- go_scatterplot_obj + geom_text(aes(label=ucsf_plot$plot_env$reducedTerms$parentTerm))
    if (center == "all centers") {
      ggplotly(go_scatterplot_obj, autosize = TRUE, width = 3000, height = 1000, size = 3) %>%
        layout(showlegend = FALSE)
    }
  })



  output$reactome_emapplot <- renderPlot({
    center <- input$center_reactome
    if (center == "jax") {
      jax_plot <- readRDS("./rda_data/reactome_emmaplots.rda")
      jax_plot[[1]]  # Return the plot object
    } else if (center == "nwu") {
      nwu_plot <- readRDS("./rda_data/reactome_emmaplots.rda")
      nwu_plot[[2]]  # Return the plot object
    } else if (center == "msk") {
      msk_plot <- readRDS("./rda_data/reactome_emmaplots.rda")
      msk_plot[[3]]  # Return the plot object
    } else if (center == "all morphic") {
      all_plot <- readRDS("./rda_data/all_morphic_reactome_emmaplot.rda")
      # Replace with code to customize the NWU viability plot
      all_plot  # Return the plot object
    } else {
      ucsf_plot <- readRDS("./rda_data/reactome_emmaplots.rda")
      ucsf_plot[[4]]  # Return the plot object
    }
  })

  output$jax_reactome_emapplot <- renderPlot({
    center <- input$center_reactome
    if (center =="all centers") {
      reactome_plots <- readRDS("./rda_data/reactome_emmaplots.rda")
      reactome_plots[[1]]
    }
  })

  output$msk_reactome_emapplot <- renderPlot({
    center <- input$center_reactome
    if (center =="all centers") {
      reactome_plots <- readRDS("./rda_data/reactome_emmaplots.rda")
      reactome_plots[[2]]
    }
  })

  output$nwu_reactome_emapplot <- renderPlot({
    center <- input$center_reactome
    if (center =="all centers") {
      reactome_plots <- readRDS("./rda_data/reactome_emmaplots.rda")
      reactome_plots[[3]]
    }
  })

  output$ucsf_reactome_emapplot <- renderPlot({
    center <- input$center_reactome
    if (center =="all centers") {
      reactome_plots <- readRDS("./rda_data/reactome_emmaplots.rda")
      reactome_plots[[4]]
    }
  })


  #GENE CONSTRAINT METRICS - CELL LINE -----------------------------------------------------------------------------------

  # Depmap ---------
  output$cell_essentiality_boxplot_depmap <- renderPlotly({

    # Load cell_essentiality RDS dataframe
    cell_essentiality <- readRDS("./rda_data/cell_essentiality.rda")
    cell_essentiality_jax <- readRDS("./rda_data/cell_essentiality_jax.rda")
    cell_essentiality_msk <- readRDS("./rda_data/cell_essentiality_msk.rda")
    cell_essentiality_nwu <- readRDS("./rda_data/cell_essentiality_nwu.rda")
    cell_essentiality_ucsf <- readRDS("./rda_data/cell_essentiality_ucsf.rda")
    cell_essentiality_all_morphic <- readRDS("./rda_data/cell_essentiality_all_morphic.rda")

    # Load hgnc ID input
    user_input_gene_symbol <- input$search_gene_symbol_cell

    center <- input$center_cell_essentiality_boxplots

    # Threshold line params
    hline <- function(y = 0, color = "grey") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list( dash = "dash",color = color)
      )
    }

    sig_threshod_depmap_boxplot <- input$cell_essential_depmap_significance_threshold
    if (sig_threshod_depmap_boxplot == "-0.5") {
      jax_plot_text <- cell_essentiality_jax$depmap_essential_05_label
      msk_plot_text <- cell_essentiality_msk$depmap_essential_05_label
      nwu_plot_text <- cell_essentiality_nwu$depmap_essential_05_label
      ucsf_plot_text <- cell_essentiality_ucsf$depmap_essential_05_label
      all_plot_text <- cell_essentiality_all_morphic$depmap_essential_05_label
      plot_text <- cell_essentiality_all_morphic$depmap_essential_05_label

      threshold_value <- -0.5
    } else if (sig_threshod_depmap_boxplot == "-1") {
      jax_plot_text <- cell_essentiality_jax$depmap_essential_1_label
      msk_plot_text <- cell_essentiality_msk$depmap_essential_1_label
      nwu_plot_text <- cell_essentiality_nwu$depmap_essential_1_label
      ucsf_plot_text <- cell_essentiality_ucsf$depmap_essential_1_label
      all_plot_text <- cell_essentiality_all_morphic$depmap_essential_1_label
      plot_text <- cell_essentiality_all_morphic$depmap_essential_1_label

      threshold_value <- -1
    }


    # MODIFY PLOT TEXT TO REFLECT THRESHOLD SET i.e. label will be like: essential (<-1)
    if (center == "jax") {
      cell_essentiality_jax <- readRDS("./rda_data/cell_essentiality_jax.rda")

      cell_essentiality_depmap_boxplot <- cell_essentiality_jax %>%
        plot_ly(name = center, y = ~mean_depmap_gene_effect_score, x = center, type = "violin", box = list(visible = T), box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol: ", cell_essentiality_jax$gene_symbol, "<br> Mean gene effect score: ", cell_essentiality_jax$mean_depmap_gene_effect_score,
                                                      "\n", jax_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_jax

    } else if (center == "nwu") {
      cell_essentiality_nwu <- readRDS("./rda_data/cell_essentiality_nwu.rda")

      cell_essentiality_depmap_boxplot <- cell_essentiality_nwu %>%
        plot_ly(name = center, y = ~mean_depmap_gene_effect_score, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol: ", cell_essentiality_nwu$gene_symbol, "<br> Mean gene effect score: ", cell_essentiality_nwu$mean_depmap_gene_effect_score,
                                                      "\n", nwu_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_nwu

    } else if (center == "msk") {
      cell_essentiality_msk <- readRDS("./rda_data/cell_essentiality_msk.rda")



      cell_essentiality_depmap_boxplot <- cell_essentiality_msk %>%
        plot_ly(name = center, y = ~mean_depmap_gene_effect_score, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol: ", cell_essentiality_msk$gene_symbol, "<br> Mean gene effect score: ", cell_essentiality_msk$mean_depmap_gene_effect_score,
                                                      "\n", msk_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_msk

    } else if (center == "all morphic") {
      cell_essentiality_all_morphic <- readRDS("./rda_data/cell_essentiality_all_morphic.rda")


      cell_essentiality_depmap_boxplot <- cell_essentiality_all_morphic %>%
        plot_ly(name = center, y = ~mean_depmap_gene_effect_score, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol: ", cell_essentiality_all_morphic$gene_symbol, "<br> Mean gene effect score: ", cell_essentiality_all_morphic$mean_depmap_gene_effect_score,
                                                      "\n", all_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_all_morphic

    } else if (center == "all centers") {
      ### PLOT_TEXT NEEDS MODIFYING
      cell_essentiality_depmap_boxplot <- cell_essentiality_jax %>%
        plot_ly(name = "JAX", y = ~mean_depmap_gene_effect_score, x = "JAX", type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol: ", cell_essentiality_jax$gene_symbol, "<br> Mean gene effect score: ", cell_essentiality_jax$mean_depmap_gene_effect_score,
                                                      "\n", jax_plot_text)) %>%
        add_trace(name = 'MSK',
                  data = cell_essentiality_msk,
                  y = ~mean_depmap_gene_effect_score,
                  x = "MSK", hoverinfo = "text",
                  hovertext = paste("Gene Symbol: ", cell_essentiality_msk$gene_symbol , "\nMean gene effect score: ", cell_essentiality_msk$mean_depmap_gene_effect_score, "\n", msk_plot_text)) %>%
        add_trace(name = 'NWU',
                  data = cell_essentiality_nwu,
                  y = ~mean_depmap_gene_effect_score,
                  x = "NWU", hoverinfo = "text",
                  hovertext = paste("Gene Symbol: ", cell_essentiality_nwu$gene_symbol , "\nMean gene effect score: ", cell_essentiality_nwu$mean_depmap_gene_effect_score, "\n", nwu_plot_text)) %>%
        add_trace(name = 'UCSF',
                  data = cell_essentiality_ucsf,
                  y = ~mean_depmap_gene_effect_score,
                  x = "UCSF", hoverinfo = "text",
                  hovertext = paste("Gene Symbol: ", cell_essentiality_ucsf$gene_symbol , "\nMean gene effect score: ", cell_essentiality_ucsf$mean_depmap_gene_effect_score, "\n", ucsf_plot_text))


      cell_essentiality_centre_selected <- cell_essentiality_depmap_boxplot

    } else {
      cell_essentiality_ucsf <- readRDS("./rda_data/cell_essentiality_ucsf.rda")

      cell_essentiality_depmap_boxplot <- cell_essentiality_ucsf %>%
        plot_ly(name = center, y = ~mean_depmap_gene_effect_score, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol: ", cell_essentiality_ucsf$gene_symbol,
                                                      "<br> Mean gene effect score: ", cell_essentiality_ucsf$mean_depmap_gene_effect_score,
                                                      "\n", ucsf_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))
      cell_essentiality_centre_selected <- cell_essentiality_ucsf

    }
    # ORDERING AND LEGEND NEEDS TWEAKING
    if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
      if (center == "all centers") {
        cell_essentiality_depmap_boxplot <- cell_essentiality_depmap_boxplot %>%
          add_trace(name = user_input_gene_symbol,
                    y = cell_essentiality_all_morphic$mean_depmap_gene_effect_score[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol], x = 'JAX', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene Symbol: ", user_input_gene_symbol,
                                      "\nMean gene effect score: ", cell_essentiality_all_morphic$mean_depmap_gene_effect_score[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = cell_essentiality_all_morphic$mean_depmap_gene_effect_score[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol], x = 'NWU', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene Symbol: ", user_input_gene_symbol,
                                      "\nMean gene effect score: ", cell_essentiality_all_morphic$mean_depmap_gene_effect_score[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = cell_essentiality_all_morphic$mean_depmap_gene_effect_score[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol], x = 'MSK', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene Symbol: ", user_input_gene_symbol,
                                      "\nMean gene effect score: ", cell_essentiality_all_morphic$mean_depmap_gene_effect_score[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = cell_essentiality_all_morphic$mean_depmap_gene_effect_score[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol], x = 'UCSF', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene Symbol: ", user_input_gene_symbol,
                                      "\nMean gene effect score: ", cell_essentiality_all_morphic$mean_depmap_gene_effect_score[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol]))
          cell_essentiality_depmap_boxplot


      } else {
        cell_essentiality_depmap_boxplot <- cell_essentiality_depmap_boxplot %>%
          add_trace(name = user_input_gene_symbol,
                    y = cell_essentiality_centre_selected$mean_depmap_gene_effect_score[cell_essentiality_centre_selected$gene_symbol == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene Symbol: ", user_input_gene_symbol,
                                      "\nMean gene effect score: ", cell_essentiality_centre_selected$mean_depmap_gene_effect_score[cell_essentiality_centre_selected$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[cell_essentiality_centre_selected$gene_symbol == user_input_gene_symbol]))
        cell_essentiality_depmap_boxplot
      }


    }

    # Add protein coding genes trace if Checkbox is Checked
    if (input$compare_protein_coding_genes) {
      # Code to generate all protein coding genes when the checkbox is checked
      cell_essentiality_depmap_boxplot <- cell_essentiality_depmap_boxplot %>%
        add_trace(name = 'all protein coding genes',
                  data = cell_essentiality,
                  y = ~mean_depmap_gene_effect_score,
                  x = "protein coding genes", hoverinfo = "text",
                  hovertext = paste("Gene Symbol: ", cell_essentiality$gene_symbol , "\nMean gene effect score: ", cell_essentiality$mean_depmap_gene_effect_score, "\n", plot_text))

      cell_essentiality_depmap_boxplot
    } else {
      # Return NULL or an empty plot when the checkbox is not checked
      cell_essentiality_depmap_boxplot
    }



  })

  # ############### --------------- new MEF
  # output$cell_essentiality_boxplot_mef <- renderPlotly({
  #
  #   # Load cell_essentiality RDS dataframe
  #   cell_essentiality <- readRDS("./rda_data/cell_essentiality.rda")
  #
  #   # Load hgnc ID input
  #   user_input_gene_symbol <- input$search_gene_symbol_cell
  #
  #   center <- input$center_cell_essentiality_boxplots
  #
  #   hline <- function(y = 0, color = "grey") {
  #     list(
  #       type = "line",
  #       x0 = 0,
  #       x1 = 1,
  #       xref = "paper",
  #       y0 = y,
  #       y1 = y,
  #       line = list( dash = "dash",color = color)
  #     )
  #   }
  #
  #   threshold_value <- 0.1
  #   if (center == "jax") {
  #     cell_essentiality_jax <- readRDS("./rda_data/cell_essentiality_jax.rda")
  #     cell_essentiality_mef_boxplot <- cell_essentiality_jax %>%
  #       plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "violin", box = list(visible = T),
  #               hoverinfo = "text", hovertext = paste("HGNC ID: ", cell_essentiality_jax$hgnc_id, "<br> Bayes factor: ", cell_essentiality_jax$h1_mef_BF,
  #                                                     "\n", cell_essentiality_jax$h1_mef_essential_label)) %>%
  #       layout(shapes = list(hline(threshold_value)))
  #
  #   } else if (center == "nwu") {
  #     cell_essentiality_nwu <- readRDS("./rda_data/cell_essentiality_nwu.rda")
  #     cell_essentiality_mef_boxplot <- cell_essentiality_nwu %>%
  #       plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "violin", box = list(visible = T),
  #               hoverinfo = "text", hovertext = paste("HGNC ID: ", cell_essentiality_nwu$hgnc_id, "<br> Bayes factor: ", cell_essentiality_nwu$h1_mef_BF,
  #                                                     "\n", cell_essentiality_nwu$h1_mef_essential_label)) %>%
  #       layout(shapes = list(hline(threshold_value)))
  #
  #   } else if (center == "msk") {
  #     cell_essentiality_msk <- readRDS("./rda_data/cell_essentiality_msk.rda")
  #     cell_essentiality_mef_boxplot <- cell_essentiality_msk %>%
  #       plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "violin", box = list(visible = T),
  #               hoverinfo = "text", hovertext = paste("HGNC ID: ", cell_essentiality_msk$hgnc_id, "<br> Bayes factor: ", cell_essentiality_msk$h1_mef_BF,
  #                                                     "\n", cell_essentiality_msk$h1_mef_essential_label)) %>%
  #       layout(shapes = list(hline(threshold_value)))
  #
  #   } else if (center == "all morphic") {
  #     cell_essentiality_all_morphic <- readRDS("./rda_data/cell_essentiality_all_morphic.rda")
  #     cell_essentiality_mef_boxplot <- cell_essentiality_all_morphic %>%
  #       plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "violin", box = list(visible = T),
  #               hoverinfo = "text", hovertext = paste("HGNC ID: ", cell_essentiality_all_morphic$hgnc_id, "<br> Bayes factor: ", cell_essentiality_all_morphic$h1_mef_BF,
  #                                                     "\n", cell_essentiality_all_morphic$h1_mef_essential_label)) %>%
  #       layout(shapes = list(hline(threshold_value)))
  #
  #   } else {
  #     cell_essentiality_ucsf <- readRDS("./rda_data/cell_essentiality_ucsf.rda")
  #     cell_essentiality_mef_boxplot <- cell_essentiality_ucsf %>%
  #       plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "violin", box = list(visible = T),
  #               hoverinfo = "text", hovertext = paste("HGNC ID: ", cell_essentiality_ucsf$hgnc_id,
  #                                                     "<br> Bayes factor: ", cell_essentiality_ucsf$h1_mef_BF,
  #                                                     "\n", cell_essentiality_ucsf$h1_mef_essential_label)) %>%
  #       layout(shapes = list(hline(threshold_value)))
  #
  #   }
  #
  #   ###############
  #
  #   if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
  #
  #     cell_essentiality_mef_boxplot <- cell_essentiality_mef_boxplot %>%
  #       add_trace(name = user_input_gene_symbol,
  #                 y = cell_essentiality$h1_mef_BF[cell_essentiality$hgnc_id == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
  #                 hovertext = paste("HGNC ID: ", user_input_gene_symbol,
  #                                   "\nBayes factor: ", cell_essentiality$h1_mef_BF[cell_essentiality$hgnc_id == user_input_gene_symbol],
  #                                   "\n", cell_essentiality$h1_mef_essential_label[cell_essentiality$hgnc_id == user_input_gene_symbol]))
  #     cell_essentiality_mef_boxplot
  #   }
  #
  #   # Add protein coding genes trace if Checkbox is Checked
  #   if (input$compare_protein_coding_genes) {
  #     # Code to generate graph2 when the checkbox is checked
  #     # ...
  #     # Your code here to generate graph2
  #     cell_essentiality_mef_boxplot <- cell_essentiality_mef_boxplot %>%
  #       add_trace(name = 'all protein coding genes',
  #                 data = cell_essentiality,
  #                 y = ~h1_mef_BF,
  #                 x = "protein coding genes", hoverinfo = "text",
  #                 hovertext = paste("HGNC ID: ", cell_essentiality$hgnc_id , "\nBayes factor: ", cell_essentiality$h1_mef_BF, "\n", cell_essentiality$h1_mef_essential_label))
  #
  #     cell_essentiality_mef_boxplot
  #   } else {
  #     # Return NULL or an empty plot when the checkbox is not checked
  #     cell_essentiality_mef_boxplot
  #   }
  #
  #
  #
  # })
  #
  # ############### --------------- new LAMININ
  # output$cell_essentiality_boxplot_laminin <- renderPlotly({
  #
  #   # Load cell_essentiality RDS dataframe
  #   cell_essentiality <- readRDS("./rda_data/cell_essentiality.rda")
  #
  #   # Load hgnc ID input
  #   user_input_gene_symbol <- input$search_gene_symbol_cell
  #
  #   center <- input$center_cell_essentiality_boxplots
  #
  #   hline <- function(y = 0, color = "grey") {
  #     list(
  #       type = "line",
  #       x0 = 0,
  #       x1 = 1,
  #       xref = "paper",
  #       y0 = y,
  #       y1 = y,
  #       line = list( dash = "dash",color = color)
  #     )
  #   }
  #
  #   threshold_value <- 0.1
  #   if (center == "jax") {
  #     cell_essentiality_jax <- readRDS("./rda_data/cell_essentiality_jax.rda")
  #     cell_essentiality_laminin_boxplot <- cell_essentiality_jax %>%
  #       plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "violin", box = list(visible = T),
  #               hoverinfo = "text", hovertext = paste("HGNC ID: ", cell_essentiality_jax$hgnc_id, "<br> Bayes factor: ", cell_essentiality_jax$h1_laminin_BF,
  #                                                     "\n", cell_essentiality_jax$h1_laminin_essential_label)) %>%
  #       layout(shapes = list(hline(threshold_value)))
  #
  #   } else if (center == "nwu") {
  #     cell_essentiality_nwu <- readRDS("./rda_data/cell_essentiality_nwu.rda")
  #     cell_essentiality_laminin_boxplot <- cell_essentiality_nwu %>%
  #       plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "violin", box = list(visible = T),
  #               hoverinfo = "text", hovertext = paste("HGNC ID: ", cell_essentiality_nwu$hgnc_id, "<br> Bayes factor: ", cell_essentiality_nwu$h1_laminin_BF,
  #                                                     "\n", cell_essentiality_nwu$h1_laminin_essential_label)) %>%
  #       layout(shapes = list(hline(threshold_value)))
  #
  #   } else if (center == "msk") {
  #     cell_essentiality_msk <- readRDS("./rda_data/cell_essentiality_msk.rda")
  #     cell_essentiality_laminin_boxplot <- cell_essentiality_msk %>%
  #       plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "violin", box = list(visible = T),
  #               hoverinfo = "text", hovertext = paste("HGNC ID: ", cell_essentiality_msk$hgnc_id, "<br> Bayes factor: ", cell_essentiality_msk$h1_laminin_BF,
  #                                                     "\n", cell_essentiality_msk$h1_laminin_essential_label)) %>%
  #       layout(shapes = list(hline(threshold_value)))
  #
  #   } else if (center == "all morphic") {
  #     cell_essentiality_all_morphic <- readRDS("./rda_data/cell_essentiality_all_morphic.rda")
  #     cell_essentiality_laminin_boxplot <- cell_essentiality_all_morphic %>%
  #       plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "violin", box = list(visible = T),
  #               hoverinfo = "text", hovertext = paste("HGNC ID: ", cell_essentiality_all_morphic$hgnc_id, "<br> Bayes factor: ", cell_essentiality_all_morphic$h1_laminin_BF,
  #                                                     "\n", cell_essentiality_all_morphic$h1_laminin_essential_label)) %>%
  #       layout(shapes = list(hline(threshold_value)))
  #
  #   } else {
  #     cell_essentiality_ucsf <- readRDS("./rda_data/cell_essentiality_ucsf.rda")
  #     cell_essentiality_laminin_boxplot <- cell_essentiality_ucsf %>%
  #       plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "violin", box = list(visible = T),
  #               hoverinfo = "text", hovertext = paste("HGNC ID: ", cell_essentiality_ucsf$hgnc_id,
  #                                                     "<br> Bayes factor: ", cell_essentiality_ucsf$h1_laminin_BF,
  #                                                     "\n", cell_essentiality_ucsf$h1_laminin_essential_label)) %>%
  #       layout(shapes = list(hline(threshold_value)))
  #
  #   }
  #
  #   ###############
  #
  #   if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
  #
  #     cell_essentiality_laminin_boxplot <- cell_essentiality_laminin_boxplot %>%
  #       add_trace(name = user_input_gene_symbol,
  #                 y = cell_essentiality$h1_laminin_BF[cell_essentiality$hgnc_id == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
  #                 hovertext = paste("HGNC ID: ", user_input_gene_symbol,
  #                                   "\nBayes factor: ", cell_essentiality$h1_laminin_BF[cell_essentiality$hgnc_id == user_input_gene_symbol],
  #                                   "\n", cell_essentiality$h1_laminin_essential_label[cell_essentiality$hgnc_id == user_input_gene_symbol]))
  #     cell_essentiality_laminin_boxplot
  #   }
  #
  #   # Add protein coding genes trace if Checkbox is Checked
  #   if (input$compare_protein_coding_genes) {
  #     # Code to generate graph2 when the checkbox is checked
  #     # ...
  #     # Your code here to generate graph2
  #     cell_essentiality_laminin_boxplot <- cell_essentiality_laminin_boxplot %>%
  #       add_trace(name = 'all protein coding genes',
  #                 data = cell_essentiality,
  #                 y = ~h1_laminin_BF,
  #                 x = "protein coding genes", hoverinfo = "text",
  #                 hovertext = paste("HGNC ID: ", cell_essentiality$hgnc_id ,
  #                                   "\nBayes factor: ", cell_essentiality$h1_laminin_BF,
  #                                   "\n", cell_essentiality$h1_laminin_essential_label))
  #
  #     cell_essentiality_laminin_boxplot
  #   } else {
  #     # Return NULL or an empty plot when the checkbox is not checked
  #     cell_essentiality_laminin_boxplot
  #   }
  # })

  # MEF ---------
  output$cell_essentiality_boxplot_mef <- renderPlotly({
    # Load cell_essentiality RDS dataframe
    cell_essentiality <- readRDS("./rda_data/cell_essentiality.rda")
    cell_essentiality_jax <- readRDS("./rda_data/cell_essentiality_jax.rda")
    cell_essentiality_msk <- readRDS("./rda_data/cell_essentiality_msk.rda")
    cell_essentiality_nwu <- readRDS("./rda_data/cell_essentiality_nwu.rda")
    cell_essentiality_ucsf <- readRDS("./rda_data/cell_essentiality_ucsf.rda")
    cell_essentiality_all_morphic <- readRDS("./rda_data/cell_essentiality_all_morphic.rda")

    # Load hgnc ID input
    user_input_gene_symbol <- input$search_gene_symbol_cell

    center <- input$center_cell_essentiality_boxplots

    # Threshold line params
    hline <- function(y = 0, color = "grey") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list( dash = "dash",color = color)
      )
    }

    
    jax_plot_text <- cell_essentiality_jax$h1_mef_essential_label
    msk_plot_text <- cell_essentiality_msk$h1_mef_essential_label
    nwu_plot_text <- cell_essentiality_nwu$h1_mef_essential_label
    ucsf_plot_text <- cell_essentiality_ucsf$h1_mef_essential_label
    all_plot_text <- cell_essentiality_all_morphic$h1_mef_essential_label
    plot_text <- cell_essentiality_all_morphic$h1_mef_essential_label
    
    # Threshold value is based on:
    # https://www.sciencedirect.com/science/article/pii/S2211124719302128
    threshold_value <- 5
  


    # MODIFY PLOT TEXT TO REFLECT THRESHOLD SET i.e. label will be like: essential (<-1)
    if (center == "jax") {
      cell_essentiality_jax <- readRDS("./rda_data/cell_essentiality_jax.rda")

      cell_essentiality_depmap_boxplot <- cell_essentiality_jax %>%
        plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "violin", box = list(visible = T), box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", cell_essentiality_jax$gene_symbol, "<br> Bayes Factor: ", cell_essentiality_jax$h1_mef_BF,
                                                      "\n", jax_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_jax

    } else if (center == "nwu") {
      cell_essentiality_nwu <- readRDS("./rda_data/cell_essentiality_nwu.rda")

      cell_essentiality_depmap_boxplot <- cell_essentiality_nwu %>%
        plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", cell_essentiality_nwu$gene_symbol, "<br> Bayes Factor: ", cell_essentiality_nwu$h1_mef_BF,
                                                      "\n", nwu_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_nwu

    } else if (center == "msk") {
      cell_essentiality_msk <- readRDS("./rda_data/cell_essentiality_msk.rda")



      cell_essentiality_depmap_boxplot <- cell_essentiality_msk %>%
        plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", cell_essentiality_msk$gene_symbol, "<br> Bayes Factor: ", cell_essentiality_msk$h1_mef_BF,
                                                      "\n", msk_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_msk

    } else if (center == "all morphic") {
      cell_essentiality_all_morphic <- readRDS("./rda_data/cell_essentiality_all_morphic.rda")


      cell_essentiality_depmap_boxplot <- cell_essentiality_all_morphic %>%
        plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", cell_essentiality_all_morphic$gene_symbol, "<br> Bayes Factor: ", cell_essentiality_all_morphic$h1_mef_BF,
                                                      "\n", all_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_all_morphic

    } else if (center == "all centers") {
      ### PLOT_TEXT NEEDS MODIFYING
      cell_essentiality_depmap_boxplot <- cell_essentiality_jax %>%
        plot_ly(name = "JAX", y = ~h1_mef_BF, x = "JAX", type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", cell_essentiality_jax$gene_symbol, "<br> Bayes Factor: ", cell_essentiality_jax$h1_mef_BF,
                                                      "\n", jax_plot_text)) %>%
        add_trace(name = 'MSK',
                  data = cell_essentiality_msk,
                  y = ~h1_mef_BF,
                  x = "MSK", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", cell_essentiality_msk$gene_symbol , "Bayes Factor: ", cell_essentiality_msk$h1_mef_BF, "\n", msk_plot_text)) %>%
        add_trace(name = 'NWU',
                  data = cell_essentiality_nwu,
                  y = ~h1_mef_BF,
                  x = "NWU", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", cell_essentiality_nwu$gene_symbol , "Bayes Factor: ", cell_essentiality_nwu$h1_mef_BF, "\n", nwu_plot_text)) %>%
        add_trace(name = 'UCSF',
                  data = cell_essentiality_ucsf,
                  y = ~h1_mef_BF,
                  x = "UCSF", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", cell_essentiality_ucsf$gene_symbol , "Bayes Factor: ", cell_essentiality_ucsf$h1_mef_BF, "\n", ucsf_plot_text))


      cell_essentiality_centre_selected <- cell_essentiality_depmap_boxplot

    } else {
      cell_essentiality_ucsf <- readRDS("./rda_data/cell_essentiality_ucsf.rda")

      cell_essentiality_depmap_boxplot <- cell_essentiality_ucsf %>%
        plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", cell_essentiality_ucsf$gene_symbol,
                                                      "<br> Bayes Factor: ", cell_essentiality_ucsf$h1_mef_BF,
                                                      "\n", ucsf_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))
      cell_essentiality_centre_selected <- cell_essentiality_ucsf

    }
    # ORDERING AND LEGEND NEEDS TWEAKING
    if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
      if (center == "all centers") {
        cell_essentiality_depmap_boxplot <- cell_essentiality_depmap_boxplot %>%
          add_trace(name = user_input_gene_symbol,
                    y = cell_essentiality_all_morphic$h1_mef_BF[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol], x = 'JAX', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "Bayes Factor: ", cell_essentiality_all_morphic$h1_mef_BF[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = cell_essentiality_all_morphic$h1_mef_BF[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol], x = 'NWU', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "Bayes Factor: ", cell_essentiality_all_morphic$h1_mef_BF[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = cell_essentiality_all_morphic$h1_mef_BF[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol], x = 'MSK', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "Bayes Factor: ", cell_essentiality_all_morphic$h1_mef_BF[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = cell_essentiality_all_morphic$h1_mef_BF[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol], x = 'UCSF', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "Bayes Factor: ", cell_essentiality_all_morphic$h1_mef_BF[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol]))
        cell_essentiality_depmap_boxplot


      } else {
        cell_essentiality_depmap_boxplot <- cell_essentiality_depmap_boxplot %>%
          add_trace(name = user_input_gene_symbol,
                    y = cell_essentiality_centre_selected$h1_mef_BF[cell_essentiality_centre_selected$gene_symbol == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "Bayes Factor: ", cell_essentiality_centre_selected$h1_mef_BF[cell_essentiality_centre_selected$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[cell_essentiality_centre_selected$gene_symbol == user_input_gene_symbol]))
        cell_essentiality_depmap_boxplot
      }


    }

    # Add protein coding genes trace if Checkbox is Checked
    if (input$compare_protein_coding_genes) {
      # Code to generate all protein coding genes when the checkbox is checked
      cell_essentiality_depmap_boxplot <- cell_essentiality_depmap_boxplot %>%
        add_trace(name = 'all protein coding genes',
                  data = cell_essentiality,
                  y = ~h1_mef_BF,
                  x = "protein coding genes", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", cell_essentiality$gene_symbol , "Bayes Factor: ", cell_essentiality$h1_mef_BF, "\n", plot_text))

      cell_essentiality_depmap_boxplot
    } else {
      # Return NULL or an empty plot when the checkbox is not checked
      cell_essentiality_depmap_boxplot
    }



  })

  # Laminin -----------
  output$cell_essentiality_boxplot_laminin <- renderPlotly({
    # Load cell_essentiality RDS dataframe
    cell_essentiality <- readRDS("./rda_data/cell_essentiality.rda")
    cell_essentiality_jax <- readRDS("./rda_data/cell_essentiality_jax.rda")
    cell_essentiality_msk <- readRDS("./rda_data/cell_essentiality_msk.rda")
    cell_essentiality_nwu <- readRDS("./rda_data/cell_essentiality_nwu.rda")
    cell_essentiality_ucsf <- readRDS("./rda_data/cell_essentiality_ucsf.rda")
    cell_essentiality_all_morphic <- readRDS("./rda_data/cell_essentiality_all_morphic.rda")

    # Load hgnc ID input
    user_input_gene_symbol <- input$search_gene_symbol_cell

    center <- input$center_cell_essentiality_boxplots

    # Threshold line params
    hline <- function(y = 0, color = "grey") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list( dash = "dash",color = color)
      )
    }

    jax_plot_text <- cell_essentiality_jax$h1_laminin_essential_label
    msk_plot_text <- cell_essentiality_msk$h1_laminin_essential_label
    nwu_plot_text <- cell_essentiality_nwu$h1_laminin_essential_label
    ucsf_plot_text <- cell_essentiality_ucsf$h1_laminin_essential_label
    all_plot_text <- cell_essentiality_all_morphic$h1_laminin_essential_label
    plot_text <- cell_essentiality_all_morphic$h1_laminin_essential_label
    
    # Threshold value is based on:
    # https://www.sciencedirect.com/science/article/pii/S2211124719302128
    threshold_value <- 5
    


    # MODIFY PLOT TEXT TO REFLECT THRESHOLD SET i.e. label will be like: essential (<-1)
    if (center == "jax") {
      cell_essentiality_jax <- readRDS("./rda_data/cell_essentiality_jax.rda")

      cell_essentiality_depmap_boxplot <- cell_essentiality_jax %>%
        plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "violin", box = list(visible = T), box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", cell_essentiality_jax$gene_symbol, "<br> Bayes Factor: ", cell_essentiality_jax$h1_laminin_BF,
                                                      "\n", jax_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_jax

    } else if (center == "nwu") {
      cell_essentiality_nwu <- readRDS("./rda_data/cell_essentiality_nwu.rda")

      cell_essentiality_depmap_boxplot <- cell_essentiality_nwu %>%
        plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", cell_essentiality_nwu$gene_symbol, "<br> Bayes Factor: ", cell_essentiality_nwu$h1_laminin_BF,
                                                      "\n", nwu_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_nwu

    } else if (center == "msk") {
      cell_essentiality_msk <- readRDS("./rda_data/cell_essentiality_msk.rda")



      cell_essentiality_depmap_boxplot <- cell_essentiality_msk %>%
        plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", cell_essentiality_msk$gene_symbol, "<br> Bayes Factor: ", cell_essentiality_msk$h1_laminin_BF,
                                                      "\n", msk_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_msk

    } else if (center == "all morphic") {
      cell_essentiality_all_morphic <- readRDS("./rda_data/cell_essentiality_all_morphic.rda")


      cell_essentiality_depmap_boxplot <- cell_essentiality_all_morphic %>%
        plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", cell_essentiality_all_morphic$gene_symbol, "<br> Bayes Factor: ", cell_essentiality_all_morphic$h1_laminin_BF,
                                                      "\n", all_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_all_morphic

    } else if (center == "all centers") {
      ### PLOT_TEXT NEEDS MODIFYING
      cell_essentiality_depmap_boxplot <- cell_essentiality_jax %>%
        plot_ly(name = "JAX", y = ~h1_laminin_BF, x = "JAX", type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", cell_essentiality_jax$gene_symbol, "<br> Bayes Factor: ", cell_essentiality_jax$h1_laminin_BF,
                                                      "\n", jax_plot_text)) %>%
        add_trace(name = 'MSK',
                  data = cell_essentiality_msk,
                  y = ~h1_laminin_BF,
                  x = "MSK", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", cell_essentiality_msk$gene_symbol , "Bayes Factor: ", cell_essentiality_msk$h1_laminin_BF, "\n", msk_plot_text)) %>%
        add_trace(name = 'NWU',
                  data = cell_essentiality_nwu,
                  y = ~h1_laminin_BF,
                  x = "NWU", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", cell_essentiality_nwu$gene_symbol , "Bayes Factor: ", cell_essentiality_nwu$h1_laminin_BF, "\n", nwu_plot_text)) %>%
        add_trace(name = 'UCSF',
                  data = cell_essentiality_ucsf,
                  y = ~h1_laminin_BF,
                  x = "UCSF", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", cell_essentiality_ucsf$gene_symbol , "Bayes Factor: ", cell_essentiality_ucsf$h1_laminin_BF, "\n", ucsf_plot_text))


      cell_essentiality_centre_selected <- cell_essentiality_depmap_boxplot

    } else {
      cell_essentiality_ucsf <- readRDS("./rda_data/cell_essentiality_ucsf.rda")

      cell_essentiality_depmap_boxplot <- cell_essentiality_ucsf %>%
        plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", cell_essentiality_ucsf$gene_symbol,
                                                      "<br> Bayes Factor: ", cell_essentiality_ucsf$h1_laminin_BF,
                                                      "\n", ucsf_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))
      cell_essentiality_centre_selected <- cell_essentiality_ucsf

    }
    # ORDERING AND LEGEND NEEDS TWEAKING
    if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
      if (center == "all centers") {
        cell_essentiality_depmap_boxplot <- cell_essentiality_depmap_boxplot %>%
          add_trace(name = user_input_gene_symbol,
                    y = cell_essentiality_all_morphic$h1_laminin_BF[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol], x = 'JAX', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "Bayes Factor: ", cell_essentiality_all_morphic$h1_laminin_BF[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = cell_essentiality_all_morphic$h1_laminin_BF[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol], x = 'NWU', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "Bayes Factor: ", cell_essentiality_all_morphic$h1_laminin_BF[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = cell_essentiality_all_morphic$h1_laminin_BF[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol], x = 'MSK', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "Bayes Factor: ", cell_essentiality_all_morphic$h1_laminin_BF[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = cell_essentiality_all_morphic$h1_laminin_BF[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol], x = 'UCSF', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "Bayes Factor: ", cell_essentiality_all_morphic$h1_laminin_BF[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[cell_essentiality_all_morphic$gene_symbol == user_input_gene_symbol]))
        cell_essentiality_depmap_boxplot


      } else {
        cell_essentiality_depmap_boxplot <- cell_essentiality_depmap_boxplot %>%
          add_trace(name = user_input_gene_symbol,
                    y = cell_essentiality_centre_selected$h1_laminin_BF[cell_essentiality_centre_selected$gene_symbol == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "Bayes Factor: ", cell_essentiality_centre_selected$h1_laminin_BF[cell_essentiality_centre_selected$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[cell_essentiality_centre_selected$gene_symbol == user_input_gene_symbol]))
        cell_essentiality_depmap_boxplot
      }


    }

    # Add protein coding genes trace if Checkbox is Checked
    if (input$compare_protein_coding_genes) {
      # Code to generate all protein coding genes when the checkbox is checked
      cell_essentiality_depmap_boxplot <- cell_essentiality_depmap_boxplot %>%
        add_trace(name = 'all protein coding genes',
                  data = cell_essentiality,
                  y = ~h1_laminin_BF,
                  x = "protein coding genes", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", cell_essentiality$gene_symbol , "Bayes Factor: ", cell_essentiality$h1_laminin_BF, "\n", plot_text))

      cell_essentiality_depmap_boxplot
    } else {
      # Return NULL or an empty plot when the checkbox is not checked
      cell_essentiality_depmap_boxplot
    }


  })

  #GENE CONSTRAINT METRICS - SEQUENCING -----------------------------------------------------------------------------------
  # NAs are inplace of a essential/non essentil label -> which may be computed using the tresholds? 
    # OE LOF -----------------------------------------------------------------------------------
  output$gene_constraint_oe_lof <- renderPlotly({
    # JAX
    jax_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_jax.rda')

    # NWU
    nwu_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_nwu.rda')

    # MSK
    msk_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_msk.rda')

    #UCSF
    ucsf_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_ucsf.rda')

    # All protein coding
    gene_constraint_metrics <- readRDS("./rda_data/gene_constraint_metrics.rda")

    # all morphic
    gene_constraint_metrics_morphic <-  readRDS('./rda_data/gene_constraint_metrics_morphic.rda')

    jax_plot_text <- 'na'
    nwu_plot_text <- 'na'
    msk_plot_text <- 'na'
    ucsf_plot_text <- 'na'
    plot_text <- 'na'
    # Load hgnc ID input
    user_input_gene_symbol <- toupper(input$search_gene_symbol_seq)

    center <- input$center_gene_constraint_seq

    hline <- function(y = 0, color = "grey") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list( dash = "dash",color = color)
      )
    }

    threshold_value <- 0.1
    if (center == "jax") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_jax.rda')
      oe_lof_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~oe_lof, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$oe_lof_upper,
                                                      "\n Lower: ", gene_constraint_seq$oe_lof_lower)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "nwu") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_nwu.rda')
      oe_lof_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~oe_lof, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$oe_lof_upper,
                                                      "\n Lower: ", gene_constraint_seq$oe_lof_lower)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "msk") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_msk.rda')
      oe_lof_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~oe_lof, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$oe_lof_upper,
                                                      "\n Lower: ", gene_constraint_seq$oe_lof_lower)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "all morphic") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_metrics_morphic.rda')
      oe_lof_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~oe_lof, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$oe_lof_upper,
                                                      "\n Lower: ", gene_constraint_seq$oe_lof_lower)) %>%
        layout(shapes = list(hline(threshold_value)))
    } else if (center == "all centers") {
      ### PLOT_TEXT NEEDS MODIFYING
      oe_lof_plot <- jax_gene_constraint_seq %>%
        plot_ly(name = "JAX", y = ~oe_lof, x = "JAX", type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", jax_gene_constraint_seq$gene_symbol, "<br> oe_lof", jax_gene_constraint_seq$oe_lof,
                                                      "\n", jax_plot_text)) %>%
        add_trace(name = 'MSK',
                  data = msk_gene_constraint_seq,
                  y = ~oe_lof,
                  x = "MSK", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", msk_gene_constraint_seq$gene_symbol , "\nMean gene effect score: ", msk_gene_constraint_seq$oe_lof, "\n", msk_plot_text)) %>%
        add_trace(name = 'NWU',
                  data = nwu_gene_constraint_seq,
                  y = ~oe_lof,
                  x = "NWU", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", nwu_gene_constraint_seq$gene_symbol , "\nMean gene effect score: ", nwu_gene_constraint_seq$oe_lof, "\n", nwu_plot_text)) %>%
        add_trace(name = 'UCSF',
                  data = ucsf_gene_constraint_seq,
                  y = ~oe_lof,
                  x = "UCSF", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", ucsf_gene_constraint_seq$gene_symbol , "\nMean gene effect score: ", ucsf_gene_constraint_seq$oe_lof, "\n", ucsf_plot_text))

    } else {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_ucsf.rda')
      oe_lof_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~oe_lof, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$oe_lof_upper,
                                                      "\n Lower: ", gene_constraint_seq$oe_lof_lower)) %>%
        layout(shapes = list(hline(threshold_value)))

    }

    ###############

    # if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
    #
    #   oe_lof_plot <- oe_lof_plot %>%
    #     add_trace(name = user_input_gene_symbol,
    #               y = gene_constraint_metrics$oe_lof[gene_constraint_metrics$gene_symbol == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
    #               hovertext = paste("Gene Symbol:  ", user_input_gene_symbol,
    #                                 "\n Upper: ", gene_constraint_metrics$oe_lof_upper[gene_constraint_metrics$gene_symbol == user_input_gene_symbol],
    #                                 "\n Lower: ", gene_constraint_metrics$oe_lof_lower[gene_constraint_metrics$gene_symbol == user_input_gene_symbol]))
    #   oe_lof_plot
    # }

    if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
      if (center == "all centers") {

        oe_lof_plot <- oe_lof_plot %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$oe_lof[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'JAX', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n oe_lof: ", gene_constraint_metrics_morphic$oe_lof[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$oe_lof[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'NWU', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n oe_lof: ", gene_constraint_metrics_morphic$oe_lof[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$oe_lof[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'MSK', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n oe_lof: ", gene_constraint_metrics_morphic$oe_lof[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$oe_lof[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'UCSF', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n oe_lof: ", gene_constraint_metrics_morphic$oe_lof[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol]))
        oe_lof_plot


      } else {

        oe_lof_plot <- oe_lof_plot %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics$oe_lof[gene_constraint_metrics$gene_symbol == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene Symbol:  ", user_input_gene_symbol,
                                      "\n Upper: ", gene_constraint_metrics$oe_lof_upper[gene_constraint_metrics$gene_symbol == user_input_gene_symbol],
                                      "\n Lower: ", gene_constraint_metrics$oe_lof_lower[gene_constraint_metrics$gene_symbol == user_input_gene_symbol]))
        oe_lof_plot
      }


    }

    # Add protein coding genes trace if Checkbox is Checked
    if (input$compare_protein_coding_genes_gene_constraint_seq) {
      # Code to generate graph2 when the checkbox is checked
      # ...
      # Your code here to generate graph2
      oe_lof_plot <- oe_lof_plot %>%
        add_trace(name = 'all protein coding genes',
                  data = gene_constraint_metrics,
                  y = ~oe_lof,
                  x = "protein coding genes", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", gene_constraint_metrics$gene_symbol ,
                                    "\n Upper: ", gene_constraint_metrics$oe_lof_upper,
                                    "\n Lower: ", gene_constraint_metrics$oe_lof_lower))

      oe_lof_plot
    } else {
      # Return NULL or an empty plot when the checkbox is not checked
      oe_lof_plot
    }
  })

    # OE MIS -----------------------------------------------------------------------------------
  output$gene_constraint_oe_mis <- renderPlotly({
    # JAX
    jax_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_jax.rda')

    # NWU
    nwu_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_nwu.rda')

    # MSK
    msk_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_msk.rda')

    #UCSF
    ucsf_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_ucsf.rda')

    # All protein coding
    gene_constraint_metrics <- readRDS("./rda_data/gene_constraint_metrics.rda")

    # all morphic
    gene_constraint_metrics_morphic <-  readRDS('./rda_data/gene_constraint_metrics_morphic.rda')

    jax_plot_text <- 'na'
    nwu_plot_text <- 'na'
    msk_plot_text <- 'na'
    ucsf_plot_text <- 'na'
    plot_text <- 'na'
    # Load hgnc ID input
    user_input_gene_symbol <- toupper(input$search_gene_symbol_seq)

    center <- input$center_gene_constraint_seq

    hline <- function(y = 0, color = "grey") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list( dash = "dash",color = color)
      )
    }

    threshold_value <- 0.1
    if (center == "jax") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_jax.rda')
      oe_mis_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~oe_mis, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$oe_mis_upper,
                                                      "\n Lower: ", gene_constraint_seq$oe_mis_lower)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "nwu") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_nwu.rda')
      oe_mis_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~oe_mis, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$oe_mis_upper,
                                                      "\n Lower: ", gene_constraint_seq$oe_mis_lower)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "msk") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_msk.rda')
      oe_mis_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~oe_mis, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$oe_mis_upper,
                                                      "\n Lower: ", gene_constraint_seq$oe_mis_lower)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "all morphic") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_metrics_morphic.rda')
      oe_mis_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~oe_mis, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$oe_mis_upper,
                                                      "\n Lower: ", gene_constraint_seq$oe_mis_lower)) %>%
        layout(shapes = list(hline(threshold_value)))
    } else if (center == "all centers") {
      ### PLOT_TEXT NEEDS MODIFYING
      oe_mis_plot <- jax_gene_constraint_seq %>%
        plot_ly(name = "JAX", y = ~oe_mis, x = "JAX", type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", jax_gene_constraint_seq$gene_symbol, "<br> oe_mis", jax_gene_constraint_seq$oe_mis,
                                                      "\n", jax_plot_text)) %>%
        add_trace(name = 'MSK',
                  data = msk_gene_constraint_seq,
                  y = ~oe_mis,
                  x = "MSK", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", msk_gene_constraint_seq$gene_symbol , "\nMean gene effect score: ", msk_gene_constraint_seq$oe_mis, "\n", msk_plot_text)) %>%
        add_trace(name = 'NWU',
                  data = nwu_gene_constraint_seq,
                  y = ~oe_mis,
                  x = "NWU", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", nwu_gene_constraint_seq$gene_symbol , "\nMean gene effect score: ", nwu_gene_constraint_seq$oe_mis, "\n", nwu_plot_text)) %>%
        add_trace(name = 'UCSF',
                  data = ucsf_gene_constraint_seq,
                  y = ~oe_mis,
                  x = "UCSF", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", ucsf_gene_constraint_seq$gene_symbol , "\nMean gene effect score: ", ucsf_gene_constraint_seq$oe_mis, "\n", ucsf_plot_text))

    } else {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_ucsf.rda')
      oe_mis_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~oe_mis, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$oe_mis_upper,
                                                      "\n Lower: ", gene_constraint_seq$oe_mis_lower)) %>%
        layout(shapes = list(hline(threshold_value)))

    }

    ###############

    # if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
    #
    #   oe_mis_plot <- oe_mis_plot %>%
    #     add_trace(name = user_input_gene_symbol,
    #               y = gene_constraint_metrics$oe_mis[gene_constraint_metrics$gene_symbol == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
    #               hovertext = paste("Gene Symbol:  ", user_input_gene_symbol,
    #                                 "\n Upper: ", gene_constraint_metrics$oe_mis_upper[gene_constraint_metrics$gene_symbol == user_input_gene_symbol],
    #                                 "\n Lower: ", gene_constraint_metrics$oe_mis_lower[gene_constraint_metrics$gene_symbol == user_input_gene_symbol]))
    #   oe_mis_plot
    # }

    if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
      if (center == "all centers") {

        oe_mis_plot <- oe_mis_plot %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$oe_mis[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'JAX', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n oe_mis: ", gene_constraint_metrics_morphic$oe_mis[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$oe_mis[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'NWU', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n oe_mis: ", gene_constraint_metrics_morphic$oe_mis[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$oe_mis[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'MSK', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n oe_mis: ", gene_constraint_metrics_morphic$oe_mis[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$oe_mis[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'UCSF', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n oe_mis: ", gene_constraint_metrics_morphic$oe_mis[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol]))
        oe_mis_plot


      } else {

        oe_mis_plot <- oe_mis_plot %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics$oe_mis[gene_constraint_metrics$gene_symbol == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene Symbol:  ", user_input_gene_symbol,
                                      "\n Upper: ", gene_constraint_metrics$oe_mis_upper[gene_constraint_metrics$gene_symbol == user_input_gene_symbol],
                                      "\n Lower: ", gene_constraint_metrics$oe_mis_lower[gene_constraint_metrics$gene_symbol == user_input_gene_symbol]))
        oe_mis_plot
      }


    }

    # Add protein coding genes trace if Checkbox is Checked
    if (input$compare_protein_coding_genes_gene_constraint_seq) {
      # Code to generate graph2 when the checkbox is checked
      # ...
      # Your code here to generate graph2
      oe_mis_plot <- oe_mis_plot %>%
        add_trace(name = 'all protein coding genes',
                  data = gene_constraint_metrics,
                  y = ~oe_mis,
                  x = "protein coding genes", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", gene_constraint_metrics$gene_symbol ,
                                    "\n Upper: ", gene_constraint_metrics$oe_mis_upper,
                                    "\n Lower: ", gene_constraint_metrics$oe_mis_lower))

      oe_mis_plot
    } else {
      # Return NULL or an empty plot when the checkbox is not checked
      oe_mis_plot
    }
  })

    # SHET RGC-ME -----------------------------------------------------------------------------------
  output$gene_constraint_shet_rgcme <- renderPlotly({
    # JAX
    jax_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_jax.rda')

    # NWU
    nwu_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_nwu.rda')

    # MSK
    msk_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_msk.rda')

    #UCSF
    ucsf_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_ucsf.rda')

    # All protein coding
    gene_constraint_metrics <- readRDS("./rda_data/gene_constraint_metrics.rda")

    # all morphic
    gene_constraint_metrics_morphic <-  readRDS('./rda_data/gene_constraint_metrics_morphic.rda')

    jax_plot_text <- 'na'
    nwu_plot_text <- 'na'
    msk_plot_text <- 'na'
    ucsf_plot_text <- 'na'
    plot_text <- 'na'
    # Load hgnc ID input
    user_input_gene_symbol <- toupper(input$search_gene_symbol_seq)

    center <- input$center_gene_constraint_seq

    hline <- function(y = 0, color = "grey") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list( dash = "dash",color = color)
      )
    }

    threshold_value <- 0.1
    if (center == "jax") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_jax.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~shet_rgcme_mean, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$shet_rgcme_upper,
                                                      "\n Lower: ", gene_constraint_seq$shet_rgcme_lower)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "nwu") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_nwu.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~shet_rgcme_mean, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$shet_rgcme_upper,
                                                      "\n Lower: ", gene_constraint_seq$shet_rgcme_lower)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "msk") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_msk.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~shet_rgcme_mean, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$shet_rgcme_upper,
                                                      "\n Lower: ", gene_constraint_seq$shet_rgcme_lower)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "all morphic") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_metrics_morphic.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~shet_rgcme_mean, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$shet_rgcme_upper,
                                                      "\n Lower: ", gene_constraint_seq$shet_rgcme_lower)) %>%
        layout(shapes = list(hline(threshold_value)))
    } else if (center == "all centers") {
      ### PLOT_TEXT NEEDS MODIFYING
      shet_rgcme_plot <- jax_gene_constraint_seq %>%
        plot_ly(name = "JAX", y = ~shet_rgcme_mean, x = "JAX", type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", jax_gene_constraint_seq$gene_symbol, "<br> shet_rgcme_mean", jax_gene_constraint_seq$shet_rgcme_mean,
                                                      "\n", jax_plot_text)) %>%
        add_trace(name = 'MSK',
                  data = msk_gene_constraint_seq,
                  y = ~shet_rgcme_mean,
                  x = "MSK", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", msk_gene_constraint_seq$gene_symbol , "\nMean gene effect score: ", msk_gene_constraint_seq$shet_rgcme_mean, "\n", msk_plot_text)) %>%
        add_trace(name = 'NWU',
                  data = nwu_gene_constraint_seq,
                  y = ~shet_rgcme_mean,
                  x = "NWU", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", nwu_gene_constraint_seq$gene_symbol , "\nMean gene effect score: ", nwu_gene_constraint_seq$shet_rgcme_mean, "\n", nwu_plot_text)) %>%
        add_trace(name = 'UCSF',
                  data = ucsf_gene_constraint_seq,
                  y = ~shet_rgcme_mean,
                  x = "UCSF", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", ucsf_gene_constraint_seq$gene_symbol , "\nMean gene effect score: ", ucsf_gene_constraint_seq$shet_rgcme_mean, "\n", ucsf_plot_text))

    } else {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_ucsf.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~shet_rgcme_mean, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$shet_rgcme_upper,
                                                      "\n Lower: ", gene_constraint_seq$shet_rgcme_lower)) %>%
        layout(shapes = list(hline(threshold_value)))

    }

    ###############

    # if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
    #
    #   shet_rgcme_plot <- shet_rgcme_plot %>%
    #     add_trace(name = user_input_gene_symbol,
    #               y = gene_constraint_metrics$shet_rgcme_mean[gene_constraint_metrics$gene_symbol == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
    #               hovertext = paste("Gene Symbol:  ", user_input_gene_symbol,
    #                                 "\n Upper: ", gene_constraint_metrics$shet_rgcme_upper[gene_constraint_metrics$gene_symbol == user_input_gene_symbol],
    #                                 "\n Lower: ", gene_constraint_metrics$shet_rgcme_lower[gene_constraint_metrics$gene_symbol == user_input_gene_symbol]))
    #   shet_rgcme_plot
    # }

    if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
      if (center == "all centers") {

        shet_rgcme_plot <- shet_rgcme_plot %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$shet_rgcme_mean[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'JAX', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n shet_rgcme_mean: ", gene_constraint_metrics_morphic$shet_rgcme_mean[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$shet_rgcme_mean[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'NWU', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n shet_rgcme_mean: ", gene_constraint_metrics_morphic$shet_rgcme_mean[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$shet_rgcme_mean[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'MSK', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n shet_rgcme_mean: ", gene_constraint_metrics_morphic$shet_rgcme_mean[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$shet_rgcme_mean[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'UCSF', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n shet_rgcme_mean: ", gene_constraint_metrics_morphic$shet_rgcme_mean[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol]))
        shet_rgcme_plot


      } else {

        shet_rgcme_plot <- shet_rgcme_plot %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics$shet_rgcme_mean[gene_constraint_metrics$gene_symbol == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene Symbol:  ", user_input_gene_symbol,
                                      "\n Upper: ", gene_constraint_metrics$shet_rgcme_upper[gene_constraint_metrics$gene_symbol == user_input_gene_symbol],
                                      "\n Lower: ", gene_constraint_metrics$shet_rgcme_lower[gene_constraint_metrics$gene_symbol == user_input_gene_symbol]))
        shet_rgcme_plot
      }


    }

    # Add protein coding genes trace if Checkbox is Checked
    if (input$compare_protein_coding_genes_gene_constraint_seq) {
      # Code to generate graph2 when the checkbox is checked
      # ...
      # Your code here to generate graph2
      shet_rgcme_plot <- shet_rgcme_plot %>%
        add_trace(name = 'all protein coding genes',
                  data = gene_constraint_metrics,
                  y = ~shet_rgcme_mean,
                  x = "protein coding genes", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", gene_constraint_metrics$gene_symbol ,
                                    "\n Upper: ", gene_constraint_metrics$shet_rgcme_upper,
                                    "\n Lower: ", gene_constraint_metrics$shet_rgcme_lower))

      shet_rgcme_plot
    } else {
      # Return NULL or an empty plot when the checkbox is not checked
      shet_rgcme_plot
    }
  })

    # SHET POSTERIOR -----------------------------------------------------------------------------------
  output$gene_constraint_shet_posterior <- renderPlotly({
    # JAX
    jax_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_jax.rda')

    # NWU
    nwu_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_nwu.rda')

    # MSK
    msk_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_msk.rda')

    #UCSF
    ucsf_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_ucsf.rda')

    # All protein coding
    gene_constraint_metrics <- readRDS("./rda_data/gene_constraint_metrics.rda")

    # all morphic
    gene_constraint_metrics_morphic <-  readRDS('./rda_data/gene_constraint_metrics_morphic.rda')

    jax_plot_text <- 'na'
    nwu_plot_text <- 'na'
    msk_plot_text <- 'na'
    ucsf_plot_text <- 'na'
    plot_text <- 'na'
    # Load hgnc ID input
    user_input_gene_symbol <- toupper(input$search_gene_symbol_seq)

    center <- input$center_gene_constraint_seq

    hline <- function(y = 0, color = "grey") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list( dash = "dash",color = color)
      )
    }

    threshold_value <- 0.1
    if (center == "jax") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_jax.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~shet_post_mean, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$shet_post_upper_95,
                                                      "\n Lower: ", gene_constraint_seq$shet_post_lower_95)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "nwu") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_nwu.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~shet_post_mean, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$shet_post_upper_95,
                                                      "\n Lower: ", gene_constraint_seq$shet_post_lower_95)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "msk") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_msk.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~shet_post_mean, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$shet_post_upper_95,
                                                      "\n Lower: ", gene_constraint_seq$shet_post_lower_95)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "all morphic") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_metrics_morphic.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~shet_post_mean, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$shet_post_upper_95,
                                                      "\n Lower: ", gene_constraint_seq$shet_post_lower_95)) %>%
        layout(shapes = list(hline(threshold_value)))
    } else if (center == "all centers") {
      ### PLOT_TEXT NEEDS MODIFYING
      shet_rgcme_plot <- jax_gene_constraint_seq %>%
        plot_ly(name = "JAX", y = ~shet_post_mean, x = "JAX", type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", jax_gene_constraint_seq$gene_symbol, "<br> shet_post_mean", jax_gene_constraint_seq$shet_post_mean,
                                                      "\n", jax_plot_text)) %>%
        add_trace(name = 'MSK',
                  data = msk_gene_constraint_seq,
                  y = ~shet_post_mean,
                  x = "MSK", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", msk_gene_constraint_seq$gene_symbol , "\nMean gene effect score: ", msk_gene_constraint_seq$shet_post_mean, "\n", msk_plot_text)) %>%
        add_trace(name = 'NWU',
                  data = nwu_gene_constraint_seq,
                  y = ~shet_post_mean,
                  x = "NWU", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", nwu_gene_constraint_seq$gene_symbol , "\nMean gene effect score: ", nwu_gene_constraint_seq$shet_post_mean, "\n", nwu_plot_text)) %>%
        add_trace(name = 'UCSF',
                  data = ucsf_gene_constraint_seq,
                  y = ~shet_post_mean,
                  x = "UCSF", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", ucsf_gene_constraint_seq$gene_symbol , "\nMean gene effect score: ", ucsf_gene_constraint_seq$shet_post_mean, "\n", ucsf_plot_text))

    } else {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_ucsf.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~shet_post_mean, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Upper: ", gene_constraint_seq$shet_post_upper_95,
                                                      "\n Lower: ", gene_constraint_seq$shet_post_lower_95)) %>%
        layout(shapes = list(hline(threshold_value)))

    }

    ###############

    # if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
    #
    #   shet_rgcme_plot <- shet_rgcme_plot %>%
    #     add_trace(name = user_input_gene_symbol,
    #               y = gene_constraint_metrics$shet_post_mean[gene_constraint_metrics$gene_symbol == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
    #               hovertext = paste("Gene Symbol:  ", user_input_gene_symbol,
    #                                 "\n Upper: ", gene_constraint_metrics$shet_post_upper_95[gene_constraint_metrics$gene_symbol == user_input_gene_symbol],
    #                                 "\n Lower: ", gene_constraint_metrics$shet_post_lower_95[gene_constraint_metrics$gene_symbol == user_input_gene_symbol]))
    #   shet_rgcme_plot
    # }

    if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
      if (center == "all centers") {

        shet_rgcme_plot <- shet_rgcme_plot %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$shet_post_mean[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'JAX', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n shet_post_mean: ", gene_constraint_metrics_morphic$shet_post_mean[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$shet_post_mean[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'NWU', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n shet_post_mean: ", gene_constraint_metrics_morphic$shet_post_mean[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$shet_post_mean[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'MSK', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n shet_post_mean: ", gene_constraint_metrics_morphic$shet_post_mean[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$shet_post_mean[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'UCSF', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n shet_post_mean: ", gene_constraint_metrics_morphic$shet_post_mean[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol]))
        shet_rgcme_plot


      } else {

        shet_rgcme_plot <- shet_rgcme_plot %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics$shet_post_mean[gene_constraint_metrics$gene_symbol == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene Symbol:  ", user_input_gene_symbol,
                                      "\n Upper: ", gene_constraint_metrics$shet_post_upper_95[gene_constraint_metrics$gene_symbol == user_input_gene_symbol],
                                      "\n Lower: ", gene_constraint_metrics$shet_post_lower_95[gene_constraint_metrics$gene_symbol == user_input_gene_symbol]))
        shet_rgcme_plot
      }


    }

    # Add protein coding genes trace if Checkbox is Checked
    if (input$compare_protein_coding_genes_gene_constraint_seq) {
      # Code to generate graph2 when the checkbox is checked
      # ...
      # Your code here to generate graph2
      shet_rgcme_plot <- shet_rgcme_plot %>%
        add_trace(name = 'all protein coding genes',
                  data = gene_constraint_metrics,
                  y = ~shet_post_mean,
                  x = "protein coding genes", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", gene_constraint_metrics$gene_symbol ,
                                    "\n Upper: ", gene_constraint_metrics$shet_post_upper_95,
                                    "\n Lower: ", gene_constraint_metrics$shet_post_lower_95))

      shet_rgcme_plot
    } else {
      # Return NULL or an empty plot when the checkbox is not checked
      shet_rgcme_plot
    }
  })

    # SCONES -----------------------------------------------------------------------------------
  output$gene_constraint_scones <- renderPlotly({
    # JAX
    jax_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_jax.rda')

    # NWU
    nwu_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_nwu.rda')

    # MSK
    msk_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_msk.rda')

    #UCSF
    ucsf_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_ucsf.rda')

    # All protein coding
    gene_constraint_metrics <- readRDS("./rda_data/gene_constraint_metrics.rda")

    # all morphic
    gene_constraint_metrics_morphic <-  readRDS('./rda_data/gene_constraint_metrics_morphic.rda')

    jax_plot_text <- 'na'
    nwu_plot_text <- 'na'
    msk_plot_text <- 'na'
    ucsf_plot_text <- 'na'
    plot_text <- 'na'
    # Load hgnc ID input
    user_input_gene_symbol <- toupper(input$search_gene_symbol_seq)

    center <- input$center_gene_constraint_seq

    hline <- function(y = 0, color = "grey") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list( dash = "dash",color = color)
      )
    }

    threshold_value <- 0.1
    if (center == "jax") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_jax.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~scones_score, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Scones score: ", jax_gene_constraint_seq$scones_score))

    } else if (center == "nwu") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_nwu.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~scones_score, x = center, type = "violin", box = list(visible = T), 
                hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Scones score: ", nwu_gene_constraint_seq$scones_score))

    } else if (center == "msk") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_msk.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~scones_score, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol,  "<br> Scones score: ", msk_gene_constraint_seq$scones_score))

    } else if (center == "all morphic") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_metrics_morphic.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~scones_score, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol,  "<br> Scones score: ", gene_constraint_metrics_morphic$scones_score))
      
    } else if (center == "all centers") {
      ### PLOT_TEXT NEEDS MODIFYING
      shet_rgcme_plot <- jax_gene_constraint_seq %>%
        plot_ly(name = "JAX", y = ~scones_score, x = "JAX", type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", jax_gene_constraint_seq$gene_symbol,  "<br> Scones score: ", jax_gene_constraint_seq$scones_score)) %>%
        add_trace(name = 'MSK',
                  data = msk_gene_constraint_seq,
                  y = ~scones_score,
                  x = "MSK", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", msk_gene_constraint_seq$gene_symbol ,  "<br> Scones score: ", msk_gene_constraint_seq$scones_score)) %>%
        add_trace(name = 'NWU',
                  data = nwu_gene_constraint_seq,
                  y = ~scones_score,
                  x = "NWU", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", nwu_gene_constraint_seq$gene_symbol ,  "<br> Scones score: ", nwu_gene_constraint_seq$scones_score)) %>%
        add_trace(name = 'UCSF',
                  data = ucsf_gene_constraint_seq,
                  y = ~scones_score,
                  x = "UCSF", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", ucsf_gene_constraint_seq$gene_symbol ,  "<br> Scones score: ", ucsf_gene_constraint_seq$scones_score))

    } else {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_ucsf.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~scones_score, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol,"<br> Scones score: ", ucsf_gene_constraint_seq$scones_score))

    }

    ###############

    # if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
    #
    #   shet_rgcme_plot <- shet_rgcme_plot %>%
    #     add_trace(name = user_input_gene_symbol,
    #               y = gene_constraint_metrics$scones_score[gene_constraint_metrics$gene_symbol == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
    #               hovertext = paste("Gene Symbol:  ", user_input_gene_symbol,
    #                                 "\n Upper: ", gene_constraint_metrics$shet_post_upper_95[gene_constraint_metrics$gene_symbol == user_input_gene_symbol],
    #                                 "\n Lower: ", gene_constraint_metrics$shet_post_lower_95[gene_constraint_metrics$gene_symbol == user_input_gene_symbol]))
    #   shet_rgcme_plot
    # }

    if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
      if (center == "all centers") {

        shet_rgcme_plot <- shet_rgcme_plot %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$scones_score[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'JAX', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n scones_score: ", gene_constraint_metrics_morphic$scones_score[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$scones_score[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'NWU', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n scones_score: ", gene_constraint_metrics_morphic$scones_score[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$scones_score[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'MSK', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n scones_score: ", gene_constraint_metrics_morphic$scones_score[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$scones_score[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'UCSF', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n scones_score: ", gene_constraint_metrics_morphic$scones_score[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol]))
        shet_rgcme_plot


      } else {

        shet_rgcme_plot <- shet_rgcme_plot %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics$scones_score[gene_constraint_metrics$gene_symbol == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene Symbol:  ", user_input_gene_symbol,
                                      "<br> Scones score: ", jax_gene_constraint_seq$scones_score))
        shet_rgcme_plot
      }


    }

    # Add protein coding genes trace if Checkbox is Checked
    if (input$compare_protein_coding_genes_gene_constraint_seq) {
      # Code to generate graph2 when the checkbox is checked
      # ...
      # Your code here to generate graph2
      shet_rgcme_plot <- shet_rgcme_plot %>%
        add_trace(name = 'all protein coding genes',
                  data = gene_constraint_metrics,
                  y = ~scones_score,
                  x = "protein coding genes", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", gene_constraint_metrics$gene_symbol ,
                                    "<br> Scones score: ", gene_constraint_metrics$scones_score))

      shet_rgcme_plot
    } else {
      # Return NULL or an empty plot when the checkbox is not checked
      shet_rgcme_plot
    }
  })

    # DOMINO -----------------------------------------------------------------------------------
  output$gene_constraint_domino <- renderPlotly({
    # JAX
    jax_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_jax.rda')

    # NWU
    nwu_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_nwu.rda')

    # MSK
    msk_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_msk.rda')

    #UCSF
    ucsf_gene_constraint_seq <- readRDS('./rda_data/gene_constraint_ucsf.rda')

    # All protein coding
    gene_constraint_metrics <- readRDS("./rda_data/gene_constraint_metrics.rda")

    # all morphic
    gene_constraint_metrics_morphic <-  readRDS('./rda_data/gene_constraint_metrics_morphic.rda')

    jax_plot_text <- 'na'
    nwu_plot_text <- 'na'
    msk_plot_text <- 'na'
    ucsf_plot_text <- 'na'
    plot_text <- 'na'
    # Load hgnc ID input
    user_input_gene_symbol <- toupper(input$search_gene_symbol_seq)

    center <- input$center_gene_constraint_seq

    hline <- function(y = 0, color = "grey") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list( dash = "dash",color = color)
      )
    }

    threshold_value <- 0.1
    if (center == "jax") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_jax.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~domino_score, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Domino score", jax_gene_constraint_seq$domino_score))

    } else if (center == "nwu") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_nwu.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~domino_score, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Domino score", nwu_gene_constraint_seq$domino_score))
    } else if (center == "msk") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_msk.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~domino_score, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Domino score", msk_gene_constraint_seq$domino_score))

    } else if (center == "all morphic") {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_metrics_morphic.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~domino_score, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Domino score", gene_constraint_metrics_morphic$domino_score))
      
    } else if (center == "all centers") {
      ### PLOT_TEXT NEEDS MODIFYING
      shet_rgcme_plot <- jax_gene_constraint_seq %>%
        plot_ly(name = "JAX", y = ~domino_score, x = "JAX", type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene symbol: ", jax_gene_constraint_seq$gene_symbol, "<br> Domino score", jax_gene_constraint_seq$domino_score)) %>%
        add_trace(name = 'MSK',
                  data = msk_gene_constraint_seq,
                  y = ~domino_score,
                  x = "MSK", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", msk_gene_constraint_seq$gene_symbol , "<br> Domino score", msk_gene_constraint_seq$domino_score)) %>%
        add_trace(name = 'NWU',
                  data = nwu_gene_constraint_seq,
                  y = ~domino_score,
                  x = "NWU", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", nwu_gene_constraint_seq$gene_symbol , "<br> Domino score", nwu_gene_constraint_seq$domino_score)) %>%
        add_trace(name = 'UCSF',
                  data = ucsf_gene_constraint_seq,
                  y = ~domino_score,
                  x = "UCSF", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", ucsf_gene_constraint_seq$gene_symbol , "<br> Domino score", ucsf_gene_constraint_seq$domino_score))

    } else {
      gene_constraint_seq <- readRDS('./rda_data/gene_constraint_ucsf.rda')
      shet_rgcme_plot <- gene_constraint_seq %>%
        plot_ly(name = center, y = ~domino_score, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("Gene Symbol:  ", gene_constraint_seq$gene_symbol, "<br> Domino score", ucsf_gene_constraint_seq$domino_score))

    }

    ###############

    # if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
    #
    #   shet_rgcme_plot <- shet_rgcme_plot %>%
    #     add_trace(name = user_input_gene_symbol,
    #               y = gene_constraint_metrics$domino_score[gene_constraint_metrics$gene_symbol == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
    #               hovertext = paste("Gene Symbol:  ", user_input_gene_symbol,
    #                                 "\n Upper: ", gene_constraint_metrics$shet_post_upper_95[gene_constraint_metrics$gene_symbol == user_input_gene_symbol],
    #                                 "\n Lower: ", gene_constraint_metrics$shet_post_lower_95[gene_constraint_metrics$gene_symbol == user_input_gene_symbol]))
    #   shet_rgcme_plot
    # }

    if (!is.null(user_input_gene_symbol) && user_input_gene_symbol != "") {
      if (center == "all centers") {

        shet_rgcme_plot <- shet_rgcme_plot %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$domino_score[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'JAX', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n domino_score: ", gene_constraint_metrics_morphic$domino_score[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$domino_score[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'NWU', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n domino_score: ", gene_constraint_metrics_morphic$domino_score[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$domino_score[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'MSK', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n domino_score: ", gene_constraint_metrics_morphic$domino_score[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol])) %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics_morphic$domino_score[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol], x = 'UCSF', type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene symbol: ", user_input_gene_symbol,
                                      "\n domino_score: ", gene_constraint_metrics_morphic$domino_score[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol],
                                      "\n", plot_text[gene_constraint_metrics_morphic$gene_symbol == user_input_gene_symbol]))
        shet_rgcme_plot


      } else {

        shet_rgcme_plot <- shet_rgcme_plot %>%
          add_trace(name = user_input_gene_symbol,
                    y = gene_constraint_metrics$domino_score[gene_constraint_metrics$gene_symbol == user_input_gene_symbol], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
                    hovertext = paste("Gene Symbol:  ", user_input_gene_symbol,
                                      "<br> Domino score", ucsf_gene_constraint_seq$domino_score))
        shet_rgcme_plot
      }


    }

    # Add protein coding genes trace if Checkbox is Checked
    if (input$compare_protein_coding_genes_gene_constraint_seq) {
      # Code to generate graph2 when the checkbox is checked
      # ...
      # Your code here to generate graph2
      shet_rgcme_plot <- shet_rgcme_plot %>%
        add_trace(name = 'all protein coding genes',
                  data = gene_constraint_metrics,
                  y = ~domino_score,
                  x = "protein coding genes", hoverinfo = "text",
                  hovertext = paste("Gene symbol: ", gene_constraint_metrics$gene_symbol ,
                                    "<br> Domino score", gene_constraint_metrics$domino_score))

      shet_rgcme_plot
    } else {
      # Return NULL or an empty plot when the checkbox is not checked
      shet_rgcme_plot
    }
  })


  #  # DATA INFORMATION TABLES-----------------------------------------------------------------------------------

  # Render each table using DT::renderDataTable and pass it to the UI
  output$table_gene_identifiers <- DT::renderDataTable({
    datatable(data_info_tables[[1]], rownames = FALSE, class = "display nowrap cell-border", options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_center_info <- DT::renderDataTable({
    datatable(data_info_tables[[2]], rownames = FALSE, class = "display nowrap cell-border", options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_impc <- DT::renderDataTable({
    datatable(data_info_tables[[3]], rownames = FALSE, class = "display nowrap cell-border",  options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_depmap <- DT::renderDataTable({
    datatable(data_info_tables[[4]], rownames = FALSE, class = "display nowrap cell-border",  options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_itv_metrics <- DT::renderDataTable({
    datatable(data_info_tables[[5]], rownames = FALSE, class = "display nowrap cell-border",  options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_ddg2p <- DT::renderDataTable({
    datatable(data_info_tables[[6]], rownames = FALSE, class = "display nowrap cell-border",  options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_omim <- DT::renderDataTable({
    datatable(data_info_tables[[7]], rownames = FALSE, class = "display nowrap cell-border",  options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_go <- DT::renderDataTable({
    datatable(data_info_tables[[8]], rownames = FALSE, class = "display nowrap cell-border",  options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_panther <- DT::renderDataTable({
    datatable(data_info_tables[[9]], rownames = FALSE, class = "display nowrap cell-border",  options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_reactome <- DT::renderDataTable({
    datatable(data_info_tables[[10]], rownames = FALSE, class = "display nowrap cell-border",  options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  #  # MORPHIC INFORMATION TAB-----------------------------------------------------------------------------------

  output$morphic_description <- renderUI({
    HTML('
    <h1>MorPhiC - Molecular Phenotypes of Null Alleles in Cells</h1>
    <p>The MorPhiC programme aims to develop a consistent catalog of molecular and cellular phenotypes for null alleles for every human gene by using in-vitro multicellular systems. The catalog will be made available for broad use by the biomedical community.</p>
    <p>MorPhiC has three components: the Data Production Research and Development Centers (DPCs), the Data Analysis and Validation Centers (DAVs) and the Data Resource and Administrative Coordinating Center (DRACC).</p>
  ')
  })

  output$jax_description <- renderUI({
    HTML('
    <p>Paul Robson, Ph.D. Jackson Laboratory, Farmington, Connecticut. JAX MorPhiC data production center.</p>
  ')
  })

  output$msk_description <- renderUI({
    HTML('
    <p>Danwei Huangfu, Ph.D. Sloan-Kettering Institute for Cancer Research, New York City. Center for scalable knockout and multimodal phenotyping in genetically diverse human genomes.</p>
  ')
  })

  output$nwu_description <- renderUI({
    HTML('
    <p>Mazhar Adli, Ph.D. Northwestern University Feinberg School of Medicine, Chicago. Molecular and cellular characterization of essential human genes.</p>
  ')
  })

  output$ucsf_description <- renderUI({
    HTML('
    <p>Luke Gilbert, Ph.D. University of California, San Francisco. Spatial multiomic mapping of gene function with CRISPRoff.</p>
  ')
  })
  #  # TEXT ABOVE VISUALISATIONS-----------------------------------------------------------------------------------

  output$upset_text <- renderUI({
    HTML("<div style='background-color: white; color: black; padding: 10px; text-align: left; font-size: 14px;'>
         Intersections of Genes selected by respective Data Production Centers
         </div>")
  })

  output$viability_text <- renderUI({
    HTML('<div style="background-color: white; color: black; padding: 10px; text-align: left; font-size: 14px;">
        <p>International Mouse Phenotyping Consortium (IMPC) lethal lines Data:</p>
        <p>Data source: <a href="http://ftp.ebi.ac.uk/pub/databases/impc/all-data-releases/release-19.1/results/" target="_blank">Mouse Viability and Phenotype Data</a></p>
      </div>')
  })

  output$disease_text <- renderUI({
    HTML('<div style="background-color: white; color: black; padding: 10px; text-align: left; font-size: 14px;">
         <p>Disease Data: OMIM (Mendelian disease) Genes Data &amp; Development Disorder Gene to Phenotype Data:</p>
         <p>Data source: <a href="https://www.omim.org/" target="_blank">OMIM Data</a></p>
       </div>')

  })

  output$panther_text <- renderUI({
    HTML('<div style="background-color: white; color: black; padding: 10px; text-align: left; font-size: 14px;">
          <p>PANTHER (Protein ANalysis THrough Evolutionary Relationships) protein-coding genes Data:</p>
          <p>Data source: <a href="https://www.pantherdb.org/" target="_blank">PANTHERdb Data</a></p>
          <ul>
            <li>Family and Protein Class (supergrouping of protein families)</li>
            <li>Subfamily (subgroup within the family phylogenetic tree)</li>
          </ul>
        </div>')

  })

  output$cell_essential_text <- renderUI({
    HTML('<div style="background-color: white; color: black; padding: 10px; text-align: left; font-size: 14px;">
          <p>Cellular Core Essentiality Data:</p>
          <p>Threshold (< -0.5)<p>
          <p>Data source: <a href="https://depmap.org/portal/download/all/" target="_blank">DepMap 23Q2 CRISPR Gene effect Data</a></p>
        </div>')

  })

  output$go_text <- renderUI({
    HTML('<div style="background-color: white; color: black; padding: 10px; text-align: left; font-size: 14px;">
          <p>Gene Ontology-based semantic similarity  Data:</p>
          <p>Plot GO terms as scattered points. Distances between points represent the similarity between terms, and axes are the first 2 components of applying a PCoA to the (di)similarity matrix. Size of the point represents the provided scores or, in its absence, the number of genes the GO term contains.</p>
          <p>Data source: <a href="http://geneontology.org/docs/download-ontology/" target="_blank">Gene Ontology Data</a></p>
        </div>')

  })

  output$reactome_text <- renderUI({
    HTML('<div style="background-color: white; color: black; padding: 10px; text-align: left; font-size: 14px;">
          <p>Reactome Pathway Data:</p>
          <p>Data source: <a href="https://reactome.org/" target="_blank">Reactome Data</a></p>
        </div>')

  })

  output$cell_line_text <- renderUI({
    HTML('<div style="background-color: white; color: black; padding: 10px; text-align: left; font-size: 14px;">
          <p>Cellular Core Essentiality from Cell lines Data:</p>
          <p>Data sources:</p>
          <ul>
            <li><a href="https://depmap.org/portal/download/all/" target="_blank">DepMap 23Q2 CRISPR Gene effect Data</a></li>
            <li><a href="https://www.sciencedirect.com/science/article/pii/S2211124719302128" target="_blank">Mair et al 2019: MEF and Laminin cultured cell lines</a></li>
          </ul>
          <p>For gene effect, a score less than -0.5 represents depletion in most cell lines, while less than -1 represents strong killing.</p>
        </div>')

    # WHAT ABOUT EG THRESHOLDS FOR MEF AND LAMININ? e.g. BF > 5 FDR < 1%

  })

  output$cell_sequencing_text <- renderUI({
    HTML('<div style="background-color: white; color: black; padding: 10px; text-align: left; font-size: 14px;">
          <p>Cellular Core Essentiality from Sequencing Data:</p>
          <p>Data sources:</p>
          <ul>
            <li><a href="https://gnomad.broadinstitute.org/downloads" target="_blank">gnomAD</a></li>
            <li><a href="https://pubmed.ncbi.nlm.nih.gov/37214792/" target="_blank">Sun et al 2023: Shet RGC-ME mean</a></li>
            <li><a href="https://www.biorxiv.org/content/10.1101/2023.05.19.541520v1" target="_blank">Zeng et al 2023: Shet posterior mean</a></li>
          </ul>
          <p>For gnomAD loss-of-function observed/expected upper bound fraction (suggested threshold for highly intolerant genes: oe_lof_upper < 0.35)</p>
          <p>For Shet RGC-ME mean, the suggested threshold for highly intolerant genes shet > 0.075)</p>
          <p>For Shet posterior mean, the suggested threshold for highly intolerant genes shet > 0.1)</p>

        </div>')
  })

  #  # TABLES BELOW VISUALISATIONS-----------------------------------------------------------------------------------

  output$impc_data_table <- renderDT(server=FALSE,{
    datatable(
      app_data_table[,c(1:3, 8:12)],
      plugins = "ellipsis",
      extensions = 'Buttons',
      class = "display nowrap",
      #container = headers,
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        searching = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = "csv", text = "Download Full Table", filename = "Full_data",
               exportOptions = list(
                 modifier = list(page = "all"),
                 orthogonal = "export"
               )
          )
        ),
        columnDefs = list(
          list(
            targets = "_all",
            render = JS("$.fn.dataTable.render.ellipsis(17, false)")
          )
        )
      )
    )
  })

  output$omim_data_table <- renderDT(server=FALSE,{
    datatable(
      app_data_table[,c(1:3, 40:43)],
      plugins = "ellipsis",
      extensions = 'Buttons',
      class = "display nowrap",
      #container = headers,
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        searching = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = "csv", text = "Download Full Table", filename = "Full_data",
               exportOptions = list(
                 modifier = list(page = "all"),
                 orthogonal = "export"
               )
          )
        ),
        columnDefs = list(
          list(
            targets = "_all",
            render = JS("$.fn.dataTable.render.ellipsis(17, false)")
          )
        )
      )
    )
  })

  output$panther_data_table <- renderDT(server=FALSE,{
    datatable(
      app_data_table[,c(1:3, 50:54)],
      plugins = "ellipsis",
      extensions = 'Buttons',
      class = "display nowrap",
      #container = headers,
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        searching = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = "csv", text = "Download Full Table", filename = "Full_data",
               exportOptions = list(
                 modifier = list(page = "all"),
                 orthogonal = "export"
               )
          )
        ),
        columnDefs = list(
          list(
            targets = "_all",
            render = JS("$.fn.dataTable.render.ellipsis(17, false)")
          )
        )
      )
    )
  })

  output$cell_essentiality_data_table <- renderDT(server=FALSE,{
    datatable(
      app_data_table[,c(1:3, 13:35)],
      plugins = "ellipsis",
      extensions = 'Buttons',
      class = "display nowrap",
      #container = headers,
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        searching = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = "csv", text = "Download Full Table", filename = "Full_data",
               exportOptions = list(
                 modifier = list(page = "all"),
                 orthogonal = "export"
               )
          )
        ),
        columnDefs = list(
          list(
            targets = "_all",
            render = JS("$.fn.dataTable.render.ellipsis(17, false)")
          )
        )
      )
    )
  })

  # CHANGE TO: TABLE WITH DATA USED IN VISUALISATION ANALYSIS
  output$go_data_table <- renderDT(server=FALSE,{
    datatable(
      app_data_table[,c(1:3, 44:49)],
      plugins = "ellipsis",
      extensions = 'Buttons',
      class = "display nowrap",
      #container = headers,
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        searching = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = "csv", text = "Download Full Table", filename = "Full_data",
               exportOptions = list(
                 modifier = list(page = "all"),
                 orthogonal = "export"
               )
          )
        ),
        columnDefs = list(
          list(
            targets = "_all",
            render = JS("$.fn.dataTable.render.ellipsis(17, false)")
          )
        )
      )
    )
  })

  # CHANGE TO: TABLE WITH DATA USED IN VISUALISATION ANALYSIS
  output$reactome_data_table <- renderDT(server=FALSE,{
    datatable(
      app_data_table[,c(1:3, 55:56)],
      plugins = "ellipsis",
      extensions = 'Buttons',
      class = "display nowrap",
      #container = headers,
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        searching = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = "csv", text = "Download Full Table", filename = "Full_data",
               exportOptions = list(
                 modifier = list(page = "all"),
                 orthogonal = "export"
               )
          )
        ),
        columnDefs = list(
          list(
            targets = "_all",
            render = JS("$.fn.dataTable.render.ellipsis(17, false)")
          )
        )
      )
    )
  })
  
  output$validate_gene <- renderText({
    user_input_gene_symbol <- input$gene_search
    if (!validGene() && user_input_gene_symbol != "" ) {
      "This gene is not part of the Morphic pipeline (yet)"
    } 
  })

}

## DEFINE THE APP (UI + SERVER)-----------------------------------------------------------------------------------
shinyApp(ui, server)
