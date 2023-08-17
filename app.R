if (!require(shiny)) install.packages(c("shiny", "shinydashboard", "DT", "plotly", "UpSetR", "ggplot2"))
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(UpSetR)
library(ggplot2)
library(rrvgo)
library(shinyjs)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD TABLES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

app_data_table <- readRDS("./rda_data/app_data_table.rda")
#colnames(app_data_table) <- gsub("_", " ", colnames(app_data_table))
data_info_tables <- readRDS("./rda_data/data_info_tables.rda")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# APP CODE...

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UI - HEADER
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
header <- dashboardHeader(
  title = tags$a(href='https://morphic.bio/',
                tags$img(src='morphiclogo.png',style = "margin-left: -20px;"),
                )
  # tags$li(
  #   class = "dropdown",
  #   tags$style(HTML("
  #         .navbar-custom-menu{float:left !important;}
  #         .sidebar-menu{display:flex;align-items:baseline;}
  #         "))
  # ),

  # tags$style(HTML("
  #     .navbar-custom {
  #       background-color: #001F3F; /* Navy blue */
  #     }
  #     .navbar-custom .navbar-nav > li > a {
  #       color: #FFFFFF; /* White text */
  #     }
  #     .navbar-custom .navbar-nav > li > a:hover {
  #       color: #87CEEB; /* Lighter blue on hover */
  #     }
  #     .navbar-custom .navbar-nav > .active > a,
  #     .navbar-custom .navbar-nav > .active > a:focus,
  #     .navbar-custom .navbar-nav > .active > a:hover {
  #       background-color: #002855; /* Darker blue when selected */
  #     }
  #   ")),
  # tags$li(
  #   class = "dropdown",
  #   sidebarMenu(
  #     id = "tablist",
  #     menuItem("MorPhiC Genes", tabName = "genes", icon = icon("table")),
  #     menuItem("Visualisations", tabName = "visualisations", icon = icon("chart-bar")),
  #     menuItem("Data info", tabName = "data_info", icon = icon("info")),
  #     menuItem("About MorPhiC", tabName = "about_morphic", icon = icon("magnifying-glass"))
  #   )
  # )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UI - SIDEBAR
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("MorPhiC Genes", tabName = "genes", icon = icon("table")),
    menuItem("Visualisations", tabName = "visualisations", icon = icon("chart-bar")),
    menuItem("Data info", tabName = "data_info", icon = icon("info")),
    menuItem("About MorPhiC", tabName = "about_morphic", icon = icon("magnifying-glass"))

  )
  # disable = TRUE
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UI - BODY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
body <- dashboardBody(
  fillPage = TRUE,
  # tags$head(tags$style(HTML('
  #                               /* logo */
  #                               .skin-blue .main-header .logo {
  #                               background-color: #f4b943;
  #                               }
  #
  #                               /* logo when hovered */
  #                               .skin-blue .main-header .logo:hover {
  #                               background-color: #f4b943;
  #                               }
  #
  #                               /* navbar (rest of the header) */
  #                               .skin-blue .main-header .navbar {
  #                               background-color: #096BD2;
  #                               }
  #
  #                               /* main sidebar */
  #                               .skin-blue .main-sidebar {
  #                               background-color: #FFFFFF;
  #                               }
  #
  #                               /* active selected tab in the sidebarmenu */
  #                               .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
  #                               background-color: #015BB8;
  #                               }
  #
  #                               /* other links in the sidebarmenu */
  #                               .skin-blue .main-sidebar .sidebar .sidebar-menu a{
  #                               background-color: #86E8F0;
  #                               color: #FFFFFF;
  #                               }
  #
  #                               /* other links in the sidebarmenu when hovered */
  #                               .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
  #                               background-color: #2286EE;
  #                               }
  #                               /* toggle button when hovered  */
  #                               .skin-blue .main-header .navbar .sidebar-toggle:hover{
  #                               background-color: #ff69b4;
  #                               }
  #
  #                               /* body */
  #                               .content-wrapper, .right-side {
  #                               background-color: #FFFFFF;
  #                               }
  #
  #                               '))),
  tabItems(
    tabItem(
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # GENE TABLE
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      tabName = "genes",
      fluidRow(
        # UI part with the new component
        box(
          width = 12,
          title = "Search Gene",
          status = "warning",
          textInput("gene_search", label = NULL, placeholder = "DLX3", width = 300),
          checkboxInput("view_summary", "View gene summary"),
          conditionalPanel(
            condition = "input.view_summary",
            fluidRow(
              column(
                width = 12,
                box(
                  title = textOutput("gene_name_title"),
                  status = "info",
                  # Display invalid gene message if the gene is not valid
                  uiOutput("invalid_gene_message")
                )
              ),
              fluidRow(
                column(width = 12,
                       box(
                         fluidRow(
                           column(width = 6, uiOutput("centers_sumtab")),
                           column(width = 6, uiOutput("omim_sumtab"))
                         ),
                         fluidRow(
                           column(width = 6, uiOutput("omim_lethal_sumtab")),
                           column(width = 6, uiOutput("viability_sumtab"))
                         ),
                         fluidRow(
                           column(width = 6, uiOutput("cell_essential_sumtab")),
                           column(width = 6, uiOutput("gene_constraint_metrics_sumtab"))
                         ),
                         title = NULL,
                         status = "info"
                       )
                )
              )
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
            column(width = 4,
                   checkboxGroupInput("data_sources", label = NULL,
                                      choices = c("Gene Identifiers", "Data Production Centers", "IMPC Mouse Model Data",
                                                  "Gene Constraint Metrics", "Disease Data",
                                                  "Gene Ontology", "PANTHERdb Protein Data", "Reactome Pathway Data"),
                                      selected = c("Gene Identifiers", "Data Production Centers")),
            )
          )
        )),

      fluidRow(
        box(
          width = 12,
          title = "Browse MorPhiC Gene list Data",
          status = "primary",
          DTOutput("genes_table")
        )
      ),
    ),
    tabItem(
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # DATA INFORMATION TAB
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # ABOUT MORPHIC TAB
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # DATA VISUALISATIONS TAB
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
                         "Select center gene list:",
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
                         "Select center gene list:",
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
                         "Select center gene list:",
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
                         "Select center gene list:",
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
                   conditionalPanel(
                     condition = "input.center_go != 'all centers'",
                     column(width = 9, plotlyOutput("go_scatter", height = "60vh"))
                   ),
                   conditionalPanel(
                     condition = "input.center_go == 'all centers'",
                     column( width = 9,
                             fluidRow(width = 12,
                                      splitLayout(cellWidths = c("50%", "50%"),
                                                  plotlyOutput("go_jax_plot"), plotlyOutput("go_msk_plot"))),
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
                         "Select center gene list:",
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
                         "Select center gene list:",
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

        # tabPanel("Cell essentiallity (boxplots)", id = "cell_essentiality_boxplots_tab",
        #          column(
        #            width = 9,
        #            fluidRow(
        #              column(12, plotlyOutput("cell_essentiality_boxplot_depmap", height = "40vh")) # Adjust height as needed
        #            ),
        #            hr(),
        #            fluidRow(
        #              column(12, plotlyOutput("cell_essentiality_boxplot_mef", height = "40vh"))    # Adjust height as needed
        #            ),
        #            hr(),
        #            fluidRow(
        #              column(12, plotlyOutput("cell_essentiality_boxplot_laminin", height = "40vh"))    # Adjust height as needed
        #            ),
        #            hr()
        #
        #          ),
        #          column(
        #            width = 3,
        #            box(
        #              width = 12,
        #              title = "Options",
        #              status = "primary",
        #              selectInput(
        #                "center_cell_essentiality_boxplots",
        #                "Select center gene list:",
        #                choices = c("all morphic", "jax", "msk", "nwu", "ucsf"),
        #                selected = "all morphic"  # Set the default selected option
        #              ),
        #              selectInput(
        #                "cell_essential_depmap_significance_threshold",
        #                "Select significance threshold for Cell essentiality DepMap data:",
        #                choices = c("0.05", "0.01"),
        #                selected = "0.05"  # Set the default selected option
        #              ),
        #              textInput("search_hgnc_id", "Search Gene:", placeholder = "HGNC:3239"),
        #              checkboxInput(inputId = "compare_protein_coding_genes", label = "Compare to all protein coding genes")
        #            )
        #          )
        # ),

        tabPanel(HTML("Cellular Core Essentiallity<br><span style='font-size: 14px;'>(Cell Line Data)</span>"), id = "cell_essentiality_boxplots_tab",
                 fluidRow(
                   column(
                     width = 9,
                     fluidRow(
                       column(12, plotlyOutput("cell_essentiality_boxplot_depmap", height = "40vh")) # Adjust height as needed
                     ),
                     hr(),
                     fluidRow(
                       column(12, plotlyOutput("cell_essentiality_boxplot_mef", height = "40vh"))    # Adjust height as needed
                     ),
                     hr(),
                     fluidRow(
                       column(12, plotlyOutput("cell_essentiality_boxplot_laminin", height = "40vh"))    # Adjust height as needed
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
                         "Select center gene list:",
                         choices = c("all morphic", "jax", "msk", "nwu", "ucsf", "all centers"),
                         selected = "all morphic"  # Set the default selected option
                       ),
                       selectInput(
                         "cell_essential_depmap_significance_threshold",
                         "Select significance threshold for Cell essentiality DepMap data:",
                         choices = c("0.05", "0.01"),
                         selected = "0.05"  # Set the default selected option
                       ),
                       textInput("search_hgnc_id", "Search Gene:", placeholder = "HGNC:3239"),
                       checkboxInput(inputId = "compare_protein_coding_genes", label = "Compare to all protein coding genes")
                     )
                   )
                   )

        ),

        #### NEEDS TO BE: oe lof, oe mis, shet rgc-me, shet post, domino, scones
        tabPanel(HTML("Cellular Core Essentiallity<br><span style='font-size: 14px;'>(Sequencing Data)</span>"), id = "gene_constraint_metrics_tab",
                 fluidRow(
                   column(
                     width = 9,
                     fluidRow(column(12, plotlyOutput("gene_constraint_mean_shet", height = "40vh"))),
                     hr(),
                     fluidRow(column(12, plotlyOutput("gene_constraint_oe_mis", height = "40vh"))),
                     hr(),
                     fluidRow(column(12, plotlyOutput("gene_constraint_domino", height = "40vh")))
                   ),
                   column(
                     width = 3,
                     box(
                       width = 12,
                       title = "Options",
                       status = "primary",
                       selectInput(
                         "center",
                         "Select center gene list:",
                         choices = c("all morphic", "jax", "msk", "nwu", "ucsf"),
                         selected = "all morphic"  # Set the default selected option
                       ),
                       sliderInput("bins", label = h4("Specified Bin Size"),
                                   min = 0.01, max = 0.1, value = 0.05, step = 0.01
                       ),
                       checkboxInput(inputId = "compare_protein_coding_genes_gene_constraint", label = "Compare to all protein coding genes")
                     )
                   )
                 )

        )
      )


    )

  )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DEFINE UI
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ui <- dashboardPage(header, sidebar, body)
ui <- dashboardPage(header, sidebar, body)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DEFINE SERVER
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
server <- function(input, output) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # FILTER TABLE BASED ON SEARCH
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # RENDER TABLE WITH HEADERS
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
        th('Mean gene effect score'), th('Cellular core essential (< -0.5)'), th('Cellular core essential (< -1)'), th('Bayes Factor'), th('False Discovery Rate'),  th('Essential/Non-essential'), th('Bayes Factor'), th('False Discovery Rate'),  th('Essential/Non-essential'), th('Score'), th('Lower score'), th('Upper score'), th('Score'), th('Lower score'), th('Upper score'), th('Mean'), th('Lower'), th('Upper'), th('Mean'), th('Lower 95'), th('Upper 95'), th('Score'), th('Score'),
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
  #Render the DT table
  # observeEvent(input$show_columns, {
  #   # Update the value of x based on the checkbox input
  #   if (input$show_columns) {
  #     hidden_columns <- c(54:55)
  #
  #     output$genes_table <- renderDT({
  #       datatable(
  #         filtered_data(),
  #         plugins = "ellipsis",
  #         class = "display nowrap cell-border",
  #         container = headers,
  #         filter = "top",
  #         rownames = FALSE,
  #         options = list(
  #           pageLength = 10,  # Number of rows displayed per page
  #           lengthMenu = c(10, 25, 50, 100),  # Choose rows per page options
  #           scrollX = TRUE,
  #           searching = TRUE,
  #           columnDefs = list(
  #             list(
  #               targets = hidden_columns,
  #               visible = FALSE
  #             ),
  #             list(
  #               targets = "_all",
  #               render = JS("$.fn.dataTable.render.ellipsis( 17, false )")
  #             )
  #           )
  #         )
  #       )
  #     })
  #   } else {
  #     output$genes_table <- renderDT({
  #       datatable(
  #         filtered_data(),
  #         plugins = "ellipsis",
  #         class = "display nowrap cell-border",
  #         container = headers,
  #         filter = "top",
  #         rownames = FALSE,
  #         options = list(
  #           pageLength = 10,  # Number of rows displayed per page
  #           lengthMenu = c(10, 25, 50, 100),  # Choose rows per page options
  #           scrollX = TRUE,
  #           searching = TRUE,
  #           columnDefs = list(
  #             list(
  #               targets = "_all",
  #               render = JS("$.fn.dataTable.render.ellipsis( 17, false )")
  #             )
  #           )
  #         )
  #       )
  #     })
  #
  #   }
  # })


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # GENE CARDS
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
    if (validGene()) {
      renderValueBox({
        input_type <- detect_input_type(toupper(input$gene_search))
        select_row <- app_data_table[
          app_data_table[[input_type]] == toupper(input$gene_search), ]
        valueBox(
          "IMPC viability",
          select_row$viability_impc,
          icon = icon("plus-minus"),
          color = "blue",
          width = 6
        )
      })
    }
  })

  output$cell_essential_sumtab <- renderUI({
    if (validGene()) {
      renderValueBox({
        input_type <- detect_input_type(toupper(input$gene_search))
        select_row <- app_data_table[
          app_data_table[[input_type]] == toupper(input$gene_search), ]
        valueBox(
          "Cellular essential (DepMap 0.05)",
          select_row$depmap_essential_05_label,
          icon = icon("heart-pulse"),
          color = "blue",
          width = 6
        )
      })
    }
  })



  output$centers_sumtab <- renderUI({
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
          "Centers with gene",
          gene_list_result,
          icon = icon("vials"),
          color = "blue",
          width = 6
        )
      })
    }
  })


  output$omim_sumtab <- renderUI({
    if (validGene()) {
      renderValueBox({
        input_type <- detect_input_type(toupper(input$gene_search))
        select_row <- app_data_table[
          app_data_table[[input_type]] == toupper(input$gene_search), ]
        valueBox(
          "Clinical phenotype",
          select_row$omim_phenotype_molecular_basis_known,
          icon = icon("hand-dots"),
          color = "blue",
          width = 6
        )
      })
    }
  })

  output$omim_lethal_sumtab <- renderUI({
    if (validGene()) {
      renderValueBox({
        input_type <- detect_input_type(toupper(input$gene_search))
        select_row <- app_data_table[
          app_data_table[[input_type]] == toupper(input$gene_search), ]
        valueBox(
          "OMIM gene lethality",
          select_row$omim_gene_lethality,
          icon = icon("skull-crossbones"),
          color = "blue",
          width = 6
        )
      })
    }
  })

  output$gene_constraint_metrics_sumtab <- renderUI({
    if (validGene()) {
      renderValueBox({
        input_type <- detect_input_type(toupper(input$gene_search))
        select_row <- app_data_table[
          app_data_table[[input_type]] == toupper(input$gene_search), ]

        metrics <- list()
        shet_rgcme_mean <- as.numeric(select_row$shet_rgcme_mean)
        oe_lof_upper <- as.numeric(select_row$oe_lof_upper)

        metrics <- append(metrics, paste("RGCME Mean:", signif(shet_rgcme_mean, 3)))
        metrics <- append(metrics, paste("OE LOF Upper:", signif(oe_lof_upper, 3)))


        valueBox(
          "Gene constraint metrics",
          metrics,
          icon = icon("flask"),
          color = "blue",
          width = 6
        )
      })
    }
  })


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # VISUALISATIONS
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
        all_centers_plot <- readRDS("./rda_data/impc_viability_all_centers_plot.rda")

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
      all_centers_plot <- readRDS("./rda_data/impc_viability_all_centers_plot.rda")

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
      all_centers_plot <- readRDS("./rda_data/omim_all_centers_plot.rda")

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
      jax_plot[[1]]  # Return the plot object
    } else if (center == "nwu") {
      nwu_plot <- readRDS("./rda_data/go_scatter_plots.rda")
      nwu_plot[[2]]  # Return the plot object
    } else if (center == "msk") {
      msk_plot <- readRDS("./rda_data/go_scatter_plots.rda")
      msk_plot[[3]]  # Return the plot object
    } else if (center == "all morphic") {
      all_plot <- readRDS("./rda_data/all_morphic_go_scatter_plot.rda")
      # Replace with code to customize the NWU viability plot
      all_plot  # Return the plot object
    } else {
      ucsf_plot <- readRDS("./rda_data/go_scatter_plots.rda")
      ucsf_plot[[4]]  # Return the plot object
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
      ggplotly(go_scatterplot_obj, autosize = TRUE, width = 3000, height = 1000, size = 3)
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
      ggplotly(go_scatterplot_obj, autosize = TRUE, width = 3000, height = 1000, size = 3)
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
      ggplotly(go_scatterplot_obj, autosize = TRUE, width = 3000, height = 1000, size = 3)
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
      ggplotly(go_scatterplot_obj, autosize = TRUE, width = 3000, height = 1000, size = 3)
    }
  })

  # Render the Reactome emapplot plot based on the selected center
  # Not using plotly as emapplot not supported
  # output$reactome_emapplot <- renderPlot({
  #   center <- input$center_reactome
  #   if (center == "jax") {
  #     jax_plot <- readRDS("./rda_data/reactome_emmaplots.rda")
  #     jax_plot[[1]]  # Return the plot object
  #   } else if (center == "nwu") {
  #     nwu_plot <- readRDS("./rda_data/reactome_emmaplots.rda")
  #     nwu_plot[[2]]  # Return the plot object
  #   } else if (center == "msk") {
  #     msk_plot <- readRDS("./rda_data/reactome_emmaplots.rda")
  #     msk_plot[[3]]  # Return the plot object
  #   } else if (center == "all morphic") {
  #     all_plot <- readRDS("./rda_data/all_morphic_reactome_emmaplot.rda")
  #     # Replace with code to customize the NWU viability plot
  #     all_plot  # Return the plot object
  #   } else {
  #     ucsf_plot <- readRDS("./rda_data/reactome_emmaplots.rda")
  #     ucsf_plot[[4]]  # Return the plot object
  #   }
  # })

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


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # CELL ESSENTIALITY BOX PLOTS
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ########-------- DepMap BoxPlot
  output$cell_essentiality_boxplot_depmap <- renderPlotly({

    # Load cell_essentiality RDS dataframe
    cell_essentiality <- readRDS("./rda_data/cell_essentiality.rda")

    # Load hgnc ID input
    user_input_hgnc_id <- input$search_hgnc_id

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

    threshold_value <- 0.1

    sig_threshod_depmap_boxplot <- input$cell_essential_depmap_significance_threshold
    if (sig_threshod_depmap_boxplot == "0.05") {
      jax_plot_text <- cell_essentiality_jax$depmap_essential_05_label
    } else if (sig_threshod_depmap_boxplot == "0.01") {
      jax_plot_text <- cell_essentiality_jax$depmap_essential_1_label
    }

    sig_threshod_depmap_boxplot <- input$cell_essential_depmap_significance_threshold
    if (sig_threshod_depmap_boxplot == "0.05") {
      msk_plot_text <- cell_essentiality_msk$depmap_essential_05_label
    } else if (sig_threshod_depmap_boxplot == "0.01") {
      msk_plot_text <- cell_essentiality_msk$depmap_essential_1_label
    }

    sig_threshod_depmap_boxplot <- input$cell_essential_depmap_significance_threshold
    if (sig_threshod_depmap_boxplot == "0.05") {
      nwu_plot_text <- cell_essentiality_nwu$depmap_essential_05_label
    } else if (sig_threshod_depmap_boxplot == "0.01") {
      nwu_plot_text <- cell_essentiality_nwu$depmap_essential_1_label
    }

    sig_threshod_depmap_boxplot <- input$cell_essential_depmap_significance_threshold
    if (sig_threshod_depmap_boxplot == "0.05") {
      ucsf_plot_text <- cell_essentiality_ucsf$depmap_essential_05_label
    } else if (sig_threshod_depmap_boxplot == "0.01") {
      ucsf_plot_text <- cell_essentiality_ucsf$depmap_essential_1_label
    }

    # MODIFY PLOT TEXT TO REFLECT THRESHOLD SET i.e. label will be like: essential (<0.01)
    if (center == "jax") {
      cell_essentiality_jax <- readRDS("./rda_data/cell_essentiality_jax.rda")

      cell_essentiality_depmap_boxplot <- cell_essentiality_jax %>%
        plot_ly(name = center, y = ~mean_depmap_gene_effect_score, x = center, type = "violin", box = list(visible = T),
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_jax$hgnc_id, "<br> Mean gene effect score :", cell_essentiality_jax$mean_depmap_gene_effect_score,
                                                      "\n", jax_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_jax

    } else if (center == "nwu") {
      cell_essentiality_nwu <- readRDS("./rda_data/cell_essentiality_nwu.rda")

      cell_essentiality_depmap_boxplot <- cell_essentiality_nwu %>%
        plot_ly(name = center, y = ~mean_depmap_gene_effect_score, x = center, type = "violin",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_nwu$hgnc_id, "<br> Mean gene effect score :", cell_essentiality_nwu$mean_depmap_gene_effect_score,
                                                      "\n", nwu_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_nwu

    } else if (center == "msk") {
      cell_essentiality_msk <- readRDS("./rda_data/cell_essentiality_msk.rda")



      cell_essentiality_depmap_boxplot <- cell_essentiality_msk %>%
        plot_ly(name = center, y = ~mean_depmap_gene_effect_score, x = center, type = "violin",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_msk$hgnc_id, "<br> Mean gene effect score :", cell_essentiality_msk$mean_depmap_gene_effect_score,
                                                      "\n", msk_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_msk

    } else if (center == "all morphic") {
      cell_essentiality_all_morphic <- readRDS("./rda_data/cell_essentiality_all_morphic.rda")

      sig_threshod_depmap_boxplot <- input$cell_essential_depmap_significance_threshold
      if (sig_threshod_depmap_boxplot == "0.05") {
        all_plot_text <- cell_essentiality_all_morphic$depmap_essential_05_label
      } else if (sig_threshod_depmap_boxplot == "0.01") {
        all_plot_text <- cell_essentiality_all_morphic$depmap_essential_1_label
      }

      cell_essentiality_depmap_boxplot <- cell_essentiality_all_morphic %>%
        plot_ly(name = center, y = ~mean_depmap_gene_effect_score, x = center, type = "violin",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_all_morphic$hgnc_id, "<br> Mean gene effect score :", cell_essentiality_all_morphic$mean_depmap_gene_effect_score,
                                                      "\n", all_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_all_morphic

    } else if (center == "all centers") {
      ### PLOT_TEXT NEEDS MODIFYING
      cell_essentiality_depmap_boxplot <- cell_essentiality_jax %>%
        plot_ly(name = "JAX", y = ~mean_depmap_gene_effect_score, x = "JAX", type = "violin",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_jax$hgnc_id, "<br> Mean gene effect score :", cell_essentiality_jax$mean_depmap_gene_effect_score,
                                                      "\n", plot_text)) %>%
        add_trace(name = 'MSK',
                  data = cell_essentiality_msk,
                  y = ~mean_depmap_gene_effect_score,
                  x = "MSK", hoverinfo = "text",
                  hovertext = paste("HGNC ID :", cell_essentiality_msk$hgnc_id , "\nMean gene effect score :", cell_essentiality_msk$mean_depmap_gene_effect_score, "\n", plot_text)) %>%
        add_trace(name = 'NWU',
                  data = cell_essentiality_nwu,
                  y = ~mean_depmap_gene_effect_score,
                  x = "NWU", hoverinfo = "text",
                  hovertext = paste("HGNC ID :", cell_essentiality_nwu$hgnc_id , "\nMean gene effect score :", cell_essentiality_nwu$mean_depmap_gene_effect_score, "\n", plot_text)) %>%
        add_trace(name = 'UCSF',
                  data = cell_essentiality_ucsf,
                  y = ~mean_depmap_gene_effect_score,
                  x = "UCSF", hoverinfo = "text",
                  hovertext = paste("HGNC ID :", cell_essentiality_ucsf$hgnc_id , "\nMean gene effect score :", cell_essentiality_ucsf$mean_depmap_gene_effect_score, "\n", plot_text))


      cell_essentiality_centre_selected <- cell_essentiality_all_morphic

    } else {
      cell_essentiality_ucsf <- readRDS("./rda_data/cell_essentiality_ucsf.rda")

      cell_essentiality_depmap_boxplot <- cell_essentiality_ucsf %>%
        plot_ly(name = center, y = ~mean_depmap_gene_effect_score, x = center, type = "violin",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_ucsf$hgnc_id,
                                                      "<br> Mean gene effect score :", cell_essentiality_ucsf$mean_depmap_gene_effect_score,
                                                      "\n", ucsf_plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))
      cell_essentiality_centre_selected <- cell_essentiality_ucsf

    }

    if (!is.null(user_input_hgnc_id) && user_input_hgnc_id != "") {

      cell_essentiality_depmap_boxplot <- cell_essentiality_depmap_boxplot %>%
        add_trace(name = user_input_hgnc_id,
                  y = cell_essentiality_centre_selected$mean_depmap_gene_effect_score[cell_essentiality_centre_selected$hgnc_id == user_input_hgnc_id], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
                  hovertext = paste("HGNC ID :", user_input_hgnc_id,
                                    "\nMean gene effect score :", cell_essentiality_centre_selected$mean_depmap_gene_effect_score[cell_essentiality_centre_selected$hgnc_id == user_input_hgnc_id],
                                    "\n", plot_text[cell_essentiality_centre_selected$hgnc_id == user_input_hgnc_id]))
      cell_essentiality_depmap_boxplot
    }

    # Add protein coding genes trace if Checkbox is Checked
    if (input$compare_protein_coding_genes) {
      # Code to generate all protein coding genes when the checkbox is checked
      cell_essentiality_depmap_boxplot <- cell_essentiality_depmap_boxplot %>%
        add_trace(name = 'all protein coding genes',
                  data = cell_essentiality,
                  y = ~mean_depmap_gene_effect_score,
                  x = "protein coding genes", hoverinfo = "text",
                  hovertext = paste("HGNC ID :", cell_essentiality$hgnc_id , "\nMean gene effect score :", cell_essentiality$mean_depmap_gene_effect_score, "\n", plot_text))

      cell_essentiality_depmap_boxplot
    } else {
      # Return NULL or an empty plot when the checkbox is not checked
      cell_essentiality_depmap_boxplot
    }



  })

  ############### --------------- new MEF
  output$cell_essentiality_boxplot_mef <- renderPlotly({

    # Load cell_essentiality RDS dataframe
    cell_essentiality <- readRDS("./rda_data/cell_essentiality.rda")

    # Load hgnc ID input
    user_input_hgnc_id <- input$search_hgnc_id

    center <- input$center_cell_essentiality_boxplots

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
      cell_essentiality_jax <- readRDS("./rda_data/cell_essentiality_jax.rda")
      cell_essentiality_mef_boxplot <- cell_essentiality_jax %>%
        plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "violin",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_jax$hgnc_id, "<br> Bayes factor :", cell_essentiality_jax$h1_mef_BF,
                                                      "\n", cell_essentiality_jax$h1_mef_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "nwu") {
      cell_essentiality_nwu <- readRDS("./rda_data/cell_essentiality_nwu.rda")
      cell_essentiality_mef_boxplot <- cell_essentiality_nwu %>%
        plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "violin",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_nwu$hgnc_id, "<br> Bayes factor :", cell_essentiality_nwu$h1_mef_BF,
                                                      "\n", cell_essentiality_nwu$h1_mef_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "msk") {
      cell_essentiality_msk <- readRDS("./rda_data/cell_essentiality_msk.rda")
      cell_essentiality_mef_boxplot <- cell_essentiality_msk %>%
        plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "violin",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_msk$hgnc_id, "<br> Bayes factor :", cell_essentiality_msk$h1_mef_BF,
                                                      "\n", cell_essentiality_msk$h1_mef_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "all morphic") {
      cell_essentiality_all_morphic <- readRDS("./rda_data/cell_essentiality_all_morphic.rda")
      cell_essentiality_mef_boxplot <- cell_essentiality_all_morphic %>%
        plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "violin",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_all_morphic$hgnc_id, "<br> Bayes factor :", cell_essentiality_all_morphic$h1_mef_BF,
                                                      "\n", cell_essentiality_all_morphic$h1_mef_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else {
      cell_essentiality_ucsf <- readRDS("./rda_data/cell_essentiality_ucsf.rda")
      cell_essentiality_mef_boxplot <- cell_essentiality_ucsf %>%
        plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "violin",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_ucsf$hgnc_id,
                                                      "<br> Bayes factor :", cell_essentiality_ucsf$h1_mef_BF,
                                                      "\n", cell_essentiality_ucsf$h1_mef_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    }

    ###############

    if (!is.null(user_input_hgnc_id) && user_input_hgnc_id != "") {

      cell_essentiality_mef_boxplot <- cell_essentiality_mef_boxplot %>%
        add_trace(name = user_input_hgnc_id,
                  y = cell_essentiality$h1_mef_BF[cell_essentiality$hgnc_id == user_input_hgnc_id], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
                  hovertext = paste("HGNC ID :", user_input_hgnc_id,
                                    "\nBayes factor :", cell_essentiality$h1_mef_BF[cell_essentiality$hgnc_id == user_input_hgnc_id],
                                    "\n", cell_essentiality$h1_mef_essential_label[cell_essentiality$hgnc_id == user_input_hgnc_id]))
      cell_essentiality_mef_boxplot
    }

    # Add protein coding genes trace if Checkbox is Checked
    if (input$compare_protein_coding_genes) {
      # Code to generate graph2 when the checkbox is checked
      # ...
      # Your code here to generate graph2
      cell_essentiality_mef_boxplot <- cell_essentiality_mef_boxplot %>%
        add_trace(name = 'all protein coding genes',
                  data = cell_essentiality,
                  y = ~h1_mef_BF,
                  x = "protein coding genes", hoverinfo = "text",
                  hovertext = paste("HGNC ID :", cell_essentiality$hgnc_id , "\nBayes factor :", cell_essentiality$h1_mef_BF, "\n", cell_essentiality$h1_mef_essential_label))

      cell_essentiality_mef_boxplot
    } else {
      # Return NULL or an empty plot when the checkbox is not checked
      cell_essentiality_mef_boxplot
    }



  })

  ############### --------------- new LAMININ
  output$cell_essentiality_boxplot_laminin <- renderPlotly({

    # Load cell_essentiality RDS dataframe
    cell_essentiality <- readRDS("./rda_data/cell_essentiality.rda")

    # Load hgnc ID input
    user_input_hgnc_id <- input$search_hgnc_id

    center <- input$center_cell_essentiality_boxplots

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
      cell_essentiality_jax <- readRDS("./rda_data/cell_essentiality_jax.rda")
      cell_essentiality_laminin_boxplot <- cell_essentiality_jax %>%
        plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "violin",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_jax$hgnc_id, "<br> Bayes factor :", cell_essentiality_jax$h1_laminin_BF,
                                                      "\n", cell_essentiality_jax$h1_laminin_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "nwu") {
      cell_essentiality_nwu <- readRDS("./rda_data/cell_essentiality_nwu.rda")
      cell_essentiality_laminin_boxplot <- cell_essentiality_nwu %>%
        plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "violin",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_nwu$hgnc_id, "<br> Bayes factor :", cell_essentiality_nwu$h1_laminin_BF,
                                                      "\n", cell_essentiality_nwu$h1_laminin_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "msk") {
      cell_essentiality_msk <- readRDS("./rda_data/cell_essentiality_msk.rda")
      cell_essentiality_laminin_boxplot <- cell_essentiality_msk %>%
        plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "violin",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_msk$hgnc_id, "<br> Bayes factor :", cell_essentiality_msk$h1_laminin_BF,
                                                      "\n", cell_essentiality_msk$h1_laminin_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "all morphic") {
      cell_essentiality_all_morphic <- readRDS("./rda_data/cell_essentiality_all_morphic.rda")
      cell_essentiality_laminin_boxplot <- cell_essentiality_all_morphic %>%
        plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "violin",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_all_morphic$hgnc_id, "<br> Bayes factor :", cell_essentiality_all_morphic$h1_laminin_BF,
                                                      "\n", cell_essentiality_all_morphic$h1_laminin_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else {
      cell_essentiality_ucsf <- readRDS("./rda_data/cell_essentiality_ucsf.rda")
      cell_essentiality_laminin_boxplot <- cell_essentiality_ucsf %>%
        plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "violin",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_ucsf$hgnc_id,
                                                      "<br> Bayes factor :", cell_essentiality_ucsf$h1_laminin_BF,
                                                      "\n", cell_essentiality_ucsf$h1_laminin_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    }

    ###############

    if (!is.null(user_input_hgnc_id) && user_input_hgnc_id != "") {

      cell_essentiality_laminin_boxplot <- cell_essentiality_laminin_boxplot %>%
        add_trace(name = user_input_hgnc_id,
                  y = cell_essentiality$h1_laminin_BF[cell_essentiality$hgnc_id == user_input_hgnc_id], x = center, type = "scatter", mode = 'markers', hoverinfo = "text",
                  hovertext = paste("HGNC ID :", user_input_hgnc_id,
                                    "\nBayes factor :", cell_essentiality$h1_laminin_BF[cell_essentiality$hgnc_id == user_input_hgnc_id],
                                    "\n", cell_essentiality$h1_laminin_essential_label[cell_essentiality$hgnc_id == user_input_hgnc_id]))
      cell_essentiality_laminin_boxplot
    }

    # Add protein coding genes trace if Checkbox is Checked
    if (input$compare_protein_coding_genes) {
      # Code to generate graph2 when the checkbox is checked
      # ...
      # Your code here to generate graph2
      cell_essentiality_laminin_boxplot <- cell_essentiality_laminin_boxplot %>%
        add_trace(name = 'all protein coding genes',
                  data = cell_essentiality,
                  y = ~h1_laminin_BF,
                  x = "protein coding genes", hoverinfo = "text",
                  hovertext = paste("HGNC ID :", cell_essentiality$hgnc_id ,
                                    "\nBayes factor :", cell_essentiality$h1_laminin_BF,
                                    "\n", cell_essentiality$h1_laminin_essential_label))

      cell_essentiality_laminin_boxplot
    } else {
      # Return NULL or an empty plot when the checkbox is not checked
      cell_essentiality_laminin_boxplot
    }
  })
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # GENE CONSTRAINT METRICS
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ### SHET MEAN
  output$gene_constraint_mean_shet <- renderPlotly({
    gene_constraint_metrics <- readRDS("./rda_data/gene_constraint_metrics.rda")

    center <- input$center
    if (center == "jax") {

      gene_constraint_jax <- readRDS("./rda_data/gene_constraint_jax.rda")


      gene_constraint_mean_shet <- plot_ly(xbins = list(size = input$bins)) %>%

        add_histogram(x = ~gene_constraint_jax$shet_rgcme_mean,
                      hovertemplate = "Number of MorPhiC Genes: %{y} <extra></extra>",
                      name = "Morphic") %>%

        layout(barmode = "stack",
               title = list(text = "SHET: Distribution of Mean scores", y = 0.99, x = 0.5),
               xaxis = list(title = ("Mean Score"),
                            zeroline = FALSE),
               yaxis = list(title = "Number of Genes",
                            zeroline = FALSE))

    } else if (center == "nwu") {

      gene_constraint_nwu <- readRDS("./rda_data/gene_constraint_nwu.rda")


      gene_constraint_mean_shet <- plot_ly(xbins = list(size = input$bins)) %>%

        add_histogram(x = ~gene_constraint_nwu$shet_rgcme_mean,
                      hovertemplate = "Number of MorPhiC Genes: %{y} <extra></extra>",
                      name = "Morphic") %>%

        layout(barmode = "stack",
               title = list(text = "SHET: Distribution of Mean scores", y = 0.99, x = 0.5),
               xaxis = list(title = ("Mean Score"),
                            zeroline = FALSE),
               yaxis = list(title = "Number of Genes",
                            zeroline = FALSE))

    } else if (center == "msk") {

      gene_constraint_msk <- readRDS("./rda_data/gene_constraint_msk.rda")


      gene_constraint_mean_shet <- plot_ly(xbins = list(size = input$bins)) %>%

        add_histogram(x = ~gene_constraint_msk$shet_rgcme_mean,
                      hovertemplate = "Number of MorPhiC Genes: %{y} <extra></extra>",
                      name = "Morphic") %>%

        layout(barmode = "stack",
               title = list(text = "SHET: Distribution of Mean scores", y = 0.99, x = 0.5),
               xaxis = list(title = ("Mean Score"),
                            zeroline = FALSE),
               yaxis = list(title = "Number of Genes",
                            zeroline = FALSE))

    } else if (center == "all morphic") {

      gene_constraint_all_morphic <- readRDS("./rda_data/gene_constraint_metrics_morphic_1006.rda")


      gene_constraint_mean_shet <- plot_ly(xbins = list(size = input$bins)) %>%

        add_histogram(x = ~gene_constraint_all_morphic$shet_rgcme_mean,
                      hovertemplate = "Number of MorPhiC Genes: %{y} <extra></extra>",
                      name = "Morphic") %>%

        layout(barmode = "stack",
               title = list(text = "SHET: Distribution of Mean scores", y = 0.99, x = 0.5),
               xaxis = list(title = ("Mean Score"),
                            zeroline = FALSE),
               yaxis = list(title = "Number of Genes",
                            zeroline = FALSE))

    } else {

      gene_constraint_ucsf <- readRDS("./rda_data/gene_constraint_ucsf.rda")


      gene_constraint_mean_shet <- plot_ly(xbins = list(size = input$bins)) %>%

        add_histogram(x = ~gene_constraint_ucsf$shet_rgcme_mean,
                      hovertemplate = "Number of MorPhiC Genes: %{y} <extra></extra>",
                      name = "Morphic") %>%

        layout(barmode = "stack",
               title = list(text = "SHET: Distribution of Mean scores", y = 0.99, x = 0.5),
               xaxis = list(title = ("Mean Score"),
                            zeroline = FALSE),
               yaxis = list(title = "Number of Genes",
                            zeroline = FALSE))

    }

    if (input$compare_protein_coding_genes_gene_constraint) {
      gene_constraint_mean_shet <- gene_constraint_mean_shet %>%
        add_histogram(x = ~gene_constraint_metrics$shet_rgcme_mean,
                      hovertemplate = "Number of protein coding Genes: %{y} <extra></extra>",
                      name = "Protein coding genes")
      # marker = list(color = "rgb(0, 119, 182)",
      #               line = list(color = "rgb(202, 240, 248)",
      #                           width = 2))) %

      gene_constraint_mean_shet
    } else {
      gene_constraint_mean_shet
    }

  })

  ### OE MIS gene_constraint_oe_mis
  output$gene_constraint_oe_mis <- renderPlotly({
    gene_constraint_metrics <- readRDS("./rda_data/gene_constraint_metrics.rda")

    center <- input$center
    if (center == "jax") {

      gene_constraint_jax <- readRDS("./rda_data/gene_constraint_jax.rda")


      gene_constraint_oe_mis <- plot_ly(xbins = list(size = input$bins)) %>%

        add_histogram(x = ~gene_constraint_jax$oe_mis,
                      hovertemplate = "Number of MorPhiC Genes: %{y} <extra></extra>",
                      name = "Morphic") %>%

        layout(barmode = "stack",
               title = list(text = "O/E MIS: Distribution of Mean scores", y = 0.99, x = 0.5),
               xaxis = list(title = ("Score"),
                            zeroline = FALSE),
               yaxis = list(title = "Number of Genes",
                            zeroline = FALSE))

    } else if (center == "nwu") {

      gene_constraint_nwu <- readRDS("./rda_data/gene_constraint_nwu.rda")


      gene_constraint_oe_mis <- plot_ly(xbins = list(size = input$bins)) %>%

        add_histogram(x = ~gene_constraint_nwu$oe_mis,
                      hovertemplate = "Number of MorPhiC Genes: %{y} <extra></extra>",
                      name = "Morphic") %>%

        layout(barmode = "stack",
               title = list(text = "O/E MIS: Distribution of Mean scores", y = 0.99, x = 0.5),
               xaxis = list(title = ("Score"),
                            zeroline = FALSE),
               yaxis = list(title = "Number of Genes",
                            zeroline = FALSE))

    } else if (center == "msk") {

      gene_constraint_msk <- readRDS("./rda_data/gene_constraint_msk.rda")


      gene_constraint_oe_mis <- plot_ly(xbins = list(size = input$bins)) %>%

        add_histogram(x = ~gene_constraint_msk$oe_mis,
                      hovertemplate = "Number of MorPhiC Genes: %{y} <extra></extra>",
                      name = "Morphic") %>%

        layout(barmode = "stack",
               title = list(text = "O/E MIS: Distribution of Mean scores", y = 0.99, x = 0.5),
               xaxis = list(title = ("Score"),
                            zeroline = FALSE),
               yaxis = list(title = "Number of Genes",
                            zeroline = FALSE))

    } else if (center == "all morphic") {

      gene_constraint_all_morphic <- readRDS("./rda_data/gene_constraint_metrics_morphic_1006.rda")


      gene_constraint_oe_mis <- plot_ly(xbins = list(size = input$bins)) %>%

        add_histogram(x = ~gene_constraint_all_morphic$oe_mis,
                      hovertemplate = "Number of MorPhiC Genes: %{y} <extra></extra>",
                      name = "Morphic") %>%

        layout(barmode = "stack",
               title = list(text = "O/E MIS: Distribution of Mean scores", y = 0.99, x = 0.5),
               xaxis = list(title = ("Score"),
                            zeroline = FALSE),
               yaxis = list(title = "Number of Genes",
                            zeroline = FALSE))

    } else {

      gene_constraint_ucsf <- readRDS("./rda_data/gene_constraint_jax.rda")


      gene_constraint_oe_mis <- plot_ly(xbins = list(size = input$bins)) %>%

        add_histogram(x = ~gene_constraint_ucsf$oe_mis,
                      hovertemplate = "Number of MorPhiC Genes: %{y} <extra></extra>",
                      name = "Morphic") %>%

        layout(barmode = "stack",
               title = list(text = "O/E MIS: Distribution of Mean scores", y = 0.99, x = 0.5),
               xaxis = list(title = ("Score"),
                            zeroline = FALSE),
               yaxis = list(title = "Number of Genes",
                            zeroline = FALSE))

    }

    if (input$compare_protein_coding_genes_gene_constraint) {
      gene_constraint_oe_mis <- gene_constraint_oe_mis %>%
        add_histogram(x = ~gene_constraint_metrics$oe_mis,
                      hovertemplate = "Number of protein coding Genes: %{y} <extra></extra>",
                      name = "Protein coding genes")
      # marker = list(color = "rgb(0, 119, 182)",
      #               line = list(color = "rgb(202, 240, 248)",
      #                           width = 2))) %

      gene_constraint_oe_mis
    } else {
      gene_constraint_oe_mis
    }

  })

  ### DOMINO
  output$gene_constraint_domino <- renderPlotly({
    gene_constraint_metrics <- readRDS("./rda_data/gene_constraint_metrics.rda")

    center <- input$center
    if (center == "jax") {

      gene_constraint_jax <- readRDS("./rda_data/gene_constraint_jax.rda")


      gene_constraint_domino <- plot_ly(xbins = list(size = input$bins)) %>%

        add_histogram(x = ~gene_constraint_jax$domino_score,
                      hovertemplate = "Number of MorPhiC Genes: %{y} <extra></extra>",
                      name = "Morphic") %>%

        layout(barmode = "stack",
               title = list(text = "DOMINO: Distribution of Mean scores", y = 0.99, x = 0.5),
               xaxis = list(title = ("Score"),
                            zeroline = FALSE),
               yaxis = list(title = "Number of Genes",
                            zeroline = FALSE))

    } else if (center == "nwu") {

      gene_constraint_nwu <- readRDS("./rda_data/gene_constraint_nwu.rda")


      gene_constraint_domino <- plot_ly(xbins = list(size = input$bins)) %>%

        add_histogram(x = ~gene_constraint_nwu$domino_score,
                      hovertemplate = "Number of MorPhiC Genes: %{y} <extra></extra>",
                      name = "Morphic") %>%

        layout(barmode = "stack",
               title = list(text = "DOMINO: Distribution of Mean scores", y = 0.99, x = 0.5),
               xaxis = list(title = ("Score"),
                            zeroline = FALSE),
               yaxis = list(title = "Number of Genes",
                            zeroline = FALSE))

    } else if (center == "msk") {

      gene_constraint_msk <- readRDS("./rda_data/gene_constraint_msk.rda")


      gene_constraint_domino <- plot_ly(xbins = list(size = input$bins)) %>%

        add_histogram(x = ~gene_constraint_msk$domino_score,
                      hovertemplate = "Number of MorPhiC Genes: %{y} <extra></extra>",
                      name = "Morphic") %>%

        layout(barmode = "stack",
               title = list(text = "DOMINO: Distribution of Mean scores", y = 0.99, x = 0.5),
               xaxis = list(title = ("Score"),
                            zeroline = FALSE),
               yaxis = list(title = "Number of Genes",
                            zeroline = FALSE))

    } else if (center == "all morphic") {

      gene_constraint_all_morphic <- readRDS("./rda_data/gene_constraint_metrics_morphic_1006.rda")


      gene_constraint_domino <- plot_ly(xbins = list(size = input$bins)) %>%

        add_histogram(x = ~gene_constraint_all_morphic$domino_score,
                      hovertemplate = "Number of MorPhiC Genes: %{y} <extra></extra>",
                      name = "Morphic") %>%

        layout(barmode = "stack",
               title = list(text = "DOMINO: Distribution of Mean scores", y = 0.99, x = 0.5),
               xaxis = list(title = ("Score"),
                            zeroline = FALSE),
               yaxis = list(title = "Number of Genes",
                            zeroline = FALSE))

    } else {

      gene_constraint_ucsf <- readRDS("./rda_data/gene_constraint_jax.rda")


      gene_constraint_domino <- plot_ly(xbins = list(size = input$bins)) %>%
        add_histogram(x = ~gene_constraint_ucsf$domino_score,
                      hovertemplate = "Number of MorPhiC Genes: %{y} <extra></extra>",
                      name = "Morphic") %>%

        layout(barmode = "stack",
               title = list(text = "DOMINO: Distribution of Mean scores", y = 0.99, x = 0.5),
               xaxis = list(title = ("Score"),
                            zeroline = FALSE),
               yaxis = list(title = "Number of Genes",
                            zeroline = FALSE))

    }

    if (input$compare_protein_coding_genes_gene_constraint) {
      gene_constraint_domino <- gene_constraint_domino %>%
        add_histogram(x = ~gene_constraint_metrics$domino_score,
                      hovertemplate = "Number of protein coding Genes: %{y} <extra></extra>",
                      name = "Protein coding genes")
      # marker = list(color = "rgb(0, 119, 182)",
      #               line = list(color = "rgb(202, 240, 248)",
      #                           width = 2))) %

      gene_constraint_domino
    } else {
      gene_constraint_domino
    }

  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # DATA INFORMATION TABLES
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # MORPHIC INFORMATION TAB
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # TEXT ABOVE VISUALISATIONS
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$upset_text <- renderUI({
    HTML("<div style='background-color: white; color: black; padding: 10px; text-align: left; font-size: 18px;'>
         Intersections of Genes selected by respective Data Production Centers
         </div>")
  })

  output$viability_text <- renderUI({
    HTML('<div style="background-color: white; color: black; padding: 10px; text-align: left; font-size: 18px;">
        <p>International Mouse Phenotyping Consortium (IMPC) lethal lines Data:</p>
        <p>Data source: <a href="http://ftp.ebi.ac.uk/pub/databases/impc/all-data-releases/release-19.1/results/" target="_blank">Mouse Viability and Phenotype Data</a></p>
      </div>')
  })

  output$disease_text <- renderUI({
    HTML('<div style="background-color: white; color: black; padding: 10px; text-align: left; font-size: 18px;">
         <p>Disease Data: OMIM (Mendelian disease) Genes Data &amp; Development Disorder Gene to Phenotype Data:</p>
         <p>Data source: <a href="https://www.omim.org/" target="_blank">OMIM data</a></p>
       </div>')

  })

  output$panther_text <- renderUI({
    HTML('<div style="background-color: white; color: black; padding: 10px; text-align: left; font-size: 18px;">
          <p>PANTHER (Protein ANalysis THrough Evolutionary Relationships) protein-coding genes Data:</p>
          <p>Data source: <a href="https://www.pantherdb.org/" target="_blank">PANTHERdb Data</a></p>
          <ul>
            <li>Family and Protein Class (supergrouping of protein families)</li>
            <li>Subfamily (subgroup within the family phylogenetic tree)</li>
          </ul>
        </div>')

  })

  output$cell_essential_text <- renderUI({
    HTML('<div style="background-color: white; color: black; padding: 10px; text-align: left; font-size: 18px;">
          <p>Cellular Core Essentiality Data:</p>
          <p>Threshold (< -0.5)<p>
          <p>Data source: <a href="https://depmap.org/portal/download/all/" target="_blank">DepMap 23Q2 CRISPR Gene effect Data</a></p>
        </div>')

  })

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # TABLES BELOW VISUALISATIONS
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DEFINE THE APP (UI + SERVER)
shinyApp(ui, server)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
