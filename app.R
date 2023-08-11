if (!require(shiny)) install.packages(c("shiny", "shinydashboard", "DT", "plotly", "UpSetR", "ggplot2"))
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(UpSetR)
library(ggplot2)

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
  title = "MorPhiC"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UI - SIDEBAR
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("MorPhiC Genes", tabName = "genes", icon = icon("table")),
    menuItem("Visualisations", tabName = "visualisations", icon = icon("chart-bar")),
    menuItem("Data info", tabName = "data_info", icon = icon("info"))
  )
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UI - BODY
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
body <- dashboardBody(
  fillPage = TRUE,
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
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # DATA VISUALISATIONS TAB
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tabItem(
      tabName = "visualisations",
      tabsetPanel(
        tabPanel("UpSet Plot", id = "upset_plot_tab",
                 plotOutput("upsetplot", height = "60vh")
        ),

        tabPanel("Viability", id = "viability_tab",
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
                         choices = c("all morphic", "jax", "msk", "nwu", "ucsf"),
                         selected = "all morphic"  # Set the default selected option
                       )
                     )
                   )
                 )
        ),

        tabPanel("OMIM", id = "omim_tab",
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
                         choices = c("all morphic", "jax", "msk", "nwu", "ucsf"),
                         selected = "all morphic"  # Set the default selected option
                       )
                     )
                   )
                 )
        ),

        tabPanel("Panther", id = "panther_tab",
                 fluidRow(
                   column(width = 9, plotlyOutput("panther_plot", height = "60vh")),
                   column(
                     width = 3,
                     box(
                       width = 12,
                       title = "Options",
                       status = "primary",
                       selectInput(
                         "center_panther",
                         "Select center gene list:",
                         choices = c("all morphic", "jax", "msk", "nwu", "ucsf"),
                         selected = "all morphic"  # Set the default selected option
                       )
                     )
                   )
                 )
        ),

        tabPanel("GO Semantic Similarity", id = "go_scatter_tab",
                 fluidRow(
                   column(width = 9, plotlyOutput("go_scatter", height = "60vh")),
                   column(
                     width = 3,
                     box(
                       width = 12,
                       title = "Options",
                       status = "primary",
                       selectInput(
                         "center_go",
                         "Select center gene list:",
                         choices = c("all morphic", "jax", "msk", "nwu", "ucsf"),
                         selected = "all morphic"  # Set the default selected option
                       )
                     )
                   )
                 )
        ),

        tabPanel("Reactome Enrichment Map", id = "reactome_emapplot_tab",
                 fluidRow(
                   column(width = 9, plotOutput("reactome_emapplot", height = "60vh")),
                   column(
                     width = 3,
                     box(
                       width = 12,
                       title = "Options",
                       status = "primary",
                       selectInput(
                         "center_reactome",
                         "Select center gene list:",
                         choices = c("all morphic", "jax", "msk", "nwu", "ucsf"),
                         selected = "all morphic"  # Set the default selected option
                       )
                     )
                   )
                 )
        ),

        tabPanel("Cellular Essentiality (barcharts)", id = "cell_essentiality_plot_tab",
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
                         choices = c("all morphic", "jax", "msk", "nwu", "ucsf"),
                         selected = "all morphic"  # Set the default selected option
                       )
                     )
                   )
                 )
        ),

        tabPanel("Cell essentiallity (boxplots)", id = "cell_essentiality_boxplots_tab",
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
                       choices = c("all morphic", "jax", "msk", "nwu", "ucsf"),
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
        ),

        tabPanel("Gene constraint metrics", id = "gene_constraint_metrics_tab",
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DEFINE UI
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    #data_subset <- github_plus_pilarsnew4_plus_mine_nodups
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
        th(colspan = 3, 'ItV Metrics: DepMap'),
        th(colspan = 3, 'ItV Metrics: MEF hPSCs'),
        th(colspan = 3, 'ItV Metrics: Laminin hPSCs'),
        th(colspan = 3, 'ItV Metrics: O/E LOF'),
        th(colspan = 3, 'ItV Metrics: O/E Missense'),
        th(colspan = 3, 'ItV Metrics: Shet RGC-ME'),
        th(colspan = 3, 'ItV Metrics: Shet posterior'),
        th(colspan = 1, 'ItV Metrics: DOMINO'),
        th(colspan = 1, 'ItV Metrics: SCoNeS'),
        th(colspan = 4, 'DDGene2Phenotype'),
        th(colspan = 4, 'OMIM'),
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
        th('viability'), th('one2one ortholog'), th('phenotypes homozygote'), th('phenotypes heterozygote'), th('phenotypes hemizygote'),
        th('mean gene effect score'), th('essential (p < 0.05'), th('essential (p < 0.01)'), th('Bayes Factor'), th('False Discovery Rate'),  th('Essential/Non-essential'), th('Bayes Factor'), th('False Discovery Rate'),  th('Essential/Non-essential'), th('Score'), th('Lower score'), th('Upper score'), th('Score'), th('Lower score'), th('Upper score'), th('Mean'), th('Lower'), th('Upper'), th('Mean'), th('Lower 95'), th('Upper 95'), th('Score'), th('Score'),
        th('disease name'), th('confidence category'), th('allelic requirement'), th('organ specificity'),
        th('phenotype'), th('mode of inheritance'), th('gene lethality'), th('earliest lethality category'),
        th('ID'), th('Term'), th('ID'), th('Term'), th('ID'), th('Term'),
        th('ID'), th('Term'), th('ID'), th('Term'), th('Term'),
        th('Path ID'), th('Path name')
      )
    )
  ))


  #Render the DT table
  output$genes_table <- renderDT({
    datatable(
      filtered_data(),
      container = headers, # Use the header structure as the container
      options = list(
        pageLength = 10,  # Number of rows displayed per page
        lengthMenu = c(10, 25, 50, 100),  # Choose rows per page options
        scrollX = TRUE,
        searching = TRUE,
        columnDefs = list(
          list(
            targets = "_all",
            render = JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data.length > 25 ?",
              "'<span title=\"' + data + '\">' + data.substr(0, 25) + '...</span>' : data;",
              "}")
          )
        )
      ),
      rownames = FALSE  # Hide row numbers
    )
  })


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
      gene_data <- github_plus_pilarsnew4_plus_mine_nodups
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
        "Please enter a valid gene name or HGNC ID."
      )
    }
  })

  output$viability_sumtab <- renderUI({
    if (validGene()) {
      renderValueBox({
        input_type <- detect_input_type(toupper(input$gene_search))
            select_row <- github_plus_pilarsnew4_plus_mine_nodups[
              github_plus_pilarsnew4_plus_mine_nodups[[input_type]] == toupper(input$gene_search), ]
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
        select_row <- github_plus_pilarsnew4_plus_mine_nodups[
          github_plus_pilarsnew4_plus_mine_nodups[[input_type]] == toupper(input$gene_search), ]
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
            select_row <- github_plus_pilarsnew4_plus_mine_nodups[
              github_plus_pilarsnew4_plus_mine_nodups[[input_type]] == toupper(input$gene_search), ]
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
            select_row <- github_plus_pilarsnew4_plus_mine_nodups[
              github_plus_pilarsnew4_plus_mine_nodups[[input_type]] == toupper(input$gene_search), ]
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
        select_row <- github_plus_pilarsnew4_plus_mine_nodups[
          github_plus_pilarsnew4_plus_mine_nodups[[input_type]] == toupper(input$gene_search), ]
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
        select_row <- github_plus_pilarsnew4_plus_mine_nodups[
          github_plus_pilarsnew4_plus_mine_nodups[[input_type]] == toupper(input$gene_search), ]

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
    if (center == "jax") {
      # Load JAX Panther plot object (jax_plot_panther_all.rda)
      jax_plot <- readRDS("./rda_data/jax_plot_panther_all.rda")
      # Replace with code to customize the JAX Panther plot
      jax_plot  # Return the plot object
    } else if (center == "msk") {
      # Load MSK Panther plot object (msk_plot_panther_all.rda)
      msk_plot <- readRDS("./rda_data/msk_plot_panther_all.rda")
      # Replace with code to customize the MSK Panther plot
      msk_plot  # Return the plot object
    } else if (center == "nwu") {
      # Load NWU Panther plot object (nwu_plot_panther_all.rda)
      nwu_plot <- readRDS("./rda_data/nwu_plot_panther_all.rda")
      # Replace with code to customize the NWU Panther plot
      nwu_plot  # Return the plot object
    } else if (center == "all morphic") {
      all_plot <- readRDS("./rda_data/all_morphic_plot_panther_all.rda")
      # Replace with code to customize the NWU viability plot
      all_plot  # Return the plot object
    } else {
      # Load UCSF Panther plot object (ucsf_plot_panther_all.rda)
      ucsf_plot <- readRDS("./rda_data/ucsf_plot_panther_all.rda")
      # Replace with code to customize the UCSF Panther plot
      ucsf_plot  # Return the plot object
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

  # Render the Reactome emapplot plot based on the selected center
  # Not using plotly as emapplot not supported
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

    # MODIFY PLOT TEXT TO REFLECT THRESHOLD SET i.e. label will be like: essential (<0.01)
    if (center == "jax") {
      cell_essentiality_jax <- readRDS("./rda_data/cell_essentiality_jax.rda")

      sig_threshod_depmap_boxplot <- input$cell_essential_depmap_significance_threshold
      if (sig_threshod_depmap_boxplot == "0.05") {
        plot_text <- cell_essentiality_jax$depmap_essential_05_label
      } else if (sig_threshod_depmap_boxplot == "0.01") {
        plot_text <- cell_essentiality_jax$depmap_essential_1_label
      }


      cell_essentiality_depmap_boxplot <- cell_essentiality_jax %>%
        plot_ly(name = center, y = ~mean_depmap_gene_effect_score, x = center, type = "box",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_jax$hgnc_id, "<br> Mean gene effect score :", cell_essentiality_jax$mean_depmap_gene_effect_score,
                                                      "\n", plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_jax

    } else if (center == "nwu") {
      cell_essentiality_nwu <- readRDS("./rda_data/cell_essentiality_nwu.rda")

      sig_threshod_depmap_boxplot <- input$cell_essential_depmap_significance_threshold
      if (sig_threshod_depmap_boxplot == "0.05") {
        plot_text <- cell_essentiality_nwu$depmap_essential_05_label
      } else if (sig_threshod_depmap_boxplot == "0.01") {
        plot_text <- cell_essentiality_nwu$depmap_essential_1_label
      }

      cell_essentiality_depmap_boxplot <- cell_essentiality_nwu %>%
        plot_ly(name = center, y = ~mean_depmap_gene_effect_score, x = center, type = "box",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_nwu$hgnc_id, "<br> Mean gene effect score :", cell_essentiality_nwu$mean_depmap_gene_effect_score,
                                                      "\n", plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_nwu

    } else if (center == "msk") {
      cell_essentiality_msk <- readRDS("./rda_data/cell_essentiality_msk.rda")

      sig_threshod_depmap_boxplot <- input$cell_essential_depmap_significance_threshold
      if (sig_threshod_depmap_boxplot == "0.05") {
        plot_text <- cell_essentiality_msk$depmap_essential_05_label
      } else if (sig_threshod_depmap_boxplot == "0.01") {
        plot_text <- cell_essentiality_msk$depmap_essential_1_label
      }

      cell_essentiality_depmap_boxplot <- cell_essentiality_msk %>%
        plot_ly(name = center, y = ~mean_depmap_gene_effect_score, x = center, type = "box",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_msk$hgnc_id, "<br> Mean gene effect score :", cell_essentiality_msk$mean_depmap_gene_effect_score,
                                                      "\n", plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_msk

    } else if (center == "all morphic") {
      cell_essentiality_all_morphic <- readRDS("./rda_data/cell_essentiality_all_morphic.rda")

      sig_threshod_depmap_boxplot <- input$cell_essential_depmap_significance_threshold
      if (sig_threshod_depmap_boxplot == "0.05") {
        plot_text <- cell_essentiality_all_morphic$depmap_essential_05_label
      } else if (sig_threshod_depmap_boxplot == "0.01") {
        plot_text <- cell_essentiality_all_morphic$depmap_essential_1_label
      }

      cell_essentiality_depmap_boxplot <- cell_essentiality_all_morphic %>%
        plot_ly(name = center, y = ~mean_depmap_gene_effect_score, x = center, type = "box",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_all_morphic$hgnc_id, "<br> Mean gene effect score :", cell_essentiality_all_morphic$mean_depmap_gene_effect_score,
                                                      "\n", plot_text)) %>%
        layout(shapes = list(hline(threshold_value)))

      cell_essentiality_centre_selected <- cell_essentiality_all_morphic

    } else {
      cell_essentiality_ucsf <- readRDS("./rda_data/cell_essentiality_ucsf.rda")

      sig_threshod_depmap_boxplot <- input$cell_essential_depmap_significance_threshold
      if (sig_threshod_depmap_boxplot == "0.05") {
        plot_text <- cell_essentiality_ucsf$depmap_essential_05_label
      } else if (sig_threshod_depmap_boxplot == "0.01") {
        plot_text <- cell_essentiality_ucsf$depmap_essential_1_label
      }

      cell_essentiality_depmap_boxplot <- cell_essentiality_ucsf %>%
        plot_ly(name = center, y = ~mean_depmap_gene_effect_score, x = center, type = "box",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_ucsf$hgnc_id,
                                                      "<br> Mean gene effect score :", cell_essentiality_ucsf$mean_depmap_gene_effect_score,
                                                      "\n", plot_text)) %>%
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
      # Code to generate graph2 when the checkbox is checked
      # ...
      # Your code here to generate graph2
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
        plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "box",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_jax$hgnc_id, "<br> Bayes factor :", cell_essentiality_jax$h1_mef_BF,
                                                      "\n", cell_essentiality_jax$h1_mef_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "nwu") {
      cell_essentiality_nwu <- readRDS("./rda_data/cell_essentiality_nwu.rda")
      cell_essentiality_mef_boxplot <- cell_essentiality_nwu %>%
        plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "box",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_nwu$hgnc_id, "<br> Bayes factor :", cell_essentiality_nwu$h1_mef_BF,
                                                      "\n", cell_essentiality_nwu$h1_mef_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "msk") {
      cell_essentiality_msk <- readRDS("./rda_data/cell_essentiality_msk.rda")
      cell_essentiality_mef_boxplot <- cell_essentiality_msk %>%
        plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "box",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_msk$hgnc_id, "<br> Bayes factor :", cell_essentiality_msk$h1_mef_BF,
                                                      "\n", cell_essentiality_msk$h1_mef_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "all morphic") {
      cell_essentiality_all_morphic <- readRDS("./rda_data/cell_essentiality_all_morphic.rda")
      cell_essentiality_mef_boxplot <- cell_essentiality_all_morphic %>%
        plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "box",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_all_morphic$hgnc_id, "<br> Bayes factor :", cell_essentiality_all_morphic$h1_mef_BF,
                                                      "\n", cell_essentiality_all_morphic$h1_mef_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else {
      cell_essentiality_ucsf <- readRDS("./rda_data/cell_essentiality_ucsf.rda")
      cell_essentiality_mef_boxplot <- cell_essentiality_ucsf %>%
        plot_ly(name = center, y = ~h1_mef_BF, x = center, type = "box",
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
        plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "box",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_jax$hgnc_id, "<br> Bayes factor :", cell_essentiality_jax$h1_laminin_BF,
                                                      "\n", cell_essentiality_jax$h1_laminin_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "nwu") {
      cell_essentiality_nwu <- readRDS("./rda_data/cell_essentiality_nwu.rda")
      cell_essentiality_laminin_boxplot <- cell_essentiality_nwu %>%
        plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "box",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_nwu$hgnc_id, "<br> Bayes factor :", cell_essentiality_nwu$h1_laminin_BF,
                                                      "\n", cell_essentiality_nwu$h1_laminin_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "msk") {
      cell_essentiality_msk <- readRDS("./rda_data/cell_essentiality_msk.rda")
      cell_essentiality_laminin_boxplot <- cell_essentiality_msk %>%
        plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "box",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_msk$hgnc_id, "<br> Bayes factor :", cell_essentiality_msk$h1_laminin_BF,
                                                      "\n", cell_essentiality_msk$h1_laminin_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else if (center == "all morphic") {
      cell_essentiality_all_morphic <- readRDS("./rda_data/cell_essentiality_all_morphic.rda")
      cell_essentiality_laminin_boxplot <- cell_essentiality_all_morphic %>%
        plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "box",
                hoverinfo = "text", hovertext = paste("HGNC ID :", cell_essentiality_all_morphic$hgnc_id, "<br> Bayes factor :", cell_essentiality_all_morphic$h1_laminin_BF,
                                                      "\n", cell_essentiality_all_morphic$h1_laminin_essential_label)) %>%
        layout(shapes = list(hline(threshold_value)))

    } else {
      cell_essentiality_ucsf <- readRDS("./rda_data/cell_essentiality_ucsf.rda")
      cell_essentiality_laminin_boxplot <- cell_essentiality_ucsf %>%
        plot_ly(name = center, y = ~h1_laminin_BF, x = center, type = "box",
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
    datatable(data_info_tables[[1]], options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_center_info <- DT::renderDataTable({
    datatable(data_info_tables[[2]], options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_impc <- DT::renderDataTable({
    datatable(data_info_tables[[3]], options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_depmap <- DT::renderDataTable({
    datatable(data_info_tables[[4]], options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_itv_metrics <- DT::renderDataTable({
    datatable(data_info_tables[[5]], options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_ddg2p <- DT::renderDataTable({
    datatable(data_info_tables[[6]], options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_omim <- DT::renderDataTable({
    datatable(data_info_tables[[7]], options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_go <- DT::renderDataTable({
    datatable(data_info_tables[[8]], options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_panther <- DT::renderDataTable({
    datatable(data_info_tables[[9]], options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })

  output$table_reactome <- DT::renderDataTable({
    datatable(reactome, options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, rownames = FALSE))
  })
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DEFINE THE APP (UI + SERVER)
shinyApp(ui, server)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
