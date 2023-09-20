# Load necessary libraries
library(DT)
library(htmltools)
library(shiny)

## GENE TABLE-----------------------------------------------------------------------------------
# UI ----
geneListPageUI <- function(id) {
  tagList(
    fluidRow(
      # UI part with the new component
      box(
        width = 12,
        solidHeader = TRUE,
        title = "Search Gene",
        status = "primary",
        textInput(NS(id, "gene_search"), label = NULL, placeholder = "DLX3", width = 300),
        textOutput(NS(id, "validate_gene")),
        #checkboxInput(NS(id, "view_summary"), "View gene summary", value = FALSE),
        # conditionalPanel(
        #   condition = "input.view_summary",
        #   fluidRow(
        #     column(
        #       width = 12,
        #       box(
        #         width = 12,
        #         title = textOutput((NS(id, "gene_name_title"))),
        #         status = "info",
        #         # Display invalid gene message if the gene is not valid
        #         uiOutput((NS(id, "invalid_gene_message"))),
        #         fluidRow(
        #           column(width = 6, uiOutput((NS(id, "centers_sumtab")))),
        #           column(width = 6, uiOutput((NS(id, "gene_aliases_sumtab"))))
        #         ),
        #         fluidRow(
        #           column(width = 6, uiOutput((NS(id, "omim_sumtab")))),
        #           column(width = 6, uiOutput((NS(id, "gene_constraint_metrics_sumtab"))))
        #         ),
        #         fluidRow(
        #           column(width = 6, uiOutput((NS(id, "panther_sumtab")))),
        #           column(width = 6, uiOutput((NS(id, "viability_sumtab"))))
        #         )
        #       )
        #     ),
        #
        #   )
        #   , ns = NS("view_summary")
        # )

        # box(
        #   width = 12,
        #   solidHeader = FALSE,
        #   title = "Expand for Gene Summary",
        #   status = "primary",
        #   collapsible = TRUE,
        #   collapsed = TRUE,
        #
        #   fluidRow(
        #     column(
        #       width = 12,
        #       box(
        #         width = 12,
        #         title = textOutput((NS(id, "gene_name_title"))),
        #         status = "info",
        #         # Display invalid gene message if the gene is not valid
        #         uiOutput((NS(id, "invalid_gene_message"))),
        #         fluidRow(
        #           column(width = 6, uiOutput((NS(id, "centers_sumtab")))),
        #           column(width = 6, uiOutput((NS(id, "gene_aliases_sumtab"))))
        #         ),
        #         fluidRow(
        #           column(width = 6, uiOutput((NS(id, "omim_sumtab")))),
        #           column(width = 6, uiOutput((NS(id, "gene_constraint_metrics_sumtab"))))
        #         ),
        #         fluidRow(
        #           column(width = 6, uiOutput((NS(id, "panther_sumtab")))),
        #           column(width = 6, uiOutput((NS(id, "viability_sumtab"))))
        #         )
        #       )
        #     ),
        #
        #   )
        # )
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
                 checkboxGroupInput(NS(id, "data_sources"), label = NULL, inline = TRUE,
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
        title = "Browse MorPhiC Gene list",
        status = "primary",
        DTOutput((NS(id, "genes_table"))
      )
    )
    ),

    fluidRow(
      box(
        solidHeader = FALSE,
        width = 12,
        status = "primary",
        textOutput(NS(id, "qmul_acknowledgement_and_github"))
        )
      )
    )
}


# SERVER -------------------------------------------------------------------------------------------------------------------------------------------------
geneListPageServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Load in gene list with metadata
    app_data_table <- readRDS("./rda_data/app_data_table.rda")
    app_data_table[app_data_table == "-"] <- ""

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
        return(toupper(input$gene_search) %in% gene_data[[input_type]])
      }
      return(FALSE)
    }

    # Valid gene reactive variable
    validGene <- reactive({
      gene_exists(toupper(input$gene_search), detect_input_type(toupper(input$gene_search)))
    })

    output$validate_gene <- renderText({
      user_input_gene_symbol <- input$gene_search
      if (!validGene() && user_input_gene_symbol != "" ) {
        "This gene is not part of the Morphic pipeline (yet)"
      }
    })

    # gene_name = searched gene
    gene_name <- reactive({
      toupper(input$gene_search)
    })

    # Generate title
    output$gene_name_title <- renderText({
      paste(gene_name(), "Summary Information")
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
            pageLength = 100,
            lengthMenu = c(10, 25, 50, 100),
            scrollX = TRUE,
            searching = FALSE,
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

    output$qmul_acknowledgement_and_github <- renderText({
      "This app was developed by Queen Mary University of London and last updated on: 19/09/2023"
    })

  }
  )}




