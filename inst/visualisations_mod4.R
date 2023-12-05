library(bslib)
library(htmltools)

visualisationsUI <- function(id) {
  
  page_sidebar(
    sidebar = sidebar(
      width = 340,
      open = c('open'),
      div(
        class = "plot_inputs",
        textOutput(NS(id, "text_test")),  # Include textOutput within the div
        selectInput(
          NS(id, "vis_select_dpcs"), 
          "Select Data Production Center", 
          c("JAX", "MSK", "NWU", "UCSF"),
          multiple = TRUE,
          selected = "JAX"
        ),
        fileInput(
          NS(id, "vis_geneList_fileUpload"),
          "Upload gene list as file",
          multiple = TRUE
        ),
        textAreaInput(
          NS(id, "vis_geneList_textInput"),
          "Input gene list as text (separate gene names by , or ; or newline)"
        ),
        checkboxInput(
          NS(id, 'compare_all_protein_coding_genes'),
          "Compare against all protein coding genes"
        )
      )
    ),
    div(
      class = "row",
      div(class = "col",
          card(
            full_screen = TRUE,
            card_header(
              "Mouse Model Data IMPC (left) & MGI (Right)"
            ),
            card_body(
              layout_column_wrap(
                width = 1/2,
                height = 500,
                card(
                  height = 500,
                  full_screen = TRUE,
                  plotlyOutput(
                    NS(id, "impc_chart")
                  )
                ),
                card(
                  height = 500,
                  full_screen = TRUE,
                  plotlyOutput(
                    NS(id, "mgi_chart")
                  )
                )
              )
            ),
          )
      )
    ),
    div(
      class = "row",
      div(class = "col",
          card(
            full_screen = TRUE,
            card_header(
              "Mendelian Disease Data (OMIM)"
            ),
            card_body(
              layout_column_wrap(
                width = 1/2,
                height = 500,
                card(
                  height = 500,
                  full_screen = TRUE,
                  plotlyOutput(
                    NS(id, "has_omim_chart")
                  )
                ),
                card(
                  height = 500,
                  full_screen = TRUE,
                  plotlyOutput(
                    NS(id, "omim_lethality_chart")
                  )
                )
              )
            ),
        )
      )
    ),
    div(
      class = "row",
      div(class = "col",
          card(
            full_screen = TRUE,
            card_header(
              "Gene Constraint Metrics - Cell Line Data"
            ),
            layout_sidebar(
              sidebar = sidebar(
                open = c('open'),
                textInput(NS(id, "gene_search_cell_line_constraint"), "Enter Gene Symbol")
                ),
              card_body(
                layout_column_wrap(
                  width = 1/2,
                  height = 1200,
                  card(
                    height = 500,
                    full_screen = TRUE,
                    plotlyOutput(
                      NS(id, "depmap_plot")
                    )
                  ),
                  card(
                    height = 500,
                    full_screen = TRUE,
                    plotlyOutput(
                      NS(id, "mef_plot")
                    )
                  ),
                  card(
                    height = 500,
                    full_screen = TRUE,
                    plotlyOutput(
                      NS(id, "laminin_plot")
                    )
                  )
                )
              ),
            )
          )
      )
    ),
    div(
      class = "row",
      div(class = "col",
          card(
            full_screen = TRUE,
            card_header(
              "Gene Constraint Metrics - Sequencing Data"
            ),
            layout_sidebar(
              sidebar = sidebar(
                open = c('open'),
                textInput(NS(id, "gene_search_sequencing_constraint"), "Enter Gene Symbol")
                ),
              card_body(
                layout_column_wrap(
                  width = 1/2,
                  height = 1500,
                  card(
                    height = 500,
                    full_screen = TRUE,
                    plotlyOutput(
                      NS(id, "lof_oe_plot")
                    )
                  ),
                  card(
                    height = 500,
                    full_screen = TRUE,
                    plotlyOutput(
                      NS(id, "mis_oe_plot")
                    )
                  ),
                  card(
                    height = 500,
                    full_screen = TRUE,
                    plotlyOutput(
                      NS(id, "shet_rgcme_plot")
                    )
                  ),
                  card(
                    height = 500,
                    full_screen = TRUE,
                    plotlyOutput(
                      NS(id, "shet_post_plot")
                    )
                  ),
                  card(
                    height = 500,
                    full_screen = TRUE,
                    plotlyOutput(
                      NS(id, "domino_plot")
                    )
                  ),
                  card(
                    height = 500,
                    full_screen = TRUE,
                    plotlyOutput(
                      NS(id, "scones_plot")
                    )
                  ),
                  card(
                    height = 500,
                    full_screen = TRUE,
                    plotlyOutput(
                      NS(id, "alpha_missense_plot")
                    )
                  )
                )
              ),
            )
          )
      )
    )
  )
}

visualisationsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    getGeneListsFromDpcSelection <- reactive({
      dpc_selected <- input$vis_select_dpcs
      if (length(dpc_selected) >= 1) {
        gene_lists <- getManyGeneListsFromSelect_vis(dpc_selected)
        return(gene_lists)
      } else {
        print("No data production center selected.")
        return(NULL)
      }
    })
    
    getGeneListsFromFileInput <- reactive({
      # GET FILE NAMES AND ZIP LIKE DPC LIST
      if (!is.null(input$vis_geneList_fileUpload))
      {
        # NOTE: getGeneList_multiFileInput_vis differs from getGeneList_multiFileInput 
        # I need to sort out the format of gene_lists result
        # 2nd output of getGeneList_multiFileInput_vis is a lists of vectors
        # whereas 2nd output of getGeneList_multiFileInput is a lists of many lists
        gene_lists_from_files <- getGeneList_multiFileInput_vis(input$vis_geneList_fileUpload)
        return(gene_lists_from_files[[2]])
      } else 
      {
        print("No gene list file uploaded.")
        return(NULL)
      }
    })
    
    getAllProteinCodingGenesForPlotting <- reactive({
      if (input$compare_all_protein_coding_genes) {
        all_protein_coding_genes <- all.protein.coding.genes$symbol
        all_protein_coding_genes_list <- list(
          list(
            all_protein_coding_genes,
            "all_protein_coding_genes"
          ))      
        return(all_protein_coding_genes_list)
      } else {
        return(NULL)
      }
    })
    
    # combine lists for plotting
    getCurrentGeneListsInput <- reactive({
      from_dpc_selection <- getGeneListsFromDpcSelection()
      from_file_input <- getGeneListsFromFileInput()
      from_protein_coding_select <- getAllProteinCodingGenesForPlotting()
      gene_lists_for_plots <- c(from_dpc_selection, from_file_input, from_protein_coding_select)
      return(gene_lists_for_plots)
    })
    
    # MOUSE PLOTS ----
    getMousePlotsDataImpc <- reactive({
      gene_lists_for_plots <- getCurrentGeneListsInput()
      mouse_data_list <- list()
      for (i in gene_lists_for_plots) {
        mouse_data <- getImpcPlotData(i[[1]])
        mouse_data_list <- c(mouse_data_list, list(mouse_data))
      }
      return(mouse_data_list)
    })
    
    getMousePlotsDataMgi <- reactive({
      gene_lists_for_plots <- getCurrentGeneListsInput()
      mouse_data_list <- list()
      for (i in gene_lists_for_plots) {
        mouse_data <- getMgiPlotData(i[[1]])
        mouse_data_list <- c(mouse_data_list, list(mouse_data))
      }
      return(mouse_data_list)
    })
    
    # DISEASE PLOTS ----
    getDiseasePlotsDataHasOmim <- reactive({
      gene_lists_for_plots <- getCurrentGeneListsInput()
      omim_data_list <- list()
      for (i in gene_lists_for_plots) {
        omim_data <- getHasOmimPlotData(i[[1]])
        omim_data_list <- c(omim_data_list, list(omim_data))
      }
      return(omim_data_list)
    })
    
    getDiseasePlotsDataLethalityOmim <- reactive({
      gene_lists_for_plots <- getCurrentGeneListsInput()
      omim_data_list <- list()
      for (i in gene_lists_for_plots) {
        omim_data <- getOmimLethalityPlotData(i[[1]])
        omim_data_list <- c(omim_data_list, list(omim_data))
      }
      return(omim_data_list)
    })
    
    getConstraintPlotsDataDepMap <- reactive({
      gene_lists_for_plots <- getCurrentGeneListsInput()
      depmap_data_list <- list()
      for (i in gene_lists_for_plots) {
        depmap_data <- gene.constraint.metrics.num.only[gene.constraint.metrics.num.only$gene_symbol %in% (i[[1]]), c('gene_symbol', 'mean_score_all')]
        depmap_data_list <- c(depmap_data_list, list(depmap_data))
      }
      return(depmap_data_list)
    })
    
    #WORKING - Muli plots begining
    output$impc_chart <- renderPlotly({
      plots <- getMousePlotsDataImpc()
      if (length(plots) == 1) {
        generateImpcPlot(plots[[1]])
      } else if (length(plots) > 1) {
        #generateImpcPlot(plots[[2]])
        plots
        percentage_cols <- lapply(plots, function(plot) plot$percentage)
        
        # Bind the percentage column
        df <- data.frame(x_axis = c("lethal", "subviable", "viable"))
        df <- bind_cols(df, !!!percentage_cols)
        # Rename the columns
        #colnames(df) <- c("x_axis", paste0("percentage_set", 1:length(percentage_cols)))
        # Extract the second elements (list names) from gene_lists_for_plots
        gene_lists_for_plots <- getCurrentGeneListsInput()
        
        list_names <- sapply(gene_lists_for_plots, function(x) x[[2]])
        
        # Create column names for the dataframe
        col_names <- c("x_axis", list_names)
        
        # Assign column names to your dataframe (replace df with your actual dataframe)
        colnames(df) <- col_names
        # set y_col as first value name for initial plotly obj
        y_col <- names(df)[2] # first value after xaxis column
        y_col
        p <- plot_ly(df, x = ~x_axis, y = as.formula(paste0("~", y_col)),
                     type = 'bar', name = y_col, textposition = 'outside', text = ~get(y_col)) %>%
          plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'IMPC preweaning viability assessment'))
        p
        # set y_cols2 for rest of value names for traces
        y_cols1<- names(df)[-1]
        y_cols2 <- y_cols1[-1]
        # Add traces
        for (i in y_cols2) {
          text_col <- paste0("text_", i)  # New variable for dynamic text
          df[[text_col]] <- df[[i]]
          
          p <- p %>%
            add_trace(data = df, y = as.formula(paste0("~", i)), name = i, text = as.formula(paste0("~", text_col)))
        }
        # Print the resulting plot
        p
      }
    })
    
    output$mgi_chart <- renderPlotly({
      plots <- getMousePlotsDataMgi()
      if (length(plots) == 1) {
        generateMgiPlot(plots[[1]])
      } else if (length(plots) > 1) {
        #generateImpcPlot(plots[[2]])
        plots
        percentage_cols <- lapply(plots, function(plot) plot$percentage)
        
        # Bind the percentage column
        df <- data.frame(x_axis = c("lethal", "viable"))
        df <- bind_cols(df, !!!percentage_cols)
        # Rename the columns
        #colnames(df) <- c("x_axis", paste0("percentage_set", 1:length(percentage_cols)))
        # Extract the second elements (list names) from gene_lists_for_plots
        gene_lists_for_plots <- getCurrentGeneListsInput()
        
        list_names <- sapply(gene_lists_for_plots, function(x) x[[2]])
        
        # Create column names for the dataframe
        col_names <- c("x_axis", list_names)
        
        # Assign column names to your dataframe (replace df with your actual dataframe)
        colnames(df) <- col_names
        # set y_col as first value name for initial plotly obj
        y_col <- names(df)[2] # first value after xaxis column
        y_col
        p <- plot_ly(df, x = ~x_axis, y = as.formula(paste0("~", y_col)),
                     type = 'bar', name = y_col, textposition = 'outside', text = ~get(y_col)) %>%
          plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'MGI viability assessment'))
        p
        # set y_cols2 for rest of value names for traces
        y_cols1<- names(df)[-1]
        y_cols2 <- y_cols1[-1]
        # Add traces
        for (i in y_cols2) {
          text_col <- paste0("text_", i)  # New variable for dynamic text
          df[[text_col]] <- df[[i]]
          
          p <- p %>%
            add_trace(data = df, y = as.formula(paste0("~", i)), name = i, text = as.formula(paste0("~", text_col)))
        }
        # Print the resulting plot
        p
      }
    })
    
    output$has_omim_chart <- renderPlotly({
      plots <- getDiseasePlotsDataHasOmim()
      if (length(plots) == 1) {
        generateHasOmimPlot(plots[[1]])
      } else if (length(plots) > 1) {
        #generateImpcPlot(plots[[2]])
        plots
        percentage_cols <- lapply(plots, function(plot) plot$percentage)
        
        # Bind the percentage column
        df <- data.frame(x_axis = c("yes", "no"))
        df <- bind_cols(df, !!!percentage_cols)
        # Rename the columns
        #colnames(df) <- c("x_axis", paste0("percentage_set", 1:length(percentage_cols)))
        # Extract the second elements (list names) from gene_lists_for_plots
        gene_lists_for_plots <- getCurrentGeneListsInput()
        
        list_names <- sapply(gene_lists_for_plots, function(x) x[[2]])
        
        # Create column names for the dataframe
        col_names <- c("x_axis", list_names)
        
        # Assign column names to your dataframe (replace df with your actual dataframe)
        colnames(df) <- col_names
        # set y_col as first value name for initial plotly obj
        y_col <- names(df)[2] # first value after xaxis column
        y_col
        p <- plot_ly(df, x = ~x_axis, y = as.formula(paste0("~", y_col)),
                     type = 'bar', name = y_col, textposition = 'outside', text = ~get(y_col)) %>%
          plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'Mendelian disease association (OMIM)'))
        p
        # set y_cols2 for rest of value names for traces
        y_cols1<- names(df)[-1]
        y_cols2 <- y_cols1[-1]
        # Add traces
        for (i in y_cols2) {
          text_col <- paste0("text_", i)  # New variable for dynamic text
          df[[text_col]] <- df[[i]]
          
          p <- p %>%
            add_trace(data = df, y = as.formula(paste0("~", i)), name = i, text = as.formula(paste0("~", text_col)))
        }
        # Print the resulting plot
        p
      }
    })
    
    # Add another x axis for non-disease gene
    output$omim_lethality_chart <- renderPlotly({
      plots <- getDiseasePlotsDataLethalityOmim()
      if (length(plots) == 1) {
        generateOmimLethalityPlot(plots[[1]])
      } else if (length(plots) > 1) {
        #generateImpcPlot(plots[[2]])
        plots
        percentage_cols <- lapply(plots, function(plot) plot$percentage)
        
        # Bind the percentage column
        df <- data.frame(x_axis = c("lethal", "nonlethal"))
        df <- bind_cols(df, !!!percentage_cols)
        # Rename the columns
        #colnames(df) <- c("x_axis", paste0("percentage_set", 1:length(percentage_cols)))
        # Extract the second elements (list names) from gene_lists_for_plots
        gene_lists_for_plots <- getCurrentGeneListsInput()
        
        list_names <- sapply(gene_lists_for_plots, function(x) x[[2]])
        
        # Create column names for the dataframe
        col_names <- c("x_axis", list_names)
        
        # Assign column names to your dataframe (replace df with your actual dataframe)
        colnames(df) <- col_names
        # set y_col as first value name for initial plotly obj
        y_col <- names(df)[2] # first value after xaxis column
        y_col
        p <- plot_ly(df, x = ~x_axis, y = as.formula(paste0("~", y_col)),
                     type = 'bar', name = y_col, textposition = 'outside', text = ~get(y_col)) %>%
          plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'Lethal Phenotypes (OMIM)'))
        p
        # set y_cols2 for rest of value names for traces
        y_cols1<- names(df)[-1]
        y_cols2 <- y_cols1[-1]
        # Add traces
        for (i in y_cols2) {
          text_col <- paste0("text_", i)  # New variable for dynamic text
          df[[text_col]] <- df[[i]]
          
          p <- p %>%
            add_trace(data = df, y = as.formula(paste0("~", i)), name = i, text = as.formula(paste0("~", text_col)))
        }
        # Print the resulting plot
        p
      }
    })
    
    # Cell line metrics
    output$depmap_plot <- renderPlotly({
      gene_lists_for_plots <- getCurrentGeneListsInput()
      req(length(gene_lists_for_plots) >= 1)
      metric_col_name <- 'mean_score_all'
      x_axis_text <- 'DepMap mean Gene Effect score '
      plot <- constraintMetricsPlots(gene_lists_for_plots, metric_col_name, x_axis_text)
      
      # Threshold line
      threshold_value_05 <- -0.5
      threshold_value_1 <- -1
      plot <- plot %>%
        plotly::layout(shapes = list(hline(threshold_value_05), hline(threshold_value_1)))
      plot
      
      # Highlight gene
      if (input$gene_search_cell_line_constraint != "") {
        plot <- addGeneTrace(plot, metric_col_name, input$gene_search_cell_line_constraint, x_axis_text)
        return(plot)
      } else {
        return(plot)
      }
    })
    
    # Add fdr to hover info
    output$mef_plot <- renderPlotly({
      gene_lists_for_plots <- getCurrentGeneListsInput()
      req(length(gene_lists_for_plots) >= 1)
      metric_col_name <- 'bf_mef'
      x_axis_text <- 'Bayes Factor (MEF) '
      plot <- constraintMetricsPlots(gene_lists_for_plots, metric_col_name, x_axis_text)
      
      if (input$gene_search_cell_line_constraint != "") {
        plot <- addGeneTrace(plot, metric_col_name, input$gene_search_cell_line_constraint, x_axis_text)
        return(plot)
      } else {
        return(plot)
      }
    })
    
    # Add fdr to hover info
    output$laminin_plot <- renderPlotly({
      gene_lists_for_plots <- getCurrentGeneListsInput()
      req(length(gene_lists_for_plots) >= 1)
      metric_col_name <- 'bf_lam'
      x_axis_text <- 'Bayes Factor (Laminin) '
      plot <- constraintMetricsPlots(gene_lists_for_plots, metric_col_name, x_axis_text)
      
      if (input$gene_search_cell_line_constraint != "") {
        plot <- addGeneTrace(plot, metric_col_name, input$gene_search_cell_line_constraint, x_axis_text)
        return(plot)
      } else {
        return(plot)
      }
    })
    
    # Sequencing metrics
    output$lof_oe_plot <- renderPlotly({
      gene_lists_for_plots <- getCurrentGeneListsInput()
      req(length(gene_lists_for_plots) >= 1)
      metric_col_name <- 'lof_oe'
      x_axis_text <- 'Loss of Function - observed/expected score '
      plot <- constraintMetricsPlots(gene_lists_for_plots, metric_col_name, x_axis_text)
      
      # Threshold line
      threshold_value <- 0.35
      plot <- plot %>%
        plotly::layout(shapes = list(hline(threshold_value)))
      plot
      
      if (input$gene_search_sequencing_constraint != "") {
        plot <- addGeneTrace(plot, metric_col_name, input$gene_search_sequencing_constraint, x_axis_text)
        return(plot)
      } else {
        return(plot)
      }
    })
    
    output$mis_oe_plot <- renderPlotly({
      gene_lists_for_plots <- getCurrentGeneListsInput()
      req(length(gene_lists_for_plots) >= 1)
      metric_col_name <- 'mis_oe'
      x_axis_text <- 'Missense - observed/expected score '
      plot <- constraintMetricsPlots(gene_lists_for_plots, metric_col_name, x_axis_text)
      
      if (input$gene_search_sequencing_constraint != "") {
        plot <- addGeneTrace(plot, metric_col_name, input$gene_search_sequencing_constraint, x_axis_text)
        return(plot)
      } else {
        return(plot)
      }
    })
    
    output$shet_rgcme_plot <- renderPlotly({
      gene_lists_for_plots <- getCurrentGeneListsInput()
      req(length(gene_lists_for_plots) >= 1)
      metric_col_name <- 'mean'
      x_axis_text <- 'Shet mean score (RGCME) '
      plot <- constraintMetricsPlots(gene_lists_for_plots, metric_col_name, x_axis_text)
      
      # Threshold line
      threshold_value <- 0.075
      plot <- plot %>%
        plotly::layout(shapes = list(hline(threshold_value)))
      plot
      
      if (input$gene_search_sequencing_constraint != "") {
        plot <- addGeneTrace(plot, metric_col_name, input$gene_search_sequencing_constraint, x_axis_text)
        return(plot)
      } else {
        return(plot)
      }
    })
    
    output$shet_post_plot <- renderPlotly({
      gene_lists_for_plots <- getCurrentGeneListsInput()
      req(length(gene_lists_for_plots) >= 1)
      metric_col_name <- 'post_mean'
      x_axis_text <- 'Shet mean score (Posterior probability) '
      plot <- constraintMetricsPlots(gene_lists_for_plots, metric_col_name, x_axis_text)
      
      # Threshold line
      threshold_value <- 0.1
      plot <- plot %>%
        plotly::layout(shapes = list(hline(threshold_value)))
      plot
      
      if (input$gene_search_sequencing_constraint != "") {
        plot <- addGeneTrace(plot, metric_col_name, input$gene_search_sequencing_constraint, x_axis_text)
        return(plot)
      } else {
        return(plot)
      }
    })
    
    output$domino_plot <- renderPlotly({
      gene_lists_for_plots <- getCurrentGeneListsInput()
      req(length(gene_lists_for_plots) >= 1)
      metric_col_name <- 'DOMINO'
      x_axis_text <- 'DOMINO score '
      plot <- constraintMetricsPlots(gene_lists_for_plots, metric_col_name, x_axis_text)
      
      if (input$gene_search_sequencing_constraint != "") {
        plot <- addGeneTrace(plot, metric_col_name, input$gene_search_sequencing_constraint, x_axis_text)
        return(plot)
      } else {
        return(plot)
      }
    })
    
    output$scones_plot <- renderPlotly({
      gene_lists_for_plots <- getCurrentGeneListsInput()
      req(length(gene_lists_for_plots) >= 1)
      metric_col_name <- 'SCoNeS'
      x_axis_text <- 'SCoNeS score '
      plot <- constraintMetricsPlots(gene_lists_for_plots, metric_col_name, x_axis_text)
      
      if (input$gene_search_sequencing_constraint != "") {
        plot <- addGeneTrace(plot, metric_col_name, input$gene_search_sequencing_constraint, x_axis_text)
        return(plot)
      } else {
        return(plot)
      }
    })
    
    output$alpha_missense_plot <- renderPlotly({
      gene_lists_for_plots <- getCurrentGeneListsInput()
      req(length(gene_lists_for_plots) >= 1)
      metric_col_name <- 'mean_am_pathogenicity'
      x_axis_text <- 'Alpha Missense mean score '
      plot <- constraintMetricsPlots(gene_lists_for_plots, metric_col_name, x_axis_text)
      
      if (input$gene_search_sequencing_constraint != "") {
        plot <- addGeneTrace(plot, metric_col_name, input$gene_search_sequencing_constraint, x_axis_text)
        return(plot)
      } else {
        return(plot)
      }
    })
    
    
    
    
    
    # getMousePlot <- reactive({
    #   gene_lists_for_plots <- getCurrentGeneListsInput()
    #   gene_list_for_plot <- gene_lists_for_plots[[1]]
    #   
    #   mouse_data <- getImpcPlotData(gene_list_for_plot)
    #   
    #   return(mouse_data)
    # })
    
    # observeEvent(input$compare_all_protein_coding_genes, {
    #   x72 <- getMousePlots()
    #   print("FILE INPUT")
    #   print(x72)
    # })

    # # Check combined gene lists list
    # observeEvent(c(input$vis_geneList_fileUpload, input$vis_select_dpcs), {
    #   x11 <- getCurrentGeneListsInput()
    #   print("COMBINED INPUTS")
    #   print(x11)
    # })
    
    # Omitting for now because we need to add an action button in order to add multiple text inputs
    # output$vis_geneList_textInput <- reactive({
    #   if (input$vis_geneList_textInput != '')
    #   {
    #     gene_list <- parseTextAreaInput(input$vis_geneList_textInput)
    #   }
    #   return(
    #     gene_list <- parseTextAreaInput(input$vis_geneList_textInput)
    #     
    #   )
    # })
    
    # observeEvent(input$vis_geneList_fileUpload, {
    #   x99 <- getGeneListsFromFileInput()
    #   print("FILE INPUT")
    #   print(x99)
    # })
    # 
    # observeEvent(input$vis_select_dpcs, {
    #   x89 <- getGeneListsFromDpcSelection()
    #   print("DPC SELECT")
    #   print(x89)
    # })
    
    # observeEvent(c(input$compare_all_protein_coding_genes), {
    #   # from_protein_coding_select <- getAllProteinCodingGenesForPlotting()
    #   # print(from_protein_coding_select[[1]][1:10])
    #   # print(from_protein_coding_select[[2]])
    #   
    #   xx <- getCurrentGeneListsInput()
    #   print(xx)
    # })
    
    # observeEvent(c(input$vis_geneList_fileUpload, input$vis_select_dpcs), {
    #   gene_lists_for_plots <- getCurrentGeneListsInput()
    #   
    #   list_names <- sapply(gene_lists_for_plots, function(x) x[[2]])
    #   print(list_names)
    # })
    
    # WORKS - Singular plot
    # output$impc_chart <- renderPlotly({
    #   plot <- getMousePlot()
    #   chart <- generateImpcPlot(plot)
    #   chart
    # })
  })
}
