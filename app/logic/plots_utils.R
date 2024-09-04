# plots_utils.R
box::use(
  shiny[NS, fluidRow, column, req, selectInput, textAreaInput,
        checkboxInput, p, bindCache, actionButton, showModal, modalDialog,
        tagList, observeEvent, fluidPage],
  purrr[map_dfr, map, imap],
  dplyr[...],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, toWebGL, add_trace,
         highlight, subplot],
  stats[as.formula],
  bslib[card, card_body, card_footer, card_title],
  htmltools[a, p, div, h6, em],
  shinycssloaders[withSpinner],
  upsetjs[...],
  UpSetR[...]
)

# Barchart plots ----
#' @export
barChartUI <- function(plot_ids, select_args, footers, titles, id, session) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      class = "align-items-center",
      column(
        width = 3,
        selectInput(session$ns('gene_list_picker'), 'Select gene list',
                    choices = names(select_args),
                    selected = names(select_args),
                    multiple = TRUE)
      ),
      column(
        width = 7,
        selectInput(session$ns('percentage_counts_picker'), 'Y-axis option',
                    choices = c('percentage of genes', 'number of genes'),
                    selected = 'percentage of genes',
                    multiple = FALSE)
      ),
      column(
        width = 2,
        div(style = "text-align: right;", actionButton(session$ns("modal"), "Plots info"))
      )
    ),
    fluidRow(
      em('Note: Hover over a plot and click the expand button at the bottom right to display the plot full screen')
    ),
    fluidRow(
      imap(
        plot_ids, ~ {
          title <- titles[[.y]]
          # print(title)
          url <- footers[[.y]]
          text <- names(footers)[[.y]]
          column(
            width = 3, # 4 vis per row
            card(
              card_title(
                htmltools::h6(title, style = "text-align: center;")
              ),
              card_body(
                plotlyOutput(
                  session$ns(.x)
                ) %>% withSpinner(color="#017176"),
              ),
              card_footer(
                htmltools::div(
                  style = "text-align: center;",
                  htmltools::p('Source: ', style = "display: inline; margin-right: 5px;"),
                  htmltools::a(href = url, text, target = "_blank", style = "display: inline;")
                )
              ),
              full_screen = TRUE
            )
          )
        }
      )
    )

  )
}

#' @export
barChartServer <- function(plot_ids, data, gene_lists, plot_func, output, yaxis_option) {
  plots <- purrr::imap(
    plot_ids, ~ {
      data_point <- data[[.x]]
      # axis_title <- axis_titles[[.y]]
      output[[.x]] <- renderPlotly({
        # if (is.factor(data_point) || is.character(data_point)) {
        plot_func(gene_lists, data, .x, yaxis_option)
      })
    }
  )
  return(plots)
}

# test list
# gene_lists <- list(
#     'JAX' =
#       list(
#         'gene_list' = dpc_lists$JAX
#       ),
#     'MSK' =
#       list(
#         'gene_list' = dpc_lists$MSK
#       ),
#     'NWU' =
#       list(
#         'gene_list' = dpc_lists$NWU
#       ),
#     'UCSF' =
#       list(
#         'gene_list' = dpc_lists$UCSF
#       ),
#     'All MorPhiC genes' =
#       list(
#         'gene_list' = unique(c(dpc_lists$JAX, dpc_lists$MSK, dpc_lists$NWU, dpc_lists$UCSF))
#       )
#     )
# is.factor(plots_df$impc_viability)
# library(purrr)
# x <- barChart(gene_lists[[1]], plots_df, 'impc_viability', 'percentage of genes')
# x
# y <- barChart_old_test(gene_lists[[1]], plots_df, 'impc_viability', 'percentage of genes')
# y

# Bar chart with denominator of only annotations
#' @export
barChart <- function(gene_list, data_source, data_col, yaxis_option) {

  if (is.factor(data_source[[data_col]])) {

    combined_dataframe <- map_dfr(seq_along(gene_list), function(i) {
      list_name <- names(gene_list)[i]
      num <- length(setdiff(gene_list[[i]], data_source[["gene_symbol"]]))
      df <- data_source %>%
        dplyr::filter(gene_symbol %in% gene_list[[i]]) %>%
        dplyr::select(gene_symbol, !! sym(data_col)) %>%
        dplyr::distinct() %>%
        dplyr::filter(!is.na(!! sym(data_col))) %>%
        dplyr::count(!! sym(data_col)) %>%
        dplyr::mutate(percentage = round(n/sum(n)*100, 2)) %>%
        dplyr::mutate(name = list_name)
      # print(df)
    })

    if (yaxis_option == 'percentage of genes') {
      plot <- plot_ly(combined_dataframe, x = as.formula(paste("~", data_col)), y = ~percentage, color = ~name,
                      textposition = 'outside', text = ~percentage, type = "bar")%>%
        layout(
          xaxis = list(title = ""),
          # xaxis = list(title = custom_x_axis_title),
          yaxis = list(title = "Percentage of genes"),
          showlegend = TRUE
        )
    } else if (yaxis_option == 'number of genes') {
      plot <- plot_ly(combined_dataframe, x = as.formula(paste("~", data_col)), y = ~n, color = ~name,
                      textposition = 'outside', text = ~n, type = "bar")%>%
        layout(
          xaxis = list(title = ""),
          # xaxis = list(title = custom_x_axis_title),
          yaxis = list(title = "Number of genes"),
          showlegend = TRUE
        )
    }

  } else if (is.character(data_source[[data_col]])) {
    combined_dataframe <- map_dfr(seq_along(gene_list), function(i) {
      list_name <- names(gene_list)[i]
      num <- length(setdiff(gene_list[[i]], data_source[["gene_symbol"]]))
      df <- data_source %>%
        dplyr::filter(gene_symbol %in% gene_list[[i]]) %>%
        dplyr::select(gene_symbol, !! sym(data_col)) %>%
        dplyr::mutate(current_col = ifelse(is.na(!! sym(data_col)), "Genes without data", "Genes with data")) %>%
        dplyr::select(gene_symbol, current_col)  %>%
        dplyr::distinct() %>%
        dplyr::group_by(current_col) %>%
        dplyr::summarize(count = n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(percentage = round((count / sum(count)) * 100, 2)) %>%
        dplyr::mutate(name = list_name)

    })

    if (yaxis_option == 'percentage of genes') {
      plot <- plot_ly(combined_dataframe, x = ~current_col, y = ~percentage, color = ~name,
                      textposition = 'outside', text = ~percentage, type = "bar") %>%
        layout(
          xaxis = list(title = ""),
          # xaxis = list(title = custom_x_axis_title),
          yaxis = list(title = "Percentage of genes"),
          showlegend = TRUE
        )
    } else if (yaxis_option == 'number of genes') {
      plot <- plot_ly(combined_dataframe, x = ~current_col, y = ~count, color = ~name,
                      textposition = 'outside', text = ~count, type = "bar") %>%
        layout(
          xaxis = list(title = ""),
          # xaxis = list(title = custom_x_axis_title),
          yaxis = list(title = "Number of genes"),
          showlegend = TRUE
        )
    }
  }

  # plot <- plot %>%
  #   layout(
  #     xaxis = list(title = ""),
  #     # xaxis = list(title = custom_x_axis_title),
  #     yaxis = list(title = "Percentage of genes"),
  #     showlegend = TRUE
  #   )
}
# barChart_old_test <- function(gene_list, data_source, data_col, yaxis_option) {
#
#   if (is.factor(data_source[[data_col]])) {
#
#     combined_dataframe <- map_dfr(seq_along(gene_list), function(i) {
#       list_name <- names(gene_list)[i]
#       num <- length(setdiff(gene_list[[i]], data_source[["gene_symbol"]]))
#       df <- data_source %>%
#         dplyr::filter(gene_symbol %in% gene_list[[i]]) %>%
#         dplyr::select(gene_symbol, !! sym(data_col)) %>%
#         dplyr::distinct() %>%
#         dplyr::count(!! sym(data_col)) %>%
#         dplyr::mutate(percentage = round(n/sum(n)*100, 2)) %>%
#         dplyr::mutate(name = list_name)
#       print(df)
#     })
#
#     if (yaxis_option == 'percentage of genes') {
#       plot <- plot_ly(combined_dataframe, x = as.formula(paste("~", data_col)), y = ~percentage, color = ~name,
#                       textposition = 'outside', text = ~percentage, type = "bar")%>%
#         layout(
#           xaxis = list(title = ""),
#           # xaxis = list(title = custom_x_axis_title),
#           yaxis = list(title = "Percentage of genes"),
#           showlegend = TRUE
#         )
#     } else if (yaxis_option == 'number of genes') {
#       plot <- plot_ly(combined_dataframe, x = as.formula(paste("~", data_col)), y = ~n, color = ~name,
#                       textposition = 'outside', text = ~n, type = "bar")%>%
#         layout(
#           xaxis = list(title = ""),
#           # xaxis = list(title = custom_x_axis_title),
#           yaxis = list(title = "Number of genes"),
#           showlegend = TRUE
#         )
#     }
#
#   } else if (is.character(data_source[[data_col]])) {
#     combined_dataframe <- map_dfr(seq_along(gene_list), function(i) {
#       list_name <- names(gene_list)[i]
#       num <- length(setdiff(gene_list[[i]], data_source[["gene_symbol"]]))
#       df <- data_source %>%
#         dplyr::filter(gene_symbol %in% gene_list[[i]]) %>%
#         dplyr::select(gene_symbol, !! sym(data_col)) %>%
#         dplyr::mutate(current_col = ifelse(is.na(!! sym(data_col)), "NA", "nonNA")) %>%
#         dplyr::select(gene_symbol, current_col)  %>%
#         dplyr::distinct() %>%
#         dplyr::group_by(current_col) %>%
#         dplyr::summarize(count = n()) %>%
#         dplyr::ungroup() %>%
#         dplyr::mutate(percentage = round((count / sum(count)) * 100, 2)) %>%
#         dplyr::mutate(name = list_name)
#
#     })
#
#     if (yaxis_option == 'percentage of genes') {
#       plot <- plot_ly(combined_dataframe, x = ~current_col, y = ~percentage, color = ~name,
#                       textposition = 'outside', text = ~percentage, type = "bar") %>%
#         layout(
#           xaxis = list(title = ""),
#           # xaxis = list(title = custom_x_axis_title),
#           yaxis = list(title = "Percentage of genes"),
#           showlegend = TRUE
#         )
#     } else if (yaxis_option == 'number of genes') {
#       plot <- plot_ly(combined_dataframe, x = ~current_col, y = ~count, color = ~name,
#                       textposition = 'outside', text = ~count, type = "bar") %>%
#         layout(
#           xaxis = list(title = ""),
#           # xaxis = list(title = custom_x_axis_title),
#           yaxis = list(title = "Number of genes"),
#           showlegend = TRUE
#         )
#     }
#   }
#
#   # plot <- plot %>%
#   #   layout(
#   #     xaxis = list(title = ""),
#   #     # xaxis = list(title = custom_x_axis_title),
#   #     yaxis = list(title = "Percentage of genes"),
#   #     showlegend = TRUE
#   #   )
# }
# barChart <- function(gene_list, data_source, data_col, yaxis_option) {
#
#   if (is.factor(data_source[[data_col]])) {
#
#     combined_dataframe <- map_dfr(seq_along(gene_list), function(i) {
#       list_name <- names(gene_list)[i]
#       num <- length(setdiff(gene_list[[i]], data_source[["gene_symbol"]]))
#       df <- data_source %>%
#         dplyr::filter(gene_symbol %in% gene_list[[i]]) %>%
#         dplyr::select(gene_symbol, !! sym(data_col)) %>%
#         dplyr::distinct() %>%
#         dplyr::count(!! sym(data_col)) %>%
#         dplyr::mutate(percentage = round(n/sum(n)*100, 2)) %>%
#         dplyr::mutate(name = list_name)
#     })
#
#     if (yaxis_option == 'percentage of genes') {
#       plot <- plot_ly(combined_dataframe, x = as.formula(paste("~", data_col)), y = ~percentage, color = ~name,
#                       textposition = 'outside', text = ~percentage, type = "bar")%>%
#         layout(
#           xaxis = list(title = ""),
#           # xaxis = list(title = custom_x_axis_title),
#           yaxis = list(title = "Percentage of genes"),
#           showlegend = TRUE
#         )
#     } else if (yaxis_option == 'number of genes') {
#       plot <- plot_ly(combined_dataframe, x = as.formula(paste("~", data_col)), y = ~n, color = ~name,
#                       textposition = 'outside', text = ~n, type = "bar")%>%
#         layout(
#           xaxis = list(title = ""),
#           # xaxis = list(title = custom_x_axis_title),
#           yaxis = list(title = "Number of genes"),
#           showlegend = TRUE
#         )
#     }
#
#   } else if (is.character(data_source[[data_col]])) {
#     combined_dataframe <- map_dfr(seq_along(gene_list), function(i) {
#       list_name <- names(gene_list)[i]
#       num <- length(setdiff(gene_list[[i]], data_source[["gene_symbol"]]))
#       df <- data_source %>%
#         dplyr::filter(gene_symbol %in% gene_list[[i]]) %>%
#         dplyr::select(gene_symbol, !! sym(data_col)) %>%
#         dplyr::mutate(current_col = ifelse(is.na(!! sym(data_col)), "NA", "nonNA")) %>%
#         dplyr::select(gene_symbol, current_col)  %>%
#         dplyr::distinct() %>%
#         dplyr::group_by(current_col) %>%
#         dplyr::summarize(count = n()) %>%
#         dplyr::ungroup() %>%
#         dplyr::mutate(percentage = round((count / sum(count)) * 100, 2)) %>%
#         dplyr::mutate(name = list_name)
#
#     })
#
#     if (yaxis_option == 'percentage of genes') {
#       plot <- plot_ly(combined_dataframe, x = ~current_col, y = ~percentage, color = ~name,
#                       textposition = 'outside', text = ~percentage, type = "bar") %>%
#         layout(
#           xaxis = list(title = ""),
#           # xaxis = list(title = custom_x_axis_title),
#           yaxis = list(title = "Percentage of genes"),
#           showlegend = TRUE
#         )
#     } else if (yaxis_option == 'number of genes') {
#       plot <- plot_ly(combined_dataframe, x = ~current_col, y = ~count, color = ~name,
#                       textposition = 'outside', text = ~count, type = "bar") %>%
#         layout(
#           xaxis = list(title = ""),
#           # xaxis = list(title = custom_x_axis_title),
#           yaxis = list(title = "Number of genes"),
#           showlegend = TRUE
#         )
#     }
#   }
#
#   # plot <- plot %>%
#   #   layout(
#   #     xaxis = list(title = ""),
#   #     # xaxis = list(title = custom_x_axis_title),
#   #     yaxis = list(title = "Percentage of genes"),
#   #     showlegend = TRUE
#   #   )
# }

# Violin plots ----
#' @export
violinPlotUI <- function(plot_ids, select_args, footers, titles, id, session) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      class = "align-items-center",
      column(
        width = 3,
        selectInput(session$ns('gene_list_picker'), 'Select gene list',
                    choices = names(select_args),
                    selected = names(select_args),
                    multiple = TRUE)
      ),
      column(
        width = 4,
        textAreaInput(session$ns('highlight_genes'), 'Enter genes to highlight', placeholder = 'Separate entries with ;'),
      ),
      column(
        width = 3,
        checkboxInput(session$ns('toggle_points'), 'Show all data points', value = FALSE),
      ),
      column(
        width = 2,
        div(style = "text-align: right;", actionButton(session$ns("modal"), "Plots info"))
      )
    ),
    fluidRow(
      em('Note: Hover over a plot and click the expand button at the bottom right to display the plot full screen')
    ),
    fluidRow(
      imap(
        plot_ids, ~ {
          title <- titles[[.y]]
          # print(title)
          url <- footers[[.y]]
          text <- names(footers)[[.y]]
          column(
            width = 3, # 4 vis per row
            card(
              card_title(
                htmltools::h6(title, style = "text-align: center;")
              ),
              card_body(
                plotlyOutput(
                  session$ns(.x)
                ) %>% withSpinner(color="#017176"),
              ),
              card_footer(
                htmltools::div(
                  style = "text-align: center;",
                  htmltools::p('Source: ', style = "display: inline; margin-right: 5px;"),
                  htmltools::a(href = url, text, target = "_blank", style = "display: inline;")
                )
              ),
              full_screen = TRUE
            )
          )
        }
      )
    )
  )
}

#' @export
violinPlotServer <- function(plot_ids, data, gene_lists, plot_func, axis_titles, genes_to_highlight, threshold_values, toggle_option, output) {
  plots <- purrr::imap(
    plot_ids, ~ {
      data_point <- data[[.x]]
      current_threshold_value <- threshold_values[[.y]]
      output[[.x]] <- renderPlotly({
        if (is.numeric(data_point)) {
          # Extract the threshold value using the index, handle NULL or missing indices
          plot_data <- plot_func(gene_lists, data, .x, genes_to_highlight, current_threshold_value, toggle_option)
          plot <- plot_data[['violin_plot']]
          data <- plot_data[['data']]
          if (length(genes_to_highlight) > 0) {
            highlighted_data <- data[data$gene_symbol %in% genes_to_highlight, ]
            plot <- plot %>%
              add_trace(
                data = highlighted_data,
                x = ~gene_list_name,
                y = as.formula(paste("~", .x)),
                type = "scatter",
                mode = "markers",
                marker = list(color = "black", size = 10),
                name = "Highlighted Genes"
              )
          } else {
            plot
          }
        }
      })
      # %>%
      #   bindCache(
      #     c(
      #       genes_to_highlight,
      #       gene_lists,
      #       toggle_option
      #     )
      #   )
    }
  )
}

#' @export
violinPlot <- function(gene_lists, raw_data, column, genes_to_highlight, threshold_value, toggle_option) {
  data <- do.call(rbind, lapply(names(gene_lists), function(gene_list_name) {
    genes <- gene_lists[[gene_list_name]]
    metrics_data <- raw_data[raw_data$gene_symbol %in% genes, c('gene_symbol', column)] %>%
      distinct()
    metrics_data$gene_list_name <- gene_list_name
    return(metrics_data)
  }))


  if (toggle_option == TRUE) {
    points_setting <- "all"
  } else {
    points_setting <- "none"
  }

  # Your existing code for creating the violin plots
  violin_plot <- plot_ly(
    data,
    y = as.formula(paste("~", column)),
    x = ~gene_list_name,
    type = "violin",
    box = list(visible = T),
    points = points_setting,
    name = ~gene_list_name,
    text = ~paste("Gene: ", gene_symbol, "<br>", column, ": ", get(column)),
    hoverinfo = "text"  # Include gene symbol and metric value in hover text
  )

  # # Add highlight points for individual genes
  # if (length(genes_to_highlight > 0)) {
  #   violin_plot <- violin_plot %>%
  #     highlight("plotly_selecting") %>%
  #     # Add points for highlighting
  #     add_trace(
  #       data = data[data$gene_symbol %in% genes_to_highlight, ],  # Only include specified genes
  #       type = "scatter",
  #       mode = "markers",
  #       x = ~gene_list_name,
  #       y = as.formula(paste("~", column)),
  #       text = ~paste("Gene: ", gene_symbol, "<br>", column, ": ", get(column)),
  #       marker = list(color = "black", size = 10),
  #       hoverinfo = "text",
  #       name = "Searched Genes"  # Legend entry for the added trace
  #     )
  # }

  # Remove x-axis title
  violin_plot <- violin_plot %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Score"),
      showlegend = TRUE
    )

  hline <- function(y = 0, color = "grey") {
    list(
      type = "line",
      x0 = 0,
      x1 = 1,
      xref = "paper",
      y0 = y,
      y1 = y,
      line = list(dash = "dash", color = color)
    )
  }

  # Modified hline function to accept multiple threshold values
  hlines <- function(y = 0, color = "grey") {
    if (length(y) == 1) {
      return(list(hline(y, color)))
    } else {
      lines <- lapply(y, function(y_val) hline(y_val, color))
      return(lines)
    }
  }

  # Check if threshold_value is not NULL and is a vector
  if (!is.null(threshold_value) && is.vector(threshold_value)) {
    violin_plot <- violin_plot %>%
      layout(
        shapes = hlines(threshold_value)
      )
  }

  violin_plot %>% toWebGL()

  return(list('violin_plot' = violin_plot, 'data' = data))
}

#' @export
generateUpsetR <- function(gene_lists, set_size_max) {

  input <- list()

  for (i in names(gene_lists)) {
    input[[i]] <- gene_lists[[i]][['gene_list']]
  }

  # upsetjs() %>%
  #   fromList(input) %>%
  #   interactiveChart()
  upset(
    fromList(input), order.by = 'freq',
    matrix.color = "black",
    main.bar.color = "black",
    sets.bar.color = "black",
    shade.color = "gray88",
    set_size.show = TRUE,
    show.numbers = "yes",
    text.scale = c(2, 2, 2, 2, 2, 2),
    point.size = 4, 
    line.size = 1.5,
    set_size.scale_max = set_size_max
  )
}

#' #' @export
#' getPantherPlots <- function(data, gene_lists) {
#'
#'   names <- names(gene_lists)
#'
#'   all_data_list <- list()
#'
#'   for (i in names) {
#'     gene_list_name <- i
#'     print(data)
#'     all_data_list <- append(
#'       all_data_list,
#'       list(
#'         list(
#'           data %>%
#'             dplyr::filter(!is.na(CLASS_TERM)) %>%
#'             dplyr::filter(gene_symbol %in% gene_lists[[i]]) %>%
#'             dplyr::select(CLASS_TERM) %>%
#'             dplyr::group_by(CLASS_TERM) %>%
#'             dplyr::tally() %>%
#'             dplyr::arrange(desc(n)) %>%
#'             dplyr::slice_head(n = 10),
#'           gene_list_name
#'         )
#'       )
#'     )
#'   }
#'
#'   plot_list <- list()
#'   for (i in all_data_list) {
#'     p <- i[[1]] %>%
#'       plot_ly(
#'         type = 'bar',
#'         x = ~reorder(CLASS_TERM, n),
#'         y = ~n,
#'         name = i[[2]]
#'       )
#'
#'     plot_list <- append(plot_list, list(p))
#'   }
#'
#'   subplots <- subplot(plot_list) %>%
#'     layout(
#'       title = 'Top 10 Protein Class Terms'
#'     )
#'
#'   return(subplots)
#' }
#' @export
getTopTenPlot <- function(data, gene_lists, data_col, title) {

  names <- names(gene_lists)

  all_data_list <- list()

  for (i in names) {
    gene_list_name <- i
    all_data_list <- append(
      all_data_list,
      list(
        list(
          data %>%
            dplyr::filter(!is.na(!! sym(data_col))) %>%
            dplyr::filter(gene_symbol %in% gene_lists[[i]][['gene_list']]) %>%
            dplyr::select(!! sym(data_col), gene_symbol) %>%
            dplyr::distinct() %>%
            dplyr::group_by(!! sym(data_col)) %>%
            dplyr::tally() %>%
            dplyr::arrange(desc(n)) %>%
            dplyr::slice_head(n = 10) %>%
            mutate(!! sym(data_col) := as.character(!! sym(data_col))),
          gene_list_name
        )
      )
    )
  }

  print(all_data_list)

  plot_list <- list()
  for (i in all_data_list) {

    p <- i[[1]] %>%
      plot_ly(
        type = 'bar',
        # x = ~reorder(CLASS_TERM, n),
        x = as.formula(paste("~", data_col)),
        y = ~n,
        name = i[[2]]
      )

    plot_list <- append(plot_list, list(p))
  }

  subplots <- subplot(plot_list) %>%
    layout(
      title = title,
      yaxis = list(title = 'Number of genes')
    )

  return(subplots)
}

# #
# t <- data1 %>%
#   dplyr::filter(!is.na(tissue)) %>%
#   dplyr::filter(gene_symbol %in% gene_lists[[1]][['gene_list']]) %>%
#   dplyr::select(tissue, gene_symbol) %>%
#   dplyr::distinct() %>%
#   dplyr::group_by(tissue) %>%
#   dplyr::tally() %>%
#   dplyr::arrange(desc(n)) %>%
#   dplyr::slice_head(n = 10)
# t
# t <- t %>% mutate(tissue = as.character(tissue))
# p <- t  %>%
#   plot_ly(
#     type = 'bar',
#     # x = ~reorder(CLASS_TERM, n),
#     x = as.formula(paste("~", 'tissue')),
#     y = ~n,
#     name = 'whatever'
#   )
# p
# #
# data2 <- read.fst('./data/panther_protein_table.fst')
# getTopTenPlot(data2, gene_lists, 'SUBFAMILY_TERM', 'this is a title')
# # getTopTenPlot(gene_list_table, gene_lists, 'CLASS_TERM')
# # getTopTenPlot(gene_list_table, gene_lists2, 'CLASS_TERM')
# # getTopTenPlot(data1, gene_lists, 'tissue')
# #
# data1 <- read.fst('./data/hca_expression_table.fst')
#
# gene_lists <- list(
#   'l1' = gene_list_table$gene_symbol[1:100],
#   'l2' = gene_list_table$gene_symbol[1000:1100],
#   'l3' = gene_list_table$gene_symbol[10000:10100]
# )
# gene_lists <- list(
#     'JAX' =
#       list(
#         'gene_list' = dpc_lists$JAX
#       ),
#     'MSK' =
#       list(
#         'gene_list' = dpc_lists$MSK
#       ),
#     'NWU' =
#       list(
#         'gene_list' = dpc_lists$NWU
#       ),
#     'UCSF' =
#       list(
#         'gene_list' = dpc_lists$UCSF
#       ),
#     'All MorPhiC genes' =
#       list(
#         'gene_list' = unique(c(dpc_lists$JAX, dpc_lists$MSK, dpc_lists$NWU, dpc_lists$UCSF))
#       )
#     )
# gene_lists2 <- list(
#   'l1' = gene_list_table$gene_symbol
# )
# getPantherPlots(gene_list_table, gene_lists)
#
# getTopTenPlot(gene_list_table, gene_lists, 'CLASS_TERM')

# TRY USING BAR CHART PLOT BUS USE FACTOR CHANGED DF AND JUST GET TOP 10 instead OF ALL levels
# gene_lists <- list(
#     'my_genes' =
#       list(
#         'gene_list' = gene_list_table$gene_symbol[1:400]
#       ),
#     'my_genes2' =
#       list(
#         'gene_list' = gene_list_table$gene_symbol[14000:14400]
#       )
#   )
# getTopTenPlot(panther_df, gene_lists, 'CLASS_TERM')
