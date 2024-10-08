# Dependencies ----
box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags,
        uiOutput, navbarPage, tagList, tabPanel, p, reactiveVal, HTML],
  bslib[navset_card_underline, nav_panel, bs_theme, card, page_navbar,
        nav_spacer, nav_menu, nav_item],
  fst[read.fst],
  googlesheets4[...],
)

# Import modules ----
box::use(
  app/view/gene_list,
  app/view/bar_chart_mod,
  app/view/chart_mod,
  app/view/go_mod,
  app/view/pathway_mod,
  app/view/violin_plot_mod,
  app/view/google_sheet_mod,
  app/view/odds_ratio_mod,
  app/view/page_bottom,
  app/view/upset_plot,
  app/logic/user_information_utils
)

gs4_auth(cache = ".secrets", email = "gabrielmarengo@gmail.com")
google_sheet <- read_sheet("1tJRqNmyFf76K6DFzpZlYY_4NvRwvyVxaszmaAG0Klv8")

#' @export
ui <- function(id) {
  ns <- NS(id)
  # Theme ----
  # theme <- bs_theme(
  #   bg = "#ffffff", # Background color
  #   fg = "#000000", # Foreground color
  #   primary = "#FFA500", # Primary color (Orange accent)
  #   secondary = "#008080", # Secondary color (Teal accent)
  #   base_font = "Arial, sans-serif", # Base font
  #   heading_font = "Georgia, serif", # Heading font
  #   font_scale = 1.1 # Font scale
  # )
  theme <- bs_theme(version = 5, bootswatch = "lux")

  page_navbar(
    tags$head(
      tags$style(HTML("
 body, html {
      height: 100%;
      margin: 0;
      display: flex;
      flex-direction: column;
    }
    .content {
      flex: 1 0 auto;
    }
    .footer {
      flex-shrink: 0;
      background: linear-gradient(to right, #89ced9, #ec947c);
      padding: 5px;
      height: 15px; /* Adjusted height to account for padding */
      box-sizing: border-box;
      text-align: center;
      color: white;
      display: flex;
      align-items: center;
      justify-content: center;
    }
  "))
    ),
    # Navbar img ----
    title = tagList(
      tags$img(src = "static/title_img.png", height = "30px", style = "margin-right: 0px;")
    ),
    bg = 'black',
    theme = theme,
    footer = tags$footer(
      class = "footer",
      "Developed by Queen Mary University of London as part of the DRACC: Updated 11th September 2024"
    ),
    # Gene list ----
    nav_panel(
      'Gene list',
      # card(
      #   height = '750px',
      #   gene_list$ui(ns('gene_list'))
      # )

      # height = '750px',
      navset_card_underline(
        nav_panel(
          'Table',
          gene_list$ui(ns('gene_list'))
        ),
        nav_panel(
          'Upset Plot',
          upset_plot$ui(ns('upset'))
        )
      )

      # page_bottom$ui(ns('gene_list_bottom'))
      ),
    # Charts ----
    nav_panel(
      'Charts',
      navset_card_underline(
        nav_panel(
          'Mouse knockouts',
          bar_chart_mod$ui(ns('impc_bar_charts'))
        ),
        nav_panel(
          'Diseases',
          bar_chart_mod$ui(ns('disease_bar_charts'))
        ),
        nav_panel(
          'Gene constraint metrics (cell lines)',
          violin_plot_mod$ui(ns('constraint_plots_cell'))
        ),
        nav_panel(
          'Gene constraint metrics (population sequencing)',
          violin_plot_mod$ui(ns('constraint_plots_pop_seq'))
        ),
        nav_panel(
          'Protein families',
          chart_mod$ui(ns('protein_families'))
        ),
        nav_panel(
          'Tissue expression',
          chart_mod$ui(ns('hca_expression'))
        )
      )
    ),
    # Enrichment analysis
    nav_panel(
      'Enrichment analysis',
      navset_card_underline(
        nav_panel(
          'Gene Ontology',
          go_mod$ui(ns('go_plots'))
        ),
        nav_panel(
          'Reactome pathways',
          pathway_mod$ui(ns('pathway_plots'))
        ),
        nav_panel(
          'Odds ratio analysis',
          odds_ratio_mod$ui(ns('odds_ratio_plots'))
        )
      )
    ),
    # Gene suggestion
    nav_panel(
      'Suggest gene',
      google_sheet_mod$ui(ns("google_sheet"))
    ),
    nav_spacer(),
    # Links
    nav_menu(
      title = "Other tools",
      align = "right",
      nav_item(tags$a(href = 'https://whri-phenogenomics.shinyapps.io/essential_genes_explorer/', "Essential Genes Explorer (EGE)"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    dpc_lists <- readRDS('./data/morphic_gene_lists2024-09-11.rda')
    # gene_list_table <- read.fst('./data/morphic_gene_list_table_2024-07-25.fst')
    gene_list_table <- read.fst('./data/morphic_gene_list_table_2024-09-11.fst')
    plots_df <- read.fst('./data/all_data_ege_2024-09-11.fst')
    panther_df <- read.fst('./data/panther_protein_table.fst')
    hca_expression_df <- read.fst('./data/hca_expression_table.fst')
    go_plots <- readRDS('./data/go_plots.rda')
    pathway_plots <- readRDS('./data/pathway_plots.rda')
    odds_ratio_plots <- readRDS('./data/odds_ratio_plots.rda')
    gene_lists <- reactiveVal(
      list(
        'JAX' =
          list(
            'gene_list' = dpc_lists$JAX
          ),
        'MSK' =
          list(
            'gene_list' = dpc_lists$MSK
          ),
        'NWU' =
          list(
            'gene_list' = dpc_lists$NWU
          ),
        'UCSF' =
          list(
            'gene_list' = dpc_lists$UCSF
          ),
        'All MorPhiC genes' =
          list(
            'gene_list' = unique(c(dpc_lists$JAX, dpc_lists$MSK, dpc_lists$NWU, dpc_lists$UCSF))
          ),
        'All protein coding genes' =
          list(
            'gene_list' = gene_list_table$gene_symbol
          )
      )
    )
    # index <- list(
    #   'gene_ids' = 1:4,
    #   'mouse' = c(5:8,10),
    #   'disease' = 11:21,
    #   'constraint_cell' = 22:25,
    #   'constraint_pop' = 26:35
    # )
    index <- list(
      'gene_ids' = 1:4,
      'mouse' = 5:9,
      'disease' = 10:20,
      'constraint_cell' = 21:24,
      'constraint_pop' = 25:34
    )
    nice_col_names <- user_information_utils$getNiceColNames()
    footers_info <- user_information_utils$getFooters()
    plots_info <- user_information_utils$getPlotsInfo()
    gene_list$server('gene_list', gene_list_table, gene_lists)
    bar_chart_mod$server(
      'impc_bar_charts',
      gene_lists,
      plots_df,
      plot_ids = names(plots_df)[index$mouse],
      footers = footers_info[[1]],
      titles = nice_col_names[index$mouse],
      plots_info =  plots_info[[1]]
    )
    bar_chart_mod$server(
      'disease_bar_charts',
      gene_lists,
      plots_df,
      plot_ids = names(plots_df)[index$disease],
      footers = footers_info[[2]],
      titles = nice_col_names[index$disease],
      plots_info = plots_info[[2]]
    )
    violin_plot_mod$server(
      'constraint_plots_cell',
      gene_lists,
      plots_df,
      plot_ids = names(plots_df)[index$constraint_cell],
      threshold_values = list(NULL, c(-0.5, -1), 5, 5),
      footers = footers_info[[3]],
      titles = nice_col_names[index$constraint_cell],
      plots_info = plots_info[[3]]
    )
    violin_plot_mod$server(
      'constraint_plots_pop_seq',
      gene_lists,
      plots_df,
      plot_ids = names(plots_df)[index$constraint_pop],
      threshold_values = list(0.6, NULL, 0.9,
                              # 0.075,
                              NULL, NULL, c(0.25, 0.75), NULL, NULL, NULL, NULL),
      footers = footers_info[[4]],
      titles = nice_col_names[index$constraint_pop],
      plots_info = plots_info[[4]]
    )
    chart_mod$server(
      'protein_families',
      gene_lists = gene_lists,
      data = panther_df,
      data_col_options = c('CLASS_TERM', 'SUBFAMILY_TERM'),
      title = 'Top 10 Protein classes'
    )
    chart_mod$server(
      'hca_expression',
      gene_lists = gene_lists,
      data = hca_expression_df,
      data_col_options = c('tissue', 'cell_type'),
      title = 'Top 10 Expressed tissues'
    )
    go_mod$server('go_plots', go_plots)
    pathway_mod$server('pathway_plots', pathway_plots)
    odds_ratio_mod$server('odds_ratio_plots', odds_ratio_plots)
    google_sheet_mod$server("google_sheet", google_sheet)
    upset_plot$server('upset', gene_lists)
    # page_bottom$server('gene_list_bottom')
    # Y$server('protein_interactions')
  })
}
