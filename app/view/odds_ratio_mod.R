box::use(
  shiny[moduleServer, NS, fluidRow, column, uiOutput, renderUI, em,
        plotOutput, renderPlot],
  # plotly[plotlyOutput, renderPlotly],
  bslib[navset_card_tab, card_title, nav_panel],
  shinycssloaders[withSpinner],
  dplyr[...],
  ggplot2[theme],
  DT[DTOutput, renderDT, datatable, JS]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    # em('Bubble size corresponds to level of enrichment, with the top few enriched terms labelled in black'),
    # checkboxInput(ns('checkbox'), 'Show/hide legend', TRUE),
    em('Odds Ratio analysis to assess if each gene list is enriched for Disease genes, genes lethal in IMPC Mouse Knockouts and genes essential in DepMap screenings.'),
    em('Threshold (red dotted line): x=1'),
    uiOutput(ns('plots'))
  )

}

#' @export
server <- function(id, rda_obj) {
  moduleServer(id, function(input, output, session) {
    output$plots <- renderUI({
      fluidRow(
        # OMIM
        column(
          width = 6,
          navset_card_tab(
            title='Odds ratio - OMIM Disease Genes',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotOutput(session$ns('omim')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('omim_tbl'))
            )
          )
        ),
        # IMPC
        column(
          width = 6,
          navset_card_tab(
            title='Odds ratio - IMPC Lethal Genes',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotOutput(session$ns('impc')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('impc_tbl'))
            )
          )
        ),
        # DepMap
        column(
          width = 6,
          navset_card_tab(
            title='Odds ratio - DepMap Essential Genes',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotOutput(session$ns('depmap')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('depmap_tbl'))
            )
          )
        )
      )
    })

    output$omim <- renderPlot({
      rda_obj[['odds_ratio_disease']][[1]]
    })

    output$impc <- renderPlot({
      rda_obj[['odds_ratio_impc']][[1]]
    })

    output$depmap <- renderPlot({
      rda_obj[['odds_ratio_depmap']][[1]]
    })

    output$omim_tbl <- renderDT({
      datatable(rda_obj[['odds_ratio_disease']][[2]], options = list(pageLength = 5))
    })

    output$impc_tbl <- renderDT({
      datatable(rda_obj[['odds_ratio_impc']][[2]], options = list(pageLength = 5))
    })

    output$depmap_tbl <- renderDT({
      datatable(rda_obj[['odds_ratio_depmap']][[2]], options = list(pageLength = 5))
    })
  })
}
