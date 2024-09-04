box::use(
  shiny[moduleServer, NS, fluidRow, column, uiOutput, renderUI, em,
        plotOutput, renderPlot],
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
    em('Plots show the top enriched Reactome pathways among each gene list.'),
    uiOutput(ns('plots'))
  )
}

#' @export
server <- function(id, rda_obj) {
  moduleServer(id, function(input, output, session) {
    output$plots <- renderUI({
      fluidRow(
        # JAX
        column(
          width = 6,
          navset_card_tab(
            title='Top 10 Enriched Reactome Pathways - JAX',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotOutput(session$ns('jax')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('jax_tbl'))
            )
          )
        ),
        # MSK
        column(
          width = 6,
          navset_card_tab(
            title='Top 10 Enriched Reactome Pathways - MSK',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotOutput(session$ns('msk')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('msk_tbl'))
            )
          )
        ),
        # NWU
        column(
          width = 6,
          navset_card_tab(
            title='Top 10 Enriched Reactome Pathways - NWU',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotOutput(session$ns('nwu')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('nwu_tbl'))
            )
          )
        ),
        # UCSF
        column(
          width = 6,
          navset_card_tab(
            title='Top 10 Enriched Reactome Pathways - UCSF',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotOutput(session$ns('ucsf')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('ucsf_tbl'))
            )
          )
        ),
        # All
        column(
          width = 6,
          navset_card_tab(
            title='Top 10 Enriched Reactome Pathways - All MorPhiC Genes',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotOutput(session$ns('all')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('all_tbl'))
            )
          )
        )
      )
    })

    output$jax <- renderPlot({
      rda_obj[['JAX']][[1]]
    })

    output$msk <- renderPlot({
      rda_obj[['MSK']][[1]]
    })

    output$nwu <- renderPlot({
      rda_obj[['NWU']][[1]]
    })

    output$ucsf <- renderPlot({
      rda_obj[['UCSF']][[1]]
    })

    output$all <- renderPlot({
      rda_obj[['All MorPhiC genes']][[1]]
    })

    output$jax_tbl <- renderDT({
      datatable(rda_obj[['JAX']][[2]], options = list(pageLength = 5))
    })

    output$msk_tbl <- renderDT({
      datatable(rda_obj[['MSK']][[2]], options = list(pageLength = 5))
    })

    output$nwu_tbl <- renderDT({
      datatable(rda_obj[['NWU']][[2]], options = list(pageLength = 5))
    })

    output$ucsf_tbl <- renderDT({
      datatable(rda_obj[['UCSF']][[2]], options = list(pageLength = 5))
    })

    output$all_tbl <- renderDT({
      datatable(rda_obj[['All MorPhiC genes']][[2]], options = list(pageLength = 5))
    })
  })
}
