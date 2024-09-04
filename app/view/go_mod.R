box::use(
  shiny[moduleServer, NS, fluidRow, column, uiOutput, renderUI, em,
        checkboxInput],
  plotly[plotlyOutput, renderPlotly],
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
    em('Bubble size corresponds to level of enrichment, with the top few enriched terms labelled in black.'),
    checkboxInput(ns('checkbox'), 'Show/hide legend', FALSE),
    uiOutput(ns('plots'))
  )

}

#' @export
server <- function(id, rda_obj) {
  moduleServer(id, function(input, output, session) {
    output$plots <- renderUI({
      fluidRow(
        #jax
        column(
          width = 6,
          navset_card_tab(
            title='GO Biological Process - JAX',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotlyOutput(session$ns('bp_jax')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('bp_jax_tbl'))
            )
          )
        ),
        column(
          width = 6,
          navset_card_tab(
            title='GO Molecular Function - JAX',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotlyOutput(session$ns('mf_jax')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('mf_jax_tbl'))
            )
          )
        ),
        #msk
        column(
          width = 6,
          navset_card_tab(
            title='GO Biological Process - MSK',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotlyOutput(session$ns('bp_msk')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('bp_msk_tbl'))
            )
          )
        ),
        column(
          width = 6,
          navset_card_tab(
            title='GO Molecular Function - MSK',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotlyOutput(session$ns('mf_msk')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('mf_msk_tbl'))
            )
          )
        ),
        #nwu
        column(
          width = 6,
          navset_card_tab(
            title='GO Biological Process - NWU',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotlyOutput(session$ns('bp_nwu')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('bp_nwu_tbl'))
            )
          )
        ),
        column(
          width = 6,
          navset_card_tab(
            title='GO Molecular Function - NWU',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotlyOutput(session$ns('mf_nwu')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('mf_nwu_tbl'))
            )
          )
        ),
        #ucsf
        column(
          width = 6,
          navset_card_tab(
            title='GO Biological Process - UCSF',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotlyOutput(session$ns('bp_ucsf')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('bp_ucsf_tbl'))
            )
          )
        ),
        column(
          width = 6,
          navset_card_tab(
            title= 'GO Molecular Function - UCSF',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotlyOutput(session$ns('mf_ucsf')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('mf_ucsf_tbl'))
            )
          )
        ),
        #all
        column(
          width = 6,
          navset_card_tab(
            title= 'GO Biological Process - All MorPhiC Genes',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotlyOutput(session$ns('bp_all')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('bp_all_tbl'))
            )
          )
        ),
        column(
          width = 6,
          navset_card_tab(
            title= 'GO Molecular Function - All MorPhiC Genes',
            full_screen = TRUE,
            nav_panel(
              card_title('Plot'),
              plotlyOutput(session$ns('mf_all')) %>% withSpinner(color="#017176")
            ),
            nav_panel(
              card_title('Table'),
              DTOutput(session$ns('mf_all_tbl'))
            )
          )
        )
      )

    })

    output$bp_jax <- renderPlotly({
      if (input$checkbox == FALSE) {
        plot <- rda_obj[['BP']][['JAX']][[1]] + theme(legend.position = "none")
      } else {
        plot <- rda_obj[['BP']][['JAX']][[1]] + theme(legend.position = "bottom")
      }
    })

    output$mf_jax <- renderPlotly({
      if (input$checkbox == FALSE) {
        plot <- rda_obj[['MF']][['JAX']][[1]] + theme(legend.position = "none")
      } else {
        plot <- rda_obj[['MF']][['JAX']][[1]] + theme(legend.position = "bottom")
      }
    })

    output$bp_msk <- renderPlotly({

      if (input$checkbox == FALSE) {
        plot <- rda_obj[['BP']][['MSK']][[1]] + theme(legend.position = "none")
      } else {
        plot <- rda_obj[['BP']][['MSK']][[1]] + theme(legend.position = "bottom")
      }
    })

    output$mf_msk <- renderPlotly({

      if (input$checkbox == FALSE) {
        plot <- rda_obj[['MF']][['MSK']][[1]] + theme(legend.position = "none")
      } else {
        plot <- rda_obj[['MF']][['MSK']][[1]] + theme(legend.position = "bottom")
      }
    })

    output$bp_nwu <- renderPlotly({

      if (input$checkbox == FALSE) {
        plot <- rda_obj[['BP']][['NWU']][[1]] + theme(legend.position = "none")
      } else {
        plot <- rda_obj[['BP']][['NWU']][[1]] + theme(legend.position = "bottom")
      }
    })

    output$mf_nwu <- renderPlotly({

      if (input$checkbox == FALSE) {
        plot <-  rda_obj[['MF']][['NWU']][[1]] + theme(legend.position = "none")
      } else {
        plot <-  rda_obj[['MF']][['NWU']][[1]] + theme(legend.position = "bottom")
      }
    })

    output$bp_ucsf <- renderPlotly({

      if (input$checkbox == FALSE) {
        plot <-  rda_obj[['BP']][['UCSF']][[1]] + theme(legend.position = "none")
      } else {
        plot <-  rda_obj[['BP']][['UCSF']][[1]] + theme(legend.position = "bottom")
      }
    })

    output$mf_ucsf <- renderPlotly({

      if (input$checkbox == FALSE) {
        plot <-  rda_obj[['MF']][['UCSF']][[1]] + theme(legend.position = "none")
      } else {
        plot <-  rda_obj[['MF']][['UCSF']][[1]] + theme(legend.position = "bottom")
      }
    })

    output$bp_all <- renderPlotly({

      if (input$checkbox == FALSE) {
        plot <- rda_obj[['BP']][['All MorPhiC genes']][[1]] + theme(legend.position = "none")
      } else {
        plot <- rda_obj[['BP']][['All MorPhiC genes']][[1]] + theme(legend.position = "bottom")
      }
    })

    output$mf_all <- renderPlotly({

      if (input$checkbox == FALSE) {
        plot <- rda_obj[['MF']][['All MorPhiC genes']][[1]] + theme(legend.position = "none")
      } else {
        plot <- rda_obj[['MF']][['All MorPhiC genes']][[1]] + theme(legend.position = "bottom")
      }
    })

    output$bp_jax_tbl <- renderDT({
      datatable(rda_obj[['BP']][['JAX']][[2]]@result, options = list(pageLength = 5))
    })

    output$mf_jax_tbl <- renderDT({
      datatable(rda_obj[['MF']][['JAX']][[2]]@result, options = list(pageLength = 5))
    })

    output$bp_msk_tbl <- renderDT({
      datatable(rda_obj[['BP']][['MSK']][[2]]@result, options = list(pageLength = 5))
    })

    output$mf_msk_tbl <- renderDT({
      datatable(rda_obj[['MF']][['MSK']][[2]]@result, options = list(pageLength = 5))
    })

    output$bp_nwu_tbl <- renderDT({
      datatable(rda_obj[['BP']][['NWU']][[2]]@result, options = list(pageLength = 5))
    })

    output$mf_nwu_tbl <- renderDT({
      datatable(rda_obj[['MF']][['NWU']][[2]]@result, options = list(pageLength = 5))
    })

    output$bp_ucsf_tbl <- renderDT({
      datatable(rda_obj[['BP']][['UCSF']][[2]]@result, options = list(pageLength = 5))
    })

    output$mf_ucsf_tbl <- renderDT({
      datatable(rda_obj[['MF']][['UCSF']][[2]]@result, options = list(pageLength = 5))
    })

    output$bp_all_tbl <- renderDT({
      datatable(rda_obj[['BP']][['All MorPhiC genes']][[2]]@result, options = list(pageLength = 5))
    })

    output$mf_all_tbl <- renderDT({
      datatable(rda_obj[['MF']][['All MorPhiC genes']][[2]]@result, options = list(pageLength = 5))
    })

  })
}
