box::use(
  shiny[moduleServer, NS, fluidPage, actionButton, textInput, textAreaInput,
        reactive, req, observeEvent, h6, p, sidebarLayout, sidebarPanel, mainPanel,
        observe, HTML, updateTextAreaInput, updateTextInput],
  htmltools[tags],
  DT[renderDT, DTOutput],
  dplyr[...],
  shinycssloaders[withSpinner],
  googlesheets4[sheet_append, read_sheet],
  shinyjs[useShinyjs, disable, enable, extendShinyjs, reset],
  bslib[card]
)



#' @export
ui <- function(id) {
  ns <- NS(id)
  card(
    useShinyjs(),
    h6('Gene submission interface'),
    p('This is a community submission interface to let us known which genes of interest you would like to be studied by the MorPhiC Project during Phase 1.'),
    sidebarLayout(
      sidebarPanel(
        textInput(ns('name'), 'Name', NULL, placeholder = 'Your name'),
        textInput(ns('email'), 'Email', NULL, placeholder = 'youremail@email.com'),
        textInput(ns('institution'), 'Institution', NULL, placeholder = 'Your institution'),
        textInput(ns('gene'), 'Gene symbol/ID', NULL, placeholder = 'A HGNC valid Gene Symbol or ID'),
        textAreaInput(ns('comment'), 'Comment', NULL, placeholder = 'Additional comments'),
        actionButton(ns('gs_btn'), 'Submit response')
      ),
      mainPanel(
        h6('Community submissions'),
        DTOutput(ns('gs_tbl')) %>% withSpinner(color="#017176"),
        actionButton(ns('refresh'), 'Refresh table')
      )
    )
  )
}

#' @export
server <- function(id, google_sheet) {
  moduleServer(id, function(input, output, session) {

    df <- reactive({
      data.frame(
        'Name' = input$name,
        'Email' = input$email,
        'Institute' = input$institution,
        'Gene' = input$gene,
        'Reason' = input$comment
      )
    })

    observe({
      # Enable the submit button only if all inputs are filled
      if (all(sapply(list(input$name, input$email, input$institution, input$gene, input$comment), nchar) > 0)) {
        shinyjs::enable('gs_btn')
      } else {
        shinyjs::disable('gs_btn')
      }
    })

    observeEvent(input$gs_btn, {
      sheet_append("1tJRqNmyFf76K6DFzpZlYY_4NvRwvyVxaszmaAG0Klv8", df(), sheet = "Sheet1")
      shinyjs::reset('name')
      shinyjs::reset('email')
      shinyjs::reset('institution')
      shinyjs::reset('gene')
      shinyjs::reset('comment')
    })

    output$gs_tbl <- renderDT({
      input$gs_btn
      input$refresh
      google_sheet <- read_sheet("1tJRqNmyFf76K6DFzpZlYY_4NvRwvyVxaszmaAG0Klv8")
    })

  })
}
