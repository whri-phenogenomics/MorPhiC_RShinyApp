library(bslib)

annotationsInfoUI <- function(id) {
  
  vbs <- list(
    value_box(
      title = "Human protein coding genes",
      value = "19383",
      showcase = bs_icon("clipboard-data-fill"),
      theme = "purple"
    ),
    value_box(
      title = "Mouse 1-1 orthologs",
      value = "16701",
      showcase = bs_icon("pie-chart"),
      theme = "teal"
    ),
    value_box(
      title = "Mouse model viability data",
      value = "7073",
      showcase = bs_icon("clipboard-data-fill"),
      theme = "pink",
      p("IMPC")
    ),
    value_box(
      title = "Mouse model viability data",
      value = "12428",
      showcase = bs_icon("pie-chart"),
      theme = "pink",
      p("MGI")
    ),
    value_box(
      title = "Associated with mendelian disease (where molecular basis is known)",
      value = "3968",
      showcase = bs_icon("clipboard-data-fill"),
      theme = "pink"
    ),
    value_box(
      title = "Gene constraint metrics derived from sequencing",
      value = "",
      showcase = bs_icon("pie-chart"),
      theme = "pink",
      p("gnomAD o/e LoF: 17249"),
      p("gnomAD o/e Missense: 17393"),
      p("Shet RGC-ME: 16438"),
      p("Shet Posterior: 16065"),
      p("DOMINO: 17695"),
      p("SCoNeS: 18603"),
      p("Alpha Missense pathogenicity: 18252")
    ),
    value_box(
      title = "Gene constraint metrics derived from cell lines",
      value = "",
      showcase = bs_icon("clipboard-data-fill"),
      theme = "pink",
      p("MEF hPSCs (Bayes Factor): 16760"),
      p("Laminin hPSCs (Bayes Factor): 16884"),
      p("DepMap (Mean gene effect score): 17421")
    ),
    value_box(
      title = "Gene Ontology terms",
      value = "",
      showcase = bs_icon("pie-chart"),
      theme = "pink",
      p("Biological process: 16887"),
      p("Molecular function: 17573"),
      p("Cellular component: 17751")
    ),
    value_box(
      title = "Number of genes with Reactome pathways",
      value = "10889",
      showcase = bs_icon("clipboard-data-fill"),
    )
  )
  
  tagList(
    h4("Values represent the number of genes for which a data source has annotations"),
    hr(),
    layout_column_wrap(
      width = "25%",
      height = "900px",
      !!!vbs
    )
  )
  
}

annotationsInfoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    
  })}