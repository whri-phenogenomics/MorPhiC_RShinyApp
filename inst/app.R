library(shiny)
library(DT)
library(plotly)
library(tools)
library(stringr)
library(dplyr)
library(bslib)
library(bsicons)
library(glue)
library(readxl)

# Auxiliary
source("./auxiliary_scripts/aux_app_funcs.R")

# Modules
source("./genesMetaData_mod5.R")
source("./geneSearch_mod2.R")
source("./visualisations_mod4.R")
source("./annotationsInfo_mod.R")
source("./metadataInfo_mod.R")


conflicted::conflicts_prefer(base::setdiff)
conflicted::conflicts_prefer(dplyr::filter)

#genesMetaDataDf_data <<- readRDS("../annotated_all_protein_coding_genes_191023.rda")
# genesMetaDataDf_data <<- readRDS("../rda_data/271023_all_protein_coding_genes.rda")
genesMetaDataDf_data <<- readRDS('./extdata/processed/rda_data/all_genes_with_anot_dec.rda')
morphic_pipeline <<- readRDS("./extdata/processed/rda_data/all_dpcs_gene_list.rda")
list_of_dpcs <<- readRDS("./extdata/processed/rda_data/zipped_dpc_names_genes.rda")
all.protein.coding.genes <<- readRDS("./extdata/processed/rda_data/all_protein_coding_genes.rda")
main.annotated.data.frame <<- readRDS("./extdata/processed/rda_data/main.annotated.data.frame.rda") # Redundant if switch use in plots for genesMetaDataDf_data
gene.constraint.metrics.num.only <<- readRDS("./extdata/processed/rda_data/gene.constraint.metrics.num.only.rda")
zipped_dt_cols <<- readRDS("./extdata/processed/rda_data/zipped_dt_cols2.rda")
data_info_tables2 <<- readRDS("./extdata/processed/rda_data/data_info_tables2.rda")

ui <- page_navbar(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  title = "MorPhiC",
  bg = "#0062cc",
  nav_panel(title = "Gene List Browser", genesMetaDataTableUI("tables")),
  nav_panel(title = "Visualisations", visualisationsUI("plots")),
  nav_panel(
    title = "Metadata Information",
    div(
      class = "row",
      annotationsInfoUI("info")
    ),
    div(
      class = "row",
      metadataInfoUI("metadata")
    )
  ),
  #nav_panel(title = "test", metadataInfoUI("metadata")),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a(href = 'https://morphic.bio/', "MorPhiC Home Page"))
  )
)


#---- server
server <- function(input, output, session) {
  # using toupper means a few genes may be lost as they are lower case - look into this
  geneSearchServer("genes")
  genesMetaDataTableServer("tables")
  visualisationsServer("plots")
  annotationsInfoServer("info")
  metadataInfoServer("metadata")
}

shinyApp(ui, server)




