# Install packages
if (!require(shiny)) install.packages(c("shiny", "shinydashboard", "DT", "plotly", "UpSetR", "ggplot2", "BiocManager", "enrichplot"))

# Load Libraries
library(BiocManager)
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(UpSetR)
library(ggplot2)
library(rrvgo)
library(shinyjs)
library(enrichplot)

# Make modules available to app.R
source("./gene_table_mod.R")
source("./meta_data_info_mod.R")
source("./visualisations_mod.R")

# APP CODE...

# UI HEADER ----
header <- dashboardHeader(
  title = "MorPhiC"
  #,
  # tags$li(a(href = 'https://morphic.bio/',
  #           img(src = 'morphiclogo.png',
  #               title = "MorPhiC.bio", height = "30px"),
  #           style = "padding-top:10px; padding-bottom:10px;"),
  #         class = "dropdown")

  # tags$li(href='https://morphic.bio/',
  #             tags$img(src='morphiclogo.png',style = "margin-left: -20px;")
          )


## UI SIDEBAR ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("MorPhiC Genes", tabName = "genes", icon = icon("table")),
    menuItem("Visualisations", tabName = "visualisations", icon = icon("chart-bar")),
    menuItem("Metadata Info", tabName = "metadata_info", icon = icon("info"))
  )
          )

# UI BODY ----
body <- dashboardBody(

  fillPage = TRUE,

  tabItems(

    # GENE TABLE ----
    tabItem(
      tabName = "genes",
      geneListPageUI("genes")
    ),

    # METADATA INFORMATION TAB ----
    tabItem(
      tabName = "metadata_info",
      metadataInfoUI("metadata_info")
    ),

    # DATA VISUALISATIONS TAB ----
    tabItem(
      tabName = "visualisations",
      visualisationsPageUI("visualisations")
    )
  )
      )

# DEFINE UI ----
ui <- dashboardPage(header, sidebar, body)

# DEFINE SERVER ----
server <- function(input, output) {

  # GENE LIST ----
  geneListPageServer("genes")

  # VISUALISATIONS ----
  visualisationsPageServer("visualisations")

  # METADATA INFORMATION ----
  metadataInfoServer("metadata_info")
}

# DEFINE THE APP (UI + SERVER) ----
shinyApp(ui, server)
