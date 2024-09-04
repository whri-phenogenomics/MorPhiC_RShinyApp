# This file allows packrat (used by rsconnect during deployment) to pick up dependencies.
library(rhino)
library(purrr)
library(datasets)
library(fst)
library(htmltools)
library(dplyr)
library(tools)
library(readxl)
library(stats)
library(shinyjs)
library(plotly)
library(clusterProfiler)
library(rrvgo)
library(epitools)
library(callr)
library(rsconnect)
library(ReactomePA)
library(org.Hs.eg.db)
library(bslib)
library(crew)
library(gridExtra)
library(epitools)
library(enrichplot)
library(shinycssloaders)
library(DT)
library(upsetjs)
library(UpSetR)
library(googlesheets4)
library(shinyalert)
library(ggtree)
library(BiocGenerics)
library(utils)
library(rstudioapi)
library(fst)

# # Install dependencies
# bioc_pkgs <- c('clusterProfiler', 'rrvgo', 'ReactomePA', 'org.Hs.eg.db',
#                'enrichplot')
#
# # List of CRAN packages
# cran_pkgs <- c('rhino', 'purrr', 'datasets', 'fst', 'htmltools', 'dplyr',
#                'tools', 'readxl', 'stats', 'shinyjs', 'plotly', 'epitools',
#                'callr', 'rsconnect', 'bslib', 'crew', 'gridExtra',
#                'shinycssloaders', 'DT', 'upsetjs', 'googlesheets4',
#                'shinyalert', 'ggplot2')
#
# BiocManager::install(bioc_pkgs, ask = FALSE)
# install.packages(cran_pkgs, ask = FALSE)
#
# lapply(bioc_pkgs, function(x) library(x, character.only=TRUE))
# lapply(cran_pkgs, function(x) library(x, character.only=TRUE))
