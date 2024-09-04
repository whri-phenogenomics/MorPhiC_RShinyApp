## deploy_app.R

# Dependencies
library(rsconnect)
library(renv)

# Renv snapshot
renv::snapshot()

# Deploy app
rsconnect::deployApp(appName = 'morphic_gene_list')
