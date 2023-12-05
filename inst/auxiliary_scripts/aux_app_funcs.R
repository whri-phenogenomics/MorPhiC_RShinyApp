library(stringr)

# Convert text area input to gene list c() format
parseTextAreaInput <- function(text){
  sep <- NULL
  if(grepl(";",text)) sep <- ";"
  if(grepl(",",text)) sep <- ","
  if(grepl("\n",text)) sep <- "\n"
  if (is.null(sep)) {
    # No separator found, return text as is (assuming each line is a gene)
    genes <- unlist(strsplit(toupper(text), "\n"))
  } else {
    # Use the identified separator to split the text
    genes <- unlist(str_split(toupper(text), sep))
  }
  # Remove any leading or trailing whitespace - NEED TO TEST THIS
  genes_list <- trimws(genes)
  return (genes_list)
}
# e.g. genes_list_test <- parseTextAreaInput(genes_text)

# reg ex for gene ids; symbol, hgnc, mgi, entrez
subsetGenesMetaDataDf_rowsOnly <- function(genes_list) {
  gene <- genes_list[1]
  if (grepl("^HGNC:\\d+$", gene)) {
    data_subset <- genesMetaDataDf_data[genesMetaDataDf_data$hgnc_id %in% genes_list, ]
  } else {
    # Check if the search term is an Entrez ID (e.g., ENSG12345)
    if (grepl("^\\d+$", gene)) {
      data_subset <- genesMetaDataDf_data[genesMetaDataDf_data$entrez_id %in% genes_list, ]
    } else {
      # Check if the search term is an MGI ID (e.g., MGI:12345)
      if (grepl("^MGI:\\d+$", gene)) {
        data_subset <- genesMetaDataDf_data[genesMetaDataDf_data$mgi_id %in% genes_list, ]
      } else {
        # Check if the search term is a gene symbol (e.g., Symbol_ABC)
        data_subset <- genesMetaDataDf_data[genesMetaDataDf_data$gene_symbol %in% genes_list, ]
      }
    }
  }
  return(data_subset)
}

subsetGenesMetaDataDf_rowsAndCols <- function(genes_list, col_name) {
  gene <- genes_list[1]
  if (grepl("^HGNC:\\d+$", gene)) {
    data_subset <- genesMetaDataDf_data[genesMetaDataDf_data$hgnc_id %in% genes_list, col_name]
  } else {
    # Check if the search term is an Entrez ID (e.g., ENSG12345)
    if (grepl("^\\d+$", gene)) {
      data_subset <- genesMetaDataDf_data[genesMetaDataDf_data$entrez_id %in% genes_list, col_name]
    } else {
      # Check if the search term is an MGI ID (e.g., MGI:12345)
      if (grepl("^MGI:\\d+$", gene)) {
        data_subset <- genesMetaDataDf_data[genesMetaDataDf_data$mgi_id %in% genes_list, col_name]
      } else {
        # Check if the search term is a gene symbol (e.g., Symbol_ABC)
        data_subset <- genesMetaDataDf_data[genesMetaDataDf_data$gene_symbol %in% genes_list, col_name]
      }
    }
  }
  return(data_subset)
}
# e.g. data_subset <- subsetGenesMetaDataDf(genes_list_test)

getGeneList_FromFile <- function(file_input) {
  file <- file_input
  ext <- tools::file_ext(file$datapath)
  if (ext == "csv") {
    genesMetaDataDf_data_raw <- read.csv(file$datapath)
    genesMetaDataDf_data_genes <- genesMetaDataDf_data_raw %>% rename(genes = 1)
  } else if (ext == "txt") {
    genesMetaDataDf_data_raw <- read.table(file$datapath)
    genesMetaDataDf_data_genes <- genesMetaDataDf_data_raw %>% rename(genes = 1)
  } else if (ext == "xlsx") {
    genesMetaDataDf_data_raw <- readxl::read_excel(file$datapath)
    genesMetaDataDf_data_genes <- genesMetaDataDf_data_raw %>% rename(genes = 1)
  } else if (ext == "tsv") {
    genesMetaDataDf_data_raw <- read.delim(file$datapath)
    genesMetaDataDf_data_genes <- genesMetaDataDf_data_raw %>% rename(genes = 1)
  } else {
    print('upload one of: tsv, csv, excel or txt format')
  }  
  return(genesMetaDataDf_data_genes$genes)
}

# Preprocessing: Get gene lists from files + unify into one ----
# 1) multiFileInput_unify: consolidate file uploads into; unified_gene_list & gene_lists
getGeneList_multiFileInput <- function(file_input) {
  # Get gene lists
  gene_lists <- list()
  unified_gene_list <- c()
  # Get the file path(s)
  file <- file_input
  path <- file$datapath
  for (i in path) {
    ext <- tools::file_ext(i)
    if (ext == "csv") {
      genesMetaDataDf_data_raw <- read.csv(i)
    } else if (ext == "txt") {
      genesMetaDataDf_data_raw <- read.table(i)
    } else if (ext == "xlsx") {
      genesMetaDataDf_data_raw <- readxl::read_excel(i)
    } else if (ext == "tsv") {
      genesMetaDataDf_data_raw <- read.delim(i)
    } else {
      print('upload one of: tsv, csv, txt or excel format')
    }
    genesMetaDataDf_data_renamed <- genesMetaDataDf_data_raw %>%
      rename(genes = 1)
    genesMetaDataDf_data_genes <- genesMetaDataDf_data_renamed$genes
    genesMetaDataDf_data_genes <- lapply(genesMetaDataDf_data_genes, function(z){ z[!is.na(z) & z != ""]})
    gene_lists <- c(gene_lists, list(genesMetaDataDf_data_genes$genes))
    unified_gene_list <- append(unified_gene_list, genesMetaDataDf_data_genes)
    
  }
  return(list(unified_gene_list, gene_lists))
}

getGeneList_multiFileInput_vis <- function(file_input) {
  # Get gene lists
  gene_lists <- list()
  unified_gene_list <- c()
  # Get the file path(s)
  file <- file_input
  paths <- file$datapath
  names <- file$name
  for (i in paths) {
    ext <- tools::file_ext(i)
    if (ext == "csv") {
      genesMetaDataDf_data_raw <- read.csv(i)
    } else if (ext == "txt") {
      genesMetaDataDf_data_raw <- read.table(i)
    } else if (ext == "xlsx") {
      genesMetaDataDf_data_raw <- readxl::read_excel(i)
    } else if (ext == "tsv") {
      genesMetaDataDf_data_raw <- read.delim(i)
    } else {
      print('upload one of: tsv, csv, txt or excel format')
    }
    # genesMetaDataDf_data_renamed <- genesMetaDataDf_data_raw %>% 
    #   rename(genes = 1)
    # genesMetaDataDf_data_genes <- genesMetaDataDf_data_renamed$genes
    # genesMetaDataDf_data_genes <- lapply(genesMetaDataDf_data_genes, function(z){ z[!is.na(z) & z != ""]})
    # gene_lists <- c(gene_lists, list(genesMetaDataDf_data_genes$genes))
    # unified_gene_list <- append(unified_gene_list, genesMetaDataDf_data_genes)
    genesMetaDataDf_data_genes <- genesMetaDataDf_data_raw %>%
      rename(genes = 1)
    #genesMetaDataDf_data_genes <- lapply(genesMetaDataDf_data_genes, function(z){ z[!is.na(z) & z != ""]})
    #genesMetaDataDf_data_genes <- genesMetaDataDf_data_genes[complete.cases(genesMetaDataDf_data_genes$genes), ]
    #print(genesMetaDataDf_data_genes$genes)
    gene_lists <- c(gene_lists, list(genesMetaDataDf_data_genes$genes))
    unified_gene_list <- append(unified_gene_list, genesMetaDataDf_data_genes$genes)
  }
  paths_and_file_names <- mapply(list, path = gene_lists, name = names, SIMPLIFY = FALSE)
  
  return(list(unified_gene_list, paths_and_file_names))
}

checkIfGeneIsMorphic <- function(gene, morphi_pipeline) {
  dpcs_with_gene <- c()
  if (gene %in% morphi_pipeline) {
    for (dpc in list_of_dpcs) {
      if (gene %in% dpc[[1]]) {
        dpcs_with_gene <- c(dpcs_with_gene, dpc[[2]])
      }
    }
    return(list(TRUE, dpcs_with_gene))
  } else {
    return(list(FALSE, dpcs_with_gene))
  }
}

checkIfValidGene <- function(gene) {
  gene <- toupper(gene)
  if (gene %in% c(genesMetaDataDf_data$gene_symbol, genesMetaDataDf_data$hgnc_id,
                  genesMetaDataDf_data$entrez_id, genesMetaDataDf_data$mgi_id,
                  genesMetaDataDf_data$uniprot_id)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# selected_dpcs <- c("JAX", "NWU", "MSK")
getGeneListsFromSelect <- function(selected_dpcs) {
  gene_lists <- c()
  for (i in list_of_dpcs) {
    if (i[[2]] %in% selected_dpcs) {
      gene_lists <- c(gene_lists, i[[1]])
    }
  }
  return(gene_lists)
}

getManyGeneListsFromSelect <- function(selected_dpcs) {
  gene_lists <- list()
  for (i in list_of_dpcs) {
    if (i[[2]] %in% selected_dpcs) {
      gene_lists <- c(gene_lists, list(i[[1]]))
    }
  }
  return(gene_lists)
}

getManyGeneListsFromSelect_vis <- function(selected_dpcs) {
  gene_lists <- list()
  for (i in list_of_dpcs) {
    if (i[[2]] %in% selected_dpcs) {
      gene_lists <- c(gene_lists, list(i))
    }
  }
  return(gene_lists)
}

# Deprecated - does not work with custom containers
# Using existing working alternative emethod instead of making this work
# showHideDTCols <- function(user_col_selection){
#   
#   cols_selected <- c()
#   
#   for (i in zipped_dt_cols) {
#     if (i[[2]] %in% user_col_selection) {
#       cols_selected <- append(cols_selected, i[[1]])
#     }
#   }
#   
#   cols <- names(genesMetaDataDf_data)
#   gene_identifiers <- cols[1:7]
#   mouse_impc_mgi <- cols[8:13]
#   disease_omim_ddg2p <- cols[14:20]
#   constraint_metrics_cell_line <- cols[21:25]
#   constraint_metrics_sequencing_data <- cols[26:42]
#   panther_db <- cols[43:47]
#   reactome <- cols[48:49]
#   gene_ontology <- cols[50:55]
#   
#   return(cols_selected)
# }
# 
# # Function to get hidden column indices based on selected data sources
# getHiddenColumns <- function(selected_sources) {
#   source_to_columns <- list(
#     'Gene IDs' = 0:6,
#     'Mouse data' = 7:12,
#     'Disease data' = 13:19,
#     'Cell line data - gene constraint metrics' = 20:24,
#     'Sequencing data - gene constraint metrics' = 25:41,
#     'Pantherdb protein data' = 42:46,
#     'Pathway data' = 47:48,
#     'Gene Ontology data' = 49:54
#   )
#   
#   hidden_columns <- unlist(lapply(selected_sources, function(source) {
#     source_to_columns[[source]]
#   }))
#   
#   all_columns <- 0:54
#   visible_columns <- setdiff(all_columns, hidden_columns)
#   return(visible_columns)
# }

# 27/11/23 Updating for new df
# showHideDTCols <- function(user_col_selection){
#   
#   cols_selected <- c()
#   
#   for (i in zipped_dt_cols) {
#     if (i[[2]] %in% user_col_selection) {
#       cols_selected <- append(cols_selected, i[[1]])
#     }
#   }
#   
#   cols <- names(genesMetaDataDf_data)
#   gene_identifiers <- cols[1:10]
#   mouse_impc_mgi <- cols[11:16]
#   disease_omim_ddg2p <- cols[17:24]
#   constraint_metrics_cell_line <- cols[25:29]
#   constraint_metrics_sequencing_data <- cols[30:44]
#   panther_db <- cols[45:49]
#   gene_ontology <- cols[50:55]
#   reactome <- cols[56:57]
#   
#   return(cols_selected)
# }

# Function to get hidden column indices based on selected data sources
getHiddenColumns <- function(selected_sources) {
  source_to_columns <- list(
    
    'Gene IDs' = 0:9,
    'Mouse data' = 10:15,
    'Disease data' = 16:23,
    'Cell line data - gene constraint metrics' = 24:28,
    'Sequencing data - gene constraint metrics' = 29:43,
    'Pantherdb protein data' = 44:48,
    'Gene Ontology data' = 49:54,
    'Pathway data' = 55:56
  )
  
  hidden_columns <- unlist(lapply(selected_sources, function(source) {
    source_to_columns[[source]]
  }))
  
  all_columns <- 0:56
  visible_columns <- setdiff(all_columns, hidden_columns)
  return(visible_columns)
}

getHiddenColumns_dpc_col <- function(selected_sources) {
  source_to_columns <- list(
    
    'DPCs studying Gene' = 0,
    'Gene IDs' = 1:10,
    'Mouse data' = 11:16,
    'Disease data' = 17:24,
    'Cell line data - gene constraint metrics' = 25:29,
    'Sequencing data - gene constraint metrics' = 30:44,
    'Pantherdb protein data' = 45:49,
    'Gene Ontology data' = 50:55,
    'Pathway data' = 56:57
    
  )
  
  hidden_columns <- unlist(lapply(selected_sources, function(source) {
    source_to_columns[[source]]
  }))
  
  all_columns <- 0:57
  visible_columns <- setdiff(all_columns, hidden_columns)
  return(visible_columns)
}

# #Test showHideDTCols - working 3/11/23
# xyz <- c("gene_identifiers", "reactome")
# showHideDTCols <- function(){
#   zipped_dt_cols <- readRDS("./rda_data/zipped_dt_cols.rda")
# 
#   #table_data <- tableData()
#   cols_selected <- c()
# 
#   for (i in zipped_dt_cols) {
#     if (i[[2]] %in% xyz) {
#       cols_selected <- append(cols_selected, i[[1]])
#     }
#   }
# 
#   cols <- names(genesMetaDataDf_data)
#   gene_identifiers <- cols[1:7]
#   mouse_impc_mgi <- cols[8:13]
#   disease_omim_ddg2p <- cols[14:20]
#   constraint_metrics_cell_line <- cols[21:25]
#   constraint_metrics_sequencing_data <- cols[26:42]
#   panther_db <- cols[43:47]
#   reactome <- cols[48:49]
#   gene_ontology <- cols[50:55]
# 
#   return(genesMetaDataDf_data[, cols_selected, drop = FALSE])
# }
# xyz_test <- showHideDTCols()

# Get zipped list for showHideDTCols
# list1 <- list(gene_identifiers, mouse_impc_mgi, disease_omim_ddg2p,
#               constraint_metrics_cell_line, constraint_metrics_sequencing_data,
#               panther_db, reactome, gene_ontology)
# 
# list2 <- list('gene_identifiers', 'mouse_impc_mgi', 'disease_omim_ddg2p',
#                       'constraint_metrics_cell_line', 'constraint_metrics_sequencing_data',
#                       'panther_db', 'reactome', 'gene_ontology')
# 
# # Combine the two lists into a new list with pairs
# combined_list <- list()
# 
# for (i in 1:length(list1)) {
#   pair <- list(list1[[i]], list2[[i]])
#   combined_list[[i]] <- pair
# }
# 
# # Print the combined list
# combined_list[[1]][[2]]
# saveRDS(combined_list, "./rda_data/zipped_dt_cols.rda")

# 27/11/23
# Get zipped list for showHideDTCols
# cols <- names(all_data_4)
# gene_identifiers <- cols[1:10]
# mouse_impc_mgi <- cols[11:16]
# disease_omim_ddg2p <- cols[17:24]
# constraint_metrics_cell_line <- cols[25:29]
# constraint_metrics_sequencing_data <- cols[30:44]
# panther_db <- cols[45:49]
# gene_ontology <- cols[50:55]
# reactome <- cols[56:57]
# 
# list1 <- list(gene_identifiers, mouse_impc_mgi, disease_omim_ddg2p,
#               constraint_metrics_cell_line, constraint_metrics_sequencing_data,
#               panther_db, gene_ontology, reactome)
# 
# list2 <- list('Gene IDs', 'Mouse data', 'Disease data',
#               'Cell line data - gene constraint metrics', 'Sequencing data - gene constraint metrics',
#               'Pantherdb protein data', 'Gene Ontology data', 'Pathway data')
# 
# # Combine the two lists into a new list with pairs
# combined_list <- list()
# 
# for (i in 1:length(list1)) {
#   pair <- list(list1[[i]], list2[[i]])
#   combined_list[[i]] <- pair
# }
# 
# # Print the combined list
# combined_list[[1]][[2]]
# saveRDS(combined_list, "./rda_data/zipped_dt_cols2.rda")



# moo <- getGeneListsFromSelect(selected_dpcs)
# moo
#Testing getGeneList_multiFileInput - Working 26/10/23
# getGeneList_multiFileInput_test <- function() {
#   # Get gene lists
#   gene_lists <- list()
#   unified_gene_list <- c()
#   # Get the file path(s)
#   path <- c("./DPC_geneLists/JAX_geneList.tsv", "./DPC_geneLists/MSK_geneList.tsv")
#   for (i in path) {
#     ext <- tools::file_ext(i)
#     if (ext == "csv") {
#       genesMetaDataDf_data_raw <- read.csv(i)
#     } else if (ext == "txt") {
#       genesMetaDataDf_data_raw <- read.table(i)
#     } else if (ext == "excel") {
#       genesMetaDataDf_data_raw <- read.excel(i)
#     } else if (ext == "tsv") {
#       genesMetaDataDf_data_raw <- read.delim(i)
#     } else {
#       print('upload one of: tsv, csv, txt or excel format')
#     }
#     genesMetaDataDf_data_genes <- genesMetaDataDf_data_raw %>%
#       rename(genes = 1)
#     genesMetaDataDf_data_genes <- lapply(genesMetaDataDf_data_genes, function(z){ z[!is.na(z) & z != ""]})
#     print(genesMetaDataDf_data_genes$genes)
#     gene_lists <- c(gene_lists, list(genesMetaDataDf_data_genes$genes))
#     unified_gene_list <- append(unified_gene_list, genesMetaDataDf_data_genes$genes)
#   }
#   return(list(unified_gene_list, gene_lists))
# }
# 
# result <- getGeneList_multiFileInput_test()
# result[[2]]
# subset_test <- subsetGenesMetaDataDf(result[[1]])

# TEST! - Working 26/10/23
# getGeneList_FromFile_test <- function() {
#   #ext <- tools::file_ext(file$datapath)
#   ext <- "tsv"
#  if (ext == "tsv") {
#    genesMetaDataDf_data_raw <- read.delim("/Users/gabrielm/Desktop/Rstuff/gene-annotations/genesetexplore/inst/extdata/raw/JAX gene list - Sheet1.tsv")
#    genesMetaDataDf_data_genes <- genesMetaDataDf_data_raw %>% rename(genes = 1)
#   } else {
#     print('upload one of: tsv, csv, excel or txt format')
#   }
# 
#   return(genesMetaDataDf_data_genes$genes)
# }
# 
# gene_list <- getGeneList_FromFile()
# test_df <- subsetGenesMetaDataDf(gene_list)

# rename showhide cols names
# zipped_dt_cols[[1]][[2]] <- 'Gene IDs'
# zipped_dt_cols[[2]][[2]] <- 'Mouse data'
# zipped_dt_cols[[3]][[2]] <- 'Disease data'
# zipped_dt_cols[[4]][[2]] <- 'Cell line data - gene constraint metrics'
# zipped_dt_cols[[5]][[2]] <- 'Sequencing data - gene constraint metrics'
# zipped_dt_cols[[6]][[2]] <- 'Pantherdb protein data'
# zipped_dt_cols[[7]][[2]] <- 'Pathway data'
# zipped_dt_cols[[8]][[2]] <- 'Gene Ontology data'
# saveRDS(zipped_dt_cols, './rda_data/zipped_dt_cols.rda')

# Plots ----
# FUNCTIONS ----
getImpcPlotData <- function(gene_list) {
  
  impc_data <- main.annotated.data.frame[main.annotated.data.frame$gene_symbol %in% gene_list, c('mgi_id', 'impc_viability')]
  
  impc_plot_data <- impc_data %>%
    filter(mgi_id != "NA") %>%
    filter(impc_viability != "NA") %>%
    mutate(impc_viability_2 = ifelse(!impc_viability %in% c("lethal","subviable","viable"),
                                     "conflicting", impc_viability)) %>%
    group_by(impc_viability_2) %>%
    tally() %>%
    mutate(impc_viability_3 = factor(impc_viability_2,
                                     levels = c("lethal","subviable","viable"))) %>%
    mutate(percentage = (n/sum(n)*100))
  
  # Remove conflicting rows 
  impc_plot_data <- impc_plot_data[impc_plot_data$impc_viability_2 != "conflicting", ]
  
  # Round the numeric columns to 3 decimal places
  impc_plot_data <- impc_plot_data %>%
    mutate_at(vars('percentage'), list(~ round(., 3)))
  
  if (dim(impc_plot_data)[1] != 3) {
    # if true then one category has 0 genes and needs to be filled
    levels <- c('viable', 'subviable', 'lethal')
    current_rows <- impc_plot_data$impc_viability_3
    missing_rows <- levels[!levels %in% current_rows]
    
    # Add missing rows with a value of 0 for both 'n' and 'percentage'
    missing_data <- data.frame(impc_viability_3 = missing_rows, n = 0, percentage = 0)
    
    # Update impc_plot_data with the missing rows
    impc_plot_data <- bind_rows(impc_plot_data, missing_data)
  }
  
  return(impc_plot_data)
}

getMgiPlotData <- function(gene_list) {
  
  mgi_data <- main.annotated.data.frame[main.annotated.data.frame$gene_symbol %in% gene_list, c('mgi_id', 'mgi_viability')]
  
  mgi_plot_data <- mgi_data %>%
    filter(mgi_id != "NA") %>%
    filter(mgi_viability != "NA") %>%
    mutate(mgi_viability_2 = ifelse(!mgi_viability %in% c("lethal", "viable"),
                                    "conflicting", mgi_viability)) %>%
    group_by(mgi_viability_2) %>%
    tally() %>%
    mutate(mgi_viability_3 = factor(mgi_viability_2,
                                    levels = c("lethal", "viable"))) %>%
    mutate(percentage = (n/sum(n)*100))
  
  # Remove conflicting rows 
  mgi_plot_data <- mgi_plot_data[mgi_plot_data$mgi_viability_2 != "conflicting", ]
  
  # Round the numeric columns to 3 decimal places
  mgi_plot_data <- mgi_plot_data %>%
    mutate_at(vars('percentage'), list(~ round(., 3)))
  
  if (dim(mgi_plot_data)[1] != 2) {
    # if true then one category has 0 genes and needs to be filled
    levels <- c('viable', 'lethal')
    current_rows <- mgi_plot_data$mgi_viability_3
    missing_rows <- levels[!levels %in% current_rows]
    
    # Add missing rows with a value of 0 for both 'n' and 'percentage'
    missing_data <- data.frame(mgi_viability_3 = missing_rows, n = 0, percentage = 0)
    
    # Update impc_plot_data with the missing rows
    mgi_plot_data <- bind_rows(mgi_plot_data, missing_data)
  }
  
  return(mgi_plot_data)
}

# Generate impc plot 
generateImpcPlot <- function(impc_plot_data) {
  # conflicted::conflicts_prefer(plotly::layout)
  x_axis <- c("lethal", "subviable", "viable")
  data <- impc_plot_data[, c('impc_viability_3', 'percentage')]
  new_col_names <- c("x_axis", "percentage")
  colnames(data) <- new_col_names
  
  impc_plot <- plot_ly(data, x = ~x_axis, y = ~percentage, type = 'bar',
                       name = "EXAMPLE", textposition = 'outside', text = ~percentage) %>%
    plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'IMPC preweaning viability assessment'))
  
  return(impc_plot)
}

# Generate mgi plot
generateMgiPlot <- function(mgi_plot_data) {
  # conflicted::conflicts_prefer(plotly::layout)
  x_axis <- c("lethal", "viable")
  data <- mgi_plot_data[, c('mgi_viability_3', 'percentage')]
  new_col_names <- c("x_axis", "percentage")
  colnames(data) <- new_col_names
  
  mgi_plot <- plot_ly(data, x = ~x_axis, y = ~percentage, type = 'bar',
                      name = "EXAMPLE", textposition = 'outside', text = ~percentage) %>%
    plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'MGI viability assessment'))
  
  return(mgi_plot)
}

# disease plots ----
getHasOmimPlotData <- function(gene_list) {
  
  omim_data <- genesMetaDataDf_data[genesMetaDataDf_data$gene_symbol %in% gene_list, c('hgnc_id', 'omim_phenotype_name')]
  
  omim_plot_data <- omim_data %>%
    mutate(has_omim_phenotype = if_else(!is.na(omim_phenotype_name), "yes", "no"))
  
  omim_summary_data <- omim_plot_data %>%
    group_by(has_omim_phenotype) %>%
    summarize(count = n()) %>%
    mutate(percentage = (count / sum(count)) * 100)
  
  # Round the numeric columns to 3 decimal places
  omim_summary_data <- omim_summary_data %>%
    mutate_at(vars('percentage'), list(~ round(., 3)))
  
  if (dim(omim_summary_data)[1] != 2) {
    # if true then one category has 0 genes and needs to be filled
    levels <- c('yes', 'no')
    current_rows <- omim_summary_data$has_omim_phenotype
    missing_rows <- levels[!levels %in% current_rows]
    
    # Add missing rows with a value of 0 for both 'n' and 'percentage'
    missing_data <- data.frame(has_omim_phenotype = missing_rows, n = 0, percentage = 0)
    
    # Update impc_plot_data with the missing rows
    omim_summary_data <- bind_rows(omim_summary_data, missing_data)
  }
  
  return(omim_summary_data)
}

generateHasOmimPlot <- function(omim_summary_data) {
  # conflicted::conflicts_prefer(plotly::layout)
  x_axis <- c("yes", "no")
  data <- omim_summary_data[, c('has_omim_phenotype', 'percentage')]
  new_col_names <- c("x_axis", "percentage")
  colnames(data) <- new_col_names
  
  omim_plot <- plot_ly(data, x = ~x_axis, y = ~percentage, type = 'bar',
                       name = "EXAMPLE", textposition = 'outside', text = ~percentage) %>%
    plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'Mendelian disease association (OMIM)'))
  
  return(omim_plot)
}

getOmimLethalityPlotData <- function(gene_list) {
  
  omim_lethality_data <- genesMetaDataDf_data[genesMetaDataDf_data$gene_symbol %in% gene_list, c('hgnc_id', 'omim_gene_lethality')]
  
  omim_summary_data <- omim_lethality_data %>%
    filter(!is.na(omim_gene_lethality)) %>%
    filter(omim_gene_lethality %in% c("lethal", "nonlethal")) %>%
    group_by(omim_gene_lethality) %>%
    summarize(count = n()) %>%
    mutate(percentage = (count / sum(count)) * 100)
  
  # Round the numeric columns to 3 decimal places
  omim_lethality_summary_data <- omim_summary_data %>%
    mutate_at(vars('percentage'), list(~ round(., 3)))
  
  if (dim(omim_lethality_summary_data)[1] != 2) {
    # if true then one category has 0 genes and needs to be filled
    levels <- c("lethal", "nonlethal")
    current_rows <- omim_lethality_summary_data$omim_gene_lethality
    missing_rows <- levels[!levels %in% current_rows]
    
    # Add missing rows with a value of 0 for both 'n' and 'percentage'
    missing_data <- data.frame(omim_gene_lethality = missing_rows, n = 0, percentage = 0)
    
    # Update impc_plot_data with the missing rows
    omim_lethality_summary_data <- bind_rows(omim_lethality_summary_data, missing_data)
  }
  
  return(omim_lethality_summary_data)
}

# omim lethality
generateOmimLethalityPlot <- function(omim_lethality_summary_data) {
  # conflicted::conflicts_prefer(plotly::layout)
  x_axis <- c("lethal", "nonlethal")
  data <- omim_lethality_summary_data[, c('omim_gene_lethality', 'percentage')]
  new_col_names <- c("x_axis", "percentage")
  colnames(data) <- new_col_names
  
  omim_lethality_plot <- plot_ly(data, x = ~x_axis, y = ~percentage, type = 'bar',
                                 name = "Example", textposition = 'outside', text = ~percentage) %>%
    plotly::layout(yaxis = list(title = '% of genes'), xaxis = list(title = 'Lethal Phenotypes (OMIM)'))
  
  return(omim_lethality_plot)
}

# constraint metrics
constraintMetricsPlots <- function(gene_lists_for_plots, metric_col_name, x_axis_text) {
  metrics_data_list <- list()
  for (i in gene_lists_for_plots) {
    metrics_data <- gene.constraint.metrics.num.only[gene.constraint.metrics.num.only$gene_symbol %in% (i[[1]]), c('gene_symbol', metric_col_name)]
    metrics_data_list <- c(metrics_data_list, list(metrics_data))
  }
  
  df <- purrr::reduce(metrics_data_list, full_join, by = "gene_symbol")
  
  # Extract the second elements (list names) from gene_lists_for_plots
  list_names <- sapply(gene_lists_for_plots, function(x) x[[2]])
  
  # Create column names for the dataframe
  col_names <- c("x_axis", list_names)
  
  # Assign column names to your dataframe (replace df with your actual dataframe)
  colnames(df) <- col_names
  # set y_col as first value name for initial plotly obj
  y_col <- names(df)[2] # first value after xaxis column
  y_col
  p <- plot_ly(df, y = as.formula(paste0("~", y_col)), x = y_col, name = y_col, type = "violin", box = list(visible = T),
               hoverinfo = "text", hovertext = paste("Gene Symbol: ", df$x_axis, "<br>", x_axis_text, df[[y_col]])) %>%
    plotly::layout(yaxis = list(title = x_axis_text))
  
  # set y_cols2 for rest of value names for traces
  y_cols1<- names(df)[-1]
  y_cols2 <- y_cols1[-1]
  # Add traces
  for (i in y_cols2) {
    text_col <- paste0("text_", i)  # New variable for dynamic text
    df[[text_col]] <- df[[i]]
    
    p <- p %>%
      add_trace(data = df, y = as.formula(paste0("~", i)), x = i, name = i, text = as.formula(paste0("~", text_col)),
                hoverinfo = "text", hovertext = paste("Gene Symbol: ", df$x_axis, "<br>", x_axis_text, df[[i]]))
  }
  # Print the resulting plot
  return(p)
}

# Gene search highlight data point
addGeneTrace <- function(plot, col, gene, x_axis_text) {
  for (i in plot$x$attrs) {
    plot <- plot %>%
      add_trace(
        y = gene.constraint.metrics.num.only[[col]][gene.constraint.metrics.num.only$gene_symbol == gene],
        x = i$x, 
        name = gene,
        hoverinfo = "text", 
        hovertext = paste("Gene Symbol: ", gene, "<br>", x_axis_text, 
                          gene.constraint.metrics.num.only[[col]][gene.constraint.metrics.num.only$gene_symbol == gene]),
        type = "scatter", mode = 'markers'
      )
  }
  return(plot)
}

# Threshold line
hline <- function(y = 0, color = "grey") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(dash = "dash", color = color)
  )
}