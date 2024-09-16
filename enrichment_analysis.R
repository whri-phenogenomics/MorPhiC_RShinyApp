goAnalysis <- function(gene_list, background, ontology, pval, qval, percent_slice, threshold) {

  go_analysis <- clusterProfiler::enrichGO(gene          = gene_list,
                                           universe      = background,
                                           keyType = "SYMBOL",
                                           OrgDb         = "org.Hs.eg.db",
                                           ont           = ontology,
                                           pAdjustMethod = "BH",
                                           pvalueCutoff  = pval,
                                           qvalueCutoff  = qval,
                                           readable      = TRUE)

  go_analysis2 <- go_analysis %>%
    mutate(across(where(is.numeric), ~ signif(.x, 3)))


  simMatrix <- rrvgo::calculateSimMatrix(go_analysis$ID,
                                         orgdb="org.Hs.eg.db",
                                         ont=ontology,
                                         method="Rel")

  scores <- stats::setNames(-log10(go_analysis$qvalue), go_analysis$ID)
  reducedTerms <- rrvgo::reduceSimMatrix(simMatrix,
                                         scores,
                                         threshold=threshold,
                                         orgdb="org.Hs.eg.db")

  # Slice for top 10% of enriched GO terms to get clearer plots
  x <- 100/percent_slice
  top_x_percent_terms <- nrow(reducedTerms)/x

  # Generate plot
  scat_p <- rrvgo::scatterPlot(simMatrix, reducedTerms[1:top_x_percent_terms, ], algorithm = c("pca"), onlyParents = FALSE)

  # Add labels for top 5 most enriched GO terms
  x <- stats::cmdscale(as.matrix(as.dist(1-simMatrix), eig=TRUE, k=2))

  df <- cbind(as.data.frame(x),
              reducedTerms[match(rownames(x), reducedTerms$go), c("term", "parent", "parentTerm", "size")])

  # Only show top 5 most significantly enriched terms
  p <- scat_p + geom_text(aes(label=parentTerm), data=subset(df[1:5,], parent == rownames(df[1:5,])), size=4, color="black")

  return(list(p, go_analysis2))
  # return(p)
}

dpc_lists <- readRDS('./data/morphic_gene_lists2024-09-11.rda')
gene_list_table <- read.fst('./data/morphic_gene_list_table_2024-09-11.fst')
gene_lists <- list(
    'JAX' =
      list(
        'gene_list' = dpc_lists$JAX
      ),
    'MSK' =
      list(
        'gene_list' = dpc_lists$MSK
      ),
    'NWU' =
      list(
        'gene_list' = dpc_lists$NWU
      ),
    'UCSF' =
      list(
        'gene_list' = dpc_lists$UCSF
      ),
    'All MorPhiC genes' =
      list(
        'gene_list' = unique(c(dpc_lists$JAX, dpc_lists$MSK, dpc_lists$NWU, dpc_lists$UCSF))
      )
  )


# Run GO analysis
ontologies <- c('BP', 'MF', 'CC', 'ALL')
results <- list()

# Initialize the results list with ontologies
for (ont in ontologies) {
  results[[ont]] <- list()
}

# Fill in the results list with GO analysis results
for (ont in ontologies[1:2]) {
  for (i in seq_along(gene_lists)) {
    gene_list_name <- names(gene_lists)[i] # Assuming gene_lists is a named list
    result <- goAnalysis(
      gene_list = gene_lists[[i]][['gene_list']],
      background = unique(gene_list_table$gene_symbol),
      ontology = ont,
      pval = 0.01,
      qval = 0.05,
      percent_slice = 25,
      threshold = 0.7
    )

    results[[ont]][[gene_list_name]] <- result
  }
}
# results$BP$JAX[[2]]@result
# results$BP$MSK[[1]]
# results$BP$NWU[[1]]
# results$BP$UCSF[[1]]
# results$BP$`All MorPhiC genes`[[1]]
#
# results$MF$JAX[[1]]
# results$MF$MSK[[1]]
# results$MF$NWU[[1]]
# results$MF$UCSF[[1]]
# results$MF$`All MorPhiC genes`[[1]]
#
# results$CC$JAX[[1]]
# results$CC$MSK[[1]]
# results$CC$NWU[[1]]
# results$CC$UCSF[[1]]
# results$CC$`All MorPhiC genes`[[1]]
saveRDS(results[1:2], './data/go_plots.rda')

head(results$BP$JAX[[2]]@result)

reactomeAnalysis <- function(gene_list, background, pval, no_pathways_shown) {

  # Enrichment
  enriched_pathway <- background %>%
    dplyr::filter(gene_symbol %in% gene_list) %>%
    dplyr::pull(entrez_id) %>%
    ReactomePA::enrichPathway(gene = ., pvalueCutoff = pval, readable = TRUE)

  # Table
  enriched_pathway_table <- data.frame(enriched_pathway, row.names = NULL) %>%
    dplyr::select(ID, Description, qvalue) %>%
    mutate(across(where(is.numeric), ~ signif(.x, 3)))

  # Plots
  edo <- enrichplot::pairwise_termsim(enriched_pathway)
  plot <- enrichplot::emapplot(edo, showCategory = no_pathways_shown)

  return(list(plot, enriched_pathway_table))
}

protein.coding.genes <- read.delim(
  "https://ftp.ebi.ac.uk/pub/databases/genenames/hgnc/tsv/locus_types/gene_with_protein_product.txt"
) %>%
  dplyr::rename(gene_symbol = symbol)

pathway_results <- list()
for (i in seq_along(gene_lists)) {
  gene_list_name <- names(gene_lists)[i] # Assuming gene_lists is a named list
  result <- reactomeAnalysis(
    gene_list = gene_lists[[i]][['gene_list']],
    background = protein.coding.genes,
    pval = 0.05,
    no_pathways_shown = 10
  )

  pathway_results[[gene_list_name]] <- result
}

# pathway_results$JAX
# pathway_results$MSK
# pathway_results$NWU
# pathway_results$UCSF
# pathway_results$`All MorPhiC genes`
saveRDS(pathway_results, './data/pathway_plots.rda')

# Odds ratio ----
#' @export
oddsRatioPlot <- function(compare_list_option, gene_list, dataset, index) {
  # Get:
  #   1) all genes with an essentially annotation (compare_list)
  #   2) all genes with annotation (or all pcg for disease analysis) (background)
  if (compare_list_option == "OMIM Disease genes") {
    compare_list <- dataset %>%
      dplyr::filter(number_key == "The molecular basis for the disorder is known; a mutation has been found in the gene") %>%
      dplyr::pull(gene_symbol) %>%
      unique()

    background <- dataset %>%
      dplyr::select(gene_symbol) %>%
      unique()
  } else if (compare_list_option == "IMPC Lethal genes") {
    compare_list <- dataset %>%
      dplyr::filter(impc_viability == "lethal") %>%
      dplyr::pull(gene_symbol) %>%
      unique()

    background <- dataset %>%
      dplyr::filter(!is.na(impc_viability)) %>%
      dplyr::select(gene_symbol) %>%
      unique()
  } else if (compare_list_option == "DepMap Essential genes") {
    compare_list <- dataset %>%
      dplyr::filter(mean_score_all < -1) %>% # -1 is strong killing, -0.5 is depletion in most cell lines
      dplyr::pull(gene_symbol) %>%
      unique()

    background <- dataset %>%
      dplyr::filter(!is.na(mean_score_all)) %>%
      dplyr::select(gene_symbol) %>%
      unique()
  }

  # Create df:
  # col1 (gene_symbol) = all genes that have an annotation:
  # col2 (selected) = y if gene is in the user list, n if not
  # col3 (compared) = y if gene has essentially annotation, n if not
  df <- background %>%
    mutate(selected = ifelse(gene_symbol %in% gene_list, "y", "n")) %>%
    mutate(compared = ifelse(gene_symbol %in% compare_list, "y", "n"))

  # Generate contingency table
  # Ensure this is right way round
  contingency_table <- table(df$compared, df$selected)

  # Sometimes, the compared list has only one level which results in a 2x1 df
  # Can't continue with 2x1 contingency table so adjust dims
  # by Adding Haldane-Anscombe correction or continuity correction (i.e. adding second col with 0.5 as vals)
  if (all(dim(contingency_table) == c(2, 1))) {
    contingency_table <- cbind(contingency_table, n = c(0.5, 0.5))
  }
  # Similarily, convert 0s to 0.5 to avoid inf values
  contingency_table[contingency_table == 0] <- 0.5

  print(contingency_table)

  # Run odds ratio analysis
  or_all <- epitools::oddsratio(contingency_table, method = "wald")

  # Create df with results
  or_all_or <- round(or_all$measure[2], 3)
  or_all_lower <- round(or_all$measure[4], 3)
  or_all_upper <- round(or_all$measure[6], 3)
  or_all_pvalue <- round(or_all$p.value[4], 3)
  or_all_df <- data.frame(
    OR = or_all_or,
    LL = or_all_lower,
    UL = or_all_upper,
    pvalue = or_all_pvalue
  )
  return(or_all_df)
}

#' @export
batchOddsRatioPlots <- function(list_of_lists, dataset, compare_list_option, odds_ratio_func) {
  results_list <- list()  # To store results from each iteration

  # Loop through the list of lists
  for (i in seq_along(list_of_lists)) {
    # Extract the gene list and the name

    gene_list <- list_of_lists[[i]][[1]]
    print(gene_list)
    list_name <- names(list_of_lists)[[i]]
    print(list_name)
    # Print to track which list is currently processing
    # print(paste("Processing list:", list_name))

    # Call your existing function
    results <- odds_ratio_func(compare_list_option = compare_list_option, gene_list = gene_list, dataset = dataset)

    # Add a row with index and list name
    results <- results %>%
      mutate(Index = i, label = list_name)

    # Append results to the list
    results_list[[i]] <- results
  }

  # Combine all results into a single data frame
  final_results <- do.call(rbind, results_list)
  return(final_results)
}

#' @export
forestPlot <- function(dat) {
  plot <- ggplot(dat, aes(y = Index, x = OR)) +
    geom_point(shape = 18, size = 5) +
    geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
    geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
    scale_y_continuous(name = "", breaks=1:length(dat$label), labels = dat$label, trans = "reverse") +
    xlab("Odds Ratio") + # check CI confidence interval
    ylab(" ") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.y = element_text(size = 12, colour = "black"),
          axis.text.x.bottom = element_text(size = 12, colour = "black"),
          axis.title.x = element_text(size = 12, colour = "black"))
  return(plot)
}

dpc_lists <- readRDS('./data/morphic_gene_lists2024-09-11.rda')
gene_list_table <- read.fst('./data/morphic_gene_list_table_2024-09-11.fst')
plots_df <- read.fst('./data/all_data_ege_2024-09-11.fst')
gene_lists <- list(
  'JAX' =
    list(
      'gene_list' = dpc_lists$JAX
    ),
  'MSK' =
    list(
      'gene_list' = dpc_lists$MSK
    ),
  'NWU' =
    list(
      'gene_list' = dpc_lists$NWU
    ),
  'UCSF' =
    list(
      'gene_list' = dpc_lists$UCSF
    ),
  'All MorPhiC genes' =
    list(
      'gene_list' = unique(c(dpc_lists$JAX, dpc_lists$MSK, dpc_lists$NWU, dpc_lists$UCSF))
    )
)

r <- batchOddsRatioPlots(
  list_of_lists = gene_lists,
  dataset = plots_df,
  compare_list_option = 'OMIM Disease genes',
  odds_ratio_func = oddsRatioPlot
)
rr <- batchOddsRatioPlots(
  list_of_lists = gene_lists,
  dataset = plots_df,
  compare_list_option = 'IMPC Lethal genes',
  odds_ratio_func = oddsRatioPlot
)
rrr <- batchOddsRatioPlots(
  list_of_lists = gene_lists,
  dataset = plots_df,
  compare_list_option = 'DepMap Essential genes',
  odds_ratio_func = oddsRatioPlot
)
odds_ratio_disease <- forestPlot(r)
odds_ratio_impc <- forestPlot(rr)
odds_ratio_depmap <- forestPlot(rrr)
odds_ratio_plots <- list(
  'odds_ratio_disease' = list(odds_ratio_disease, r),
  'odds_ratio_impc' = list(odds_ratio_impc, rr),
  'odds_ratio_depmap' = list(odds_ratio_depmap, rrr)
)
odds_ratio_plots$odds_ratio_disease[[1]]
odds_ratio_plots$odds_ratio_disease[[2]]
saveRDS(odds_ratio_plots, './data/odds_ratio_plots.rda')


