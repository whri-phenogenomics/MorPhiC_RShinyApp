box::use(
  shiny[moduleServer, NS, renderUI, uiOutput, fluidRow, observe, selectInput, radioButtons,
        downloadHandler, downloadButton, outputOptions, showModal, modalDialog, actionButton,
        observeEvent, p, fluidPage],
  bslib[layout_sidebar, sidebar, accordion, accordion_panel, card],
  DT[DTOutput, renderDT, datatable, JS],
  dplyr[...],
  utils[write.csv],
  shinycssloaders[withSpinner]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    layout_sidebar(
      # height = '750px',
      sidebar = sidebar(
        actionButton(ns('info'), 'Table info'),
        uiOutput(ns('sidebar')),
        downloadButton(ns("download_data"), "Download table"),
        actionButton(ns('disclaimer'), 'Data disclaimer')
      ),
      DTOutput(ns("gene_list_table")) %>% withSpinner(color="#017176")
    )
  )
}

#' @export
server <- function(id, gene_list_table, gene_lists) {
  moduleServer(id, function(input, output, session) {

    nice_col_names2 <- c(
      'DPC',
      'Gene symbol', 'Gene name', 'Gene group', 'HGNC ID',
      'IMPC Viability', 'IMPC Homozygote phenotypes', 'IMPC Heterozygote phenotypes', 'Window of Lethality',
      # 'MGI viability',
      'Human-Mouse ortholog',
      'OMIM Phenotype', 'Molecular basis',
      'Affected organ', 'Disease', 'Allelic requiremnet', 'Gene-Disease association confidence',
      'Percentage essential lines', 'Mean CERES score', 'Bayes factor (MEF)', 'Bayes factor (Laminin)',
      'gnomAD LOEUF', 'gnomAD Mis', 'Alpha Missense', 'Shet (GeneBayes)', 'DOMINO', 'SCoNeS', 'GISMO median', 'GISMO decile','GISMO-mis median', 'GISMO-mis decile',
      'PANTHER Class', 'PANTHER Subfamily',
      'Reactome pathway',
      'GO Biological process', 'GO Molecular function', 'GO Cellulcar component',
      'HCA Highly expressed tissues', 'GTEx average expression across tissues',
      'STRING PPIs'
    )

    index <- list(
      'DPC Genes' = 1,
      'Gene name/ID/symbol' = 2:4,
      'Mouse knockouts' = 5:9,
      'Diseases' = 10:15,
      'Constraint metrics (hPSCs)' = 16:19,
      'Constraint metrics (population sequencing)' = 20:30,
      'Protein families' = 31:32,
      'Pathways' = 33,
      'Gene Ontology' = 34:36,
      'Gene expression' = 37:38,
      'Protein interactions' = 39

    )

    output$sidebar <- renderUI({
      fluidRow(
        accordion(
          id = session$ns('display_options'),
          open = FALSE,
          accordion_panel(
            title = "Display Options",
            selectInput(
              session$ns('select_cols'),
              'Select columns',
              choices = names(index),
              multiple = TRUE,
              selected = names(index)
            ),
            selectInput(
              session$ns('select_genes'),
              'Select Gene lists',
              choices = names(gene_lists()),
              multiple = TRUE,
              selected = names(gene_lists())
            ),
            radioButtons(
              session$ns('union_intersect'),
              'Show union or intersection of genes',
              choices = c("Union", "Intersection"),
              selected = "Union"
            )
          )
        ),
        # downloadButton(session$ns("download_data"), "Download table")
      )

    })

    observe({

      input$union_intersect
      input$select_cols
      input$select_genes

      # Column selection
      cols <- c()
      for (selection in input$select_cols) {
        cols <- c(cols, index[[selection]])
      }
      cols <- unique(cols)
      cols <- sort(cols)

      # Gene list selection
      genes <- list()
      for (selection in input$select_genes) {
        genes[[selection]] <- gene_lists()[[selection]][['gene_list']]
      }

      if (is.null(input$union_intersect)) {
        selected_genes <- genes
      } else if (input$union_intersect == "Union") {
        selected_genes <- unique(unlist(genes))
      } else if (input$union_intersect == "Intersection") {
        selected_genes <- Reduce(intersect, genes)
      }

      names(gene_list_table) <- nice_col_names2

      output$gene_list_table <- renderDT({
        datatable(
          filtered_table <- gene_list_table %>%
            filter(`Gene symbol` %in% selected_genes) %>%
            select(all_of(cols)) %>%
            arrange(`Gene symbol`),
          rownames = FALSE,
          filter = 'top',
          plugins = "ellipsis",
          # filter = 'top',
          options = list(
            dom = "Bfrtip",  # B = Buttons, f = Filtering input, r = Processing, t = Table, i = Info, p = Pagination
            searching = TRUE,  # Enable global search
            # Unsure if this actually speeds up the tooltip
            initComplete = JS(
              "function(settings, json) {",
              "$('[data-toggle=\"tooltip\"]').tooltip({ delay: { show: 0, hide: 0 } });",
              "}"
            ),
            columnDefs = list(
              list(
                targets = "_all",
                render = JS("$.fn.dataTable.render.ellipsis(17, true)")
              )
            ),
            pageLength = 100,
            scrollX = TRUE
          )
        )
      })
    })


    output$download_data <- downloadHandler(
      filename = function() {
        paste("MorPhiC-Gene-List-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(data.frame(gene_list_table), file)
      }
    )
    outputOptions(output, "download_data", suspendWhenHidden = FALSE)

    observeEvent(input$info, {
      showModal(modalDialog(
        title = "Table information",
        easyClose = TRUE,
        p('This table displays all protein coding genes with associated metadata from mouse knockouts, human disease data, gene constraint metrics from hPSCs and population sequencing, gene expression, protein-protein interactions, protein families, gene ontology and pathways.'),
        p('The table also shows which genes are being studied by each of the four Data Production Centers (DPCs).'),

        p('Metadata:'),
        p('DPC: Data Production Center(s) studying gene.'),
        p("Gene symbol: The HGNC approved protein coding gene symbol"),
        p("Gene name: HGNC approved name for the gene"),
        p('Gene group: The gene group of related genes'),
        p('Entrez ID: NCBI gene ID'),
        p('IMPC viablity: Outcome of gene knockout on mouse survival or ability to thrive'),
        p('IMPC zygosity: Number of alleles knocked out in a mouse. Homozgous = both alleles knocked out, Heterozygous = one allele knocked out, Hemizygous = one allele knocked out in sex chromosome'),
        p('IMPC phenotypes: Phenotypes observed in mouse knockout. Terms are described in the Mammalian Phenotype Ontology (MP)'),
        p('Window of Lethality: Embryonic day when death occured. Early gestation lethal = E9.5, Mid gestation lethal = E12.5-15.5, Late gestation lethal = E15.5-18.5. Data was curated by Cacheiro et al. 2022'),
        p('MGI viability: Outcome of gene knockout on mouse survival'),
        p('Ortholog mapping: Human-Mouse Ortholog relationship based on GenTaR pipeline'),
        p('Include NAs: check/uncheck to include/exclude genes with missing data for that column'),
        p('Mode of inheritance: How the disease is passed to the next generation'),
        p('Disease type: Clinical significance and impact of the disorder'),
        p('Molecular basis: Details of the genetic mapping of the disorder'),
        p('OMIM Phenotype ID: Unique ID linked to disorder'),
        p('Phenotype: Name of disorder'),
        p('Lethal gene: One of three lethality caregories acording to OMIM clinical records that were data mined using a series of API queries of terms linked to lethality. Data was curated by Cacheiro et al. 2024'),
        p('Earliest age of death category: One of seven lethality categories according to the earliest age at which death occured according to OMIM records.
          L1: Prenatal death (HP:0034241); Death before birth
          L1.1: Miscariage (HP:0005268); Spontaneous loss of a fetus before the 22th week of pregnancy
          L1.2: Stillbirth (HP:0003826); Death of the fetus in utero after at least 22 weeks of gestation
          L2: Neonatal death (HP:0003811); Death within the first 28 days of life
          L3: Death in infancy (HP:0001522); Death within the first 24 months of life
          L4: Death in childhood (HP:0003819); Death during childhood, defined here as between the ages of 2 and 10 years
          L5: Death in adolescence (HP:0011421); Death during adolescence, the period between childhood and adulthood (roughly between the ages of 10 and 19 years)
          L6: Death in adulthood (HP:0033763); Cessation of life at the age of 16 years or later
          LU: Age of death undetermined. Data was curated by Cacheiro et al. 2024'),
        p('Disease: Name of disorder'),
        p('Allelic requirement: Number of mutated alleles in a gene needed to cause the disorder'),
        p('Confidence category: Confidence of the gene-phenotype association'),
        p('Organ specificity: Organ systems affected by disorder'),
        p('Include NAs: check/uncheck to include/exclude genes with missing data for that column'),
        p('Percentage of sample essential in: This was computed by calculating the number of different cancer cell lines depleted (using the -0.5 gene effect threshold) amongst all cancer cell lines'),
        p('Mean DepMap Gene Effect score: For gene effect, a score less than -0.5 represents depletion in most cell lines, while less than -1 represents strong killing, outlined here: https://forum.depmap.org/t/depmap-genetic-dependencies-faq/131'),
        p('Bayes factor (MEF): MEF and Laminin refer to the Mouse Embryonic Feeder cells and Laminin substrate used to grow H1-iCas9 human pluripotent stem cells (hPSCs). Bayes Factor (BF) scores represent a confidence measure as to whether a gene is essential, whereby a threshold of BF > 5 can be used to distinguish essential genes'),
        p('Bayes factor (Laminin): MEF and Laminin refer to the Mouse Embryonic Feeder cells and Laminin substrate used to grow H1-iCas9 human pluripotent stem cells (hPSCs). Bayes Factor (BF) scores represent a confidence measure as to whether a gene is essential, whereby a threshold of BF > 5 can be used to distinguish essential genes'),
        p('Include NAs: check/uncheck to include/exclude genes with missing data for that column'),
        p('gnomAD LOEUF: The loss-of-function observed/expected upper bound fraction (LOEUF) score is a continuous metric designed to demonstrate a gene’s intolerance to loss-of-function variation. Constrained genes are defined using the cut-off LOEUF < 0.6'),
        p('gnomAD missense score: observed/expected ratio upper bound fraction for missense variants'),
        p('AlphaMissense mean pathogenicity: This metric represents the probability of a gene being pathogenic, according to the 2023 AlphaMissense deep learning model developed by Cheng et al. 2023. Scores closer to 1 indicate higher likelihood of a gene variant being pathogenic'),
        # p('Shet (RGC-ME): These metrics are derived from the RGC-ME dataset, published by Sun et al. 2023, comprising 985,830 exomes from individuals of diverse ancestry. Here, Shet RGC-ME represents the selectin coefficient estimated per gene from heterozygous probability Loss of Function
        #        (pLoF) variation amungst the RGC-ME dataset. A Shet score greater than 0.075 indicates highly constrained genes'),
        p('Shet (GeneBayes): These metrics are derived from the machine learning model GeneBayes, created by Zeng et al. 2023. Here, Shet posterior represents the Bayesian estimation of the selectin coefficient from heterzygous LoF carriers from the gnomAD (v2.1.1) dataset. Higher Shet scores indicate more constrained genes'),
        p('DOMINO: This metric was developed by Quinodoz et al. 2017 and assesses the likelihood of a gene to harbor dominant changes using the DOMINO machine learning model'),
        p('SCoNeS: This metric was developed by Rapaport et al. 2021 and predicts the likelihood for a gene to underlie an Autosomal Dominant (AD) or Autosomal Recessive (AD) disorder. Thresholds of SCoNeS score > 0.75 and SCoNeS score < 0.25 are used
               to determine genes are underlying AR and AD disorders respectivley'),
        p('GISMO Median score: This metric was developed by Liao et al. 2024 and measures gene loss across mammals weighed by evolutionary distance relative to humans.'),
        p('GISMO Decile: This is a bin associated to the GISMO score. The GISMO score was developed by Liao et al. 2024 and measures gene loss across mammals weighed by evolutionary distance relative to humans.'),
        p('GISMO-mis Median score: This metric was developed by Liao et al. 2024 and quantifies the ratio of missense to synonymous variants across mammalian species for a given gene.'),
        p('GISMO-mis Decile: This is a bin associated to the GISMO-mis score. The GISMO-mis score was developed by Liao et al. 2024 and quantifies the ratio of missense to synonymous variants across mammalian species for a given gene.'),
        p('Panther class: PANTHERdb protein class that gene belongs to.'),
        p('Panther subfamily: PANTHERdb protein subfamily that gene belongs to.'),
        p('Reactome pathway: pathway (name and ID) that gene is involved with. Source: Reactome'),
        p('GO Biological Process: biological process (term and ID) associated with gene. Source: Gene Ontology.'),
        p('GO Molecular function: molecular process (term and ID) associated with gene. Source: Gene Ontology.'),
        p('GO Cellular process: cellular process (term and ID) associated with gene. Source: Gene Ontology.'),
        p('HCA Highly expressed tissues: tissues with high expression of associated gene. Source: Human Cell Atlas.'),
        p('GTEX average expression across tissues: GTEx expression averaged across all tissue types. Source: GTEx.'),
        p('STRING PPIs: Protein-protein interactions associated to gene. Source: STRINGdb.')
      ))
    })
    
    observeEvent(input$disclaimer, {
      showModal(modalDialog(
        title = "Data disclaimer",
        easyClose = TRUE,
        p('The information provided in our data is for research purposes only. While we strive to ensure accuracy, we do not guarantee the completeness or correctness of the data. We disclaim any liability for errors, omissions, or any outcomes resulting from the use of this information. Users are advised to verify the data independently.'),
        p('Please note that due to an error in the computation of gene annotations for MGI and Disease gene lethality annotations, any MGI or Disease lethality data downloaded before August 15, 2024, is incorrect.')
      ))
    })
    




  })
}
