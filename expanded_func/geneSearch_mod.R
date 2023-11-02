library(bslib)

geneSearchUI <- function(id) {
  
  tagList(
    searchInput(
      NS(id, 'gene_search'), placeholder = 'Search Gene',
      btnSearch = icon("magnifying-glass"), btnReset = icon("xmark")
    ),
    uiOutput(
      NS(id, 'gene_search_result_gene_info'),
      style = "width: 100%;"
    )
  )
  
}

geneSearchServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # using toupper means a few genes may be lost as they are lower case - look into this
    output$gene_search_result_gene_info <- renderUI({
      is_valid_gene <- checkIfValidGene(toupper(input$gene_search))
      if (input$gene_search != '') {
        
        if (is_valid_gene == TRUE) {
          
          gene_info <- subsetGenesMetaDataDf_rowsAndCols(toupper(input$gene_search), c('gene_symbol', 'gene_name', 'alias_symbol'))
          
          impc_mgi <- subsetGenesMetaDataDf_rowsAndCols(toupper(input$gene_search), c('mgi_id' ,'impc_viability', 'mgi_viability', 'impc_phenotypes_homozygote',
                                                                                      'impc_phenotypes_heterozygote', 'impc_phenotypes_hemizygote', 'impc_one2one_ortholog'))
          omim <- subsetGenesMetaDataDf_rowsAndCols(toupper(input$gene_search), c('omim_phenotpye', 'omim_lethality', 'omim_earliest_lethality_category'))
          ddg2p <- subsetGenesMetaDataDf_rowsAndCols(toupper(input$gene_search), c('dd_g2p_phenotpye', 'dd_g2p_confidence_category', 'dd_g2p_allelic_requirement',
                                                                                   'dd_g2p_organ_specificity_list'))
          symbol <- gene_info[[1]]
          name <- gene_info[[2]]
          alias <- gene_info[[3]]
          
          mgi_id <- impc_mgi[[1]]
          impc_viability <- impc_mgi[[2]]
          mgi_viability <- impc_mgi[[3]]
          impc_phenotypes_homozygote <- impc_mgi[[4]]
          impc_phenotypes_heterozygote <- impc_mgi[[5]]
          impc_phenotypes_hemizygote <- impc_mgi[[6]]
          impc_one2one_ortholog <- impc_mgi[[7]]
          
          omim_phenotype <- omim[[1]]
          omim_lethality <- omim[[2]]
          omim_earliest_lethality_category <- omim[[3]]
          
          dd_g2p_phenotpye <- ddg2p[[1]]
          dd_g2p_confidence_category <- ddg2p[[2]]
          dd_g2p_allelic_requirement <- ddg2p[[3]]
          dd_g2p_organ_specificity_list <- ddg2p[[4]]

          morphic_pipeline_check <- checkIfGeneIsMorphic(toupper(input$gene_search), morphic_pipeline)
          if (morphic_pipeline_check[[1]] == TRUE)
          {
            dpc_list_str <- paste(morphic_pipeline_check[[2]], collapse = ", ")
            is_morphic <- glue('<h3 style="font-size: 18px;">DPCs studying gene: {dpc_list_str}</h3>')
          } else {
            is_morphic <- '<h3 style="font-size: 18px;">This gene is not in the MorPhiC pipeline yet</h3>'
          }
          
          HTML(glue('
<div class="container pt-2 pb-2 border border-dark rounded mb-4" style="border: 5px solid #000; background-color: #fff;">
  <h3 class="font-weight-bold text-black" style="font-size: 22px;">
    <span style="background-color: #007bff; color: #000; padding: 5px;">Gene info</span>
  </h3>
              <div class="row justify-content-center">
                <div class="col-md-auto">
                  <div class="row">
                    <h3 class="font-weight-bold" style="font-size: 18px;">Gene Symbol: {symbol}</h3>
                  </div>
                  <div class="row">
                    <h3 class="font-weight-bold" style="font-size: 18px;">Gene Name: {name}</h3>
                  </div>
                  <div class="row">
                    <h3 class="font-weight-bold" style="font-size: 18px;">Aliases: {alias}</h3>
                  </div>
                </div>
                <div class="col-md-auto">
                  <div class="row">
                    <h3 class="font-weight-bold" style="font-size: 22px;">{is_morphic}</h3>
                  </div>
                </div>
              </div>
            </div>
            
<div class="container pt-2 pb-2 border border-dark rounded mb-4" style="border: 5px solid #000; background-color: #fff;">
  <h3 class="font-weight-bold text-black" style="font-size: 22px;">
    <span style="background-color: #007bff; color: #000; padding: 5px;">Phenotypes</span>
  </h3>
                 <div class="row justify-content-center">
                        <div class="col-md-auto">
                          <div class="row">
                            <h3 class="font-weight-bold" style="font-size: 18px;">Human</h3>
                          </div>
                          <div class="row">
                            <h3 class="font-weight-bold" style="font-size: 18px;">Phenotype (OMIM): {omim_phenotype}</h3>
                          </div>
                          <div class="row">
                            <h3 class="font-weight-bold" style="font-size: 18px;">Lethality (OMIM): {omim_lethality}</h3>
                          </div>
                          <div class="row">
                            <h3 class="font-weight-bold" style="font-size: 18px;">Earliest lethality category (OMIM): {omim_earliest_lethality_category}</h3>
                          </div>
                          <div class="row">
                            <h3 class="font-weight-bold" style="font-size: 18px;">Phenotype (DDG2P): {dd_g2p_phenotpye}</h3>
                          </div>
                          <div class="row">
                            <h3 class="font-weight-bold" style="font-size: 18px;">Allelic requirement: {dd_g2p_allelic_requirement}</h3>
                          </div>
                          <div class="row">
                            <h3 class="font-weight-bold" style="font-size: 18px;">Affected organ(s): {dd_g2p_organ_specificity_list}</h3>
                          </div>
                        </div>
                        <div class="col-md-auto">
                          <div class="row">
                            <h3 class="font-weight-bold" style="font-size: 18px;">Mouse</h3>
                          </div>
                          <div class="row">
                            <h3 class="font-weight-bold" style="font-size: 18px;">MGI ID: {mgi_id}</h3>
                          </div>
                          <div class="row">
                            <h3 class="font-weight-bold" style="font-size: 18px;">Viability (MGI): {mgi_viability}</h3>
                          </div>
                          <div class="row">
                            <h3 class="font-weight-bold" style="font-size: 18px;">Viability (IMPC): {impc_viability}</h3>
                          </div>
                          <div class="row">
                            <h3 class="font-weight-bold" style="font-size: 18px;">Human-Mouse 1-1 Ortholog: {impc_viability}</h3>
                          </div>
                          <div class="row">
                            <h3 class="font-weight-bold" style="font-size: 18px;">Homozygote phenotype: {impc_phenotypes_homozygote}</h3>
                          </div>
                         <div class="row">
                            <h3 class="font-weight-bold" style="font-size: 18px;">Heterozygote phenotype: {impc_phenotypes_heterozygote}</h3>
                          </div>
                          <div class="row">
                            <h3 class="font-weight-bold" style="font-size: 18px;">Hemizygote phenotype: {impc_phenotypes_hemizygote}</h3>
                          </div>
                        </div>
                      </div>
                '
              )
            )
        } else if (is_valid_gene == FALSE) {
          HTML('Please search for a valid gene')
        }
        
      }
    })
    
  })}