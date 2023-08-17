# MorPhiC_RShinyApp
RShiny App for browsing the MorPhiC gene list, meta-data and data visualisations.

### Metadata
- Data production center (DPC) genes of interest 
- [Gene Ontology](http://geneontology.org/)
- [PANTHERdb](https://www.pantherdb.org/)
- [Reactome](https://reactome.org/)
- [IMPC (International Mouse Phenotyping Consortium)](https://www.mousephenotype.org/)
- Intolerance to Variation/Gene constraint metrics:
  - [DepMap](https://depmap.org/portal/)
  - [H1-iCas9 Genetic Screen on MEFs and Laminin](https://www.sciencedirect.com/science/article/pii/S2211124719302128)
  - [gnomAD loss-of-function observed/expected]()
  - [Selection against heterozygous Loss of Function (rgcme)](https://pubmed.ncbi.nlm.nih.gov/37214792/)
  - [Selection against heterozygous Loss of Function (posterior)](https://www.biorxiv.org/content/10.1101/2023.05.19.541520v1)
- Disease data:
  - [Online Mendelian Inheritance in Man (OMIM)](https://www.omim.org/)
  - [Development disorder gene2phenotpye (DDG2P)](https://www.ebi.ac.uk/gene2phenotype)

## To run the app...

Set your working directroy to the folder containing **app.R** and **rda_data**

Run in the console `shiny::runApp()`
