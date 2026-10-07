# Function which prepares names for olink_pathway_enrichment

This function changes the Assay column in order to map all the genes in
the pathway enrichment analysis

## Usage

``` r
olink_gsea_map(data, test_results)
```

## Arguments

- data:

  NPX data frame in long format with at least protein name (Assay),
  OlinkID, UniProt,SampleID, QC_Warning, NPX, and LOD

- test_results:

  a dataframe of statistical test results including Adjusted_pval and
  estimate columns.

## Value

the two data frames with Assay column adjusted for the pathway analysis
