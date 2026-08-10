#' Mapping of Olink protein names to gene names
#'
#' This mapping covers the following Olink 96 Panels:
#' Immuno-oncology, Inflammation.
#'
#' @format A character vector.
#' @examples
#' print(prot2gene)
#'
#' @author Stefano Bergamini
#'
#' @export
prot2gene <- c("TRAIL" = "TNFSF10", "Flt3L" = "FLT3LG",
               "TWEAK" = "TNFSF12", "IL22 RA1" = "IL22RA1",
               "OPG" = "TNFRSF11B", "PDL1" = "CD274",
               "TGFalpha" = "TGFA", "STAMPB" = "STAMBP",
               "BetaNGF" = "NGF", "SCF" = "KITLG",
               "MCP2" = "CCL8", "MCP1" = "CCL2",
               "NT3" = "NTF3", "TNFB" = "LTA",
               "IL8" = "CXCL8", "IL1 alpha" = "IL1A",
               "IFNgamma" = "IFNG", "4EBP1" = "EIF4EBP1",
               "ENRAGE" = "S100A12", "MCP3" = "CCL7",
               "MCP4" = "CCL13", "TRANCE" = "TNFSF11",
               "ST1A1" = "SULT1A1", "uPA" = "PLAU",
               "LAP TGFbeta1" = "TGFB1", "VEGFR2" = "KDR",
               "PDGF subunit B" = "PDGFB", "HO1" = "HMOX1",
               "CD40L" = "CD40LG", "IL12" = "IL12A",
               "TIE2" = "TEK", "Gal9" = "LGALS9",
               "PDL2" = "PDCD1LG2", "MICA/B" = "MICB",
               "CAIX" = "CA9", "Gal1" = "LGALS1")


#' Map Olink protein names to gene names
#'
#' @description
#' This function changes the "Assay" column of NPX Olink data and statistical
#' tests results from OlinkAnalyze package (e.g. olink_wilcox) in order to map
#' all the protein names to gene names. This is necessary to allow
#' OlinkAnalyze::olink_pathway_enrichment() to map the gene names to entrez IDs
#' for pathway enrichment analysis.
#'
#'
#' @param data NPX dataframe in long format with at least protein name (Assay),
#' OlinkID, UniProt, SampleID, QC_Warning, NPX, and LOD.
#' @param test_results A dataframe of statistical test results including
#' Adjusted_pval and estimate columns obtained from package OlinkAnalyze.
#' @param gsea_mapping Character vector mapping Olink protein names to gene
#' names. Default is "prot2gene".
#'
#' @return A list with the two data frames "data" and "test_results" having "Assay"
#' column overwritten for the pathway analysis.
#'
#' @author Stefano Bergamini
#'
#' @examples
#' if (requireNamespace("OlinkAnalyze", quietly = TRUE)){
#'
#' data("npx_data1", package = "OlinkAnalyze")
#'
#' # Select one panel
#' data_example <- npx_data1[npx_data1$Panel == "Olink Inflammation",]
#'
#' # Remove controls
#' npx_df <- data_example[!grepl("control", data_example$SampleID, ignore.case = TRUE),]
#'
#' # Run statistical test
#' wilcox_results <- OlinkAnalyze::olink_wilcox(df = npx_df,
#' variable = "Treatment", alternative = "two.sided")
#'
#' # Mapping
#' olink_gsea_map(npx_df, wilcox_results)
#' }
#'
#' @export
olink_gsea_map <- function(data, test_results, gsea_mapping=prot2gene) {

  # Data Checks
  if(length(unique(data[, "OlinkID"])) != length(unique(test_results[, "OlinkID"]))) {
    warning("The number of Olink IDs in the data does not equal the number of Olink IDs in the test results.")
  }
  if(!("estimate" %in% colnames(test_results))) {
    stop("Estimate column is not present in test results. Please check arguments.")
  }

  test_results <- as.data.frame(test_results)
  data <- as.data.frame(data)

  test_results[, "Assay"] <- gsub("-", "", test_results[, "Assay"])
  data[, "Assay"] <- gsub("-", "", data[, "Assay"])

  for (i in 1:length(test_results[, "Assay"])){
    if (test_results[i, "Assay"] %in% names(gsea_mapping)){
      test_results[, "Assay"][i] <- gsea_mapping[which(names(gsea_mapping) == test_results[i, "Assay"])]
    } else { }
  }

  for (i in 1:length(data[, "Assay"])){
    if (data[i, "Assay"] %in% names(gsea_mapping)){
      data[, "Assay"][i] <- gsea_mapping[which(names(gsea_mapping) == data[i, "Assay"])]
    } else { }
  }

  return(list(data = data, test_results = test_results))

}
