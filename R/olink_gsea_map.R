#'Function which prepares names for olink_pathway_enrichment
#'
#'This function changes the Assay column in order to map all the genes in the pathway enrichment analysis
#'
#'
#' @param data NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt,SampleID, QC_Warning, NPX, and LOD
#' @param test_results a dataframe of statistical test results including Adjusted_pval and estimate columns.
#'
#' @return the two data frames with Assay column adjusted for the pathway analysis

olink_gsea_map <- function(data, test_results) {

  # Data Checks
  if(length(unique(data[, "OlinkID"]) != length(unique(test_results[, "OlinkID"])))) {
    warning("The number of Olink IDs in the data does not equal the number of Olink IDs in the test results.")
  }
  if(!("estimate" %in% colnames(test_results))) {
    stop("Estimate column is not present in test results. Please check arguments.")
  }

  data_name <- as.character(match.call()$data)
  test_results_name <- as.character(match.call()$test_results)

  test_results <- as.data.frame(test_results)
  data <- as.data.frame(data)

  test_results[, "Assay"] <- gsub("-", "", test_results[, "Assay"])
  data[, "Assay"] <- gsub("-", "", data[, "Assay"])

  gsea_mapping <- c("TRAIL" = "TNFSF10", "Flt3L" = "FLT3LG",
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
  assign(data_name, data, .GlobalEnv)
  assign(test_results_name, test_results, .GlobalEnv)

}
