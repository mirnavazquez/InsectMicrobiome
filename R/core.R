#' @title Create a core plot
#' @description This fuction takes a phyloseq object and create a pdf file with plots information.
#' @param objPhyloseq A phyloseq object.
#' @param niveau The subsseting object. Valid levels are "Females", "Males", "Larvae", "Pulp", "all", "adults" and "all-stages"
#' @details This function is part of a package used for the analysis of microbial communities.
#' @examples
#' core(mergedata, "Females")
#' @export
core<-function(objtPhyloseq, niveau){

  if(niveau == "Females"){
    pseq.1 <- phyloseq::subset_samples(objtPhyloseq, SampleType == "Females" )
  } else if (niveau == "Males") {
    pseq.1 <- phyloseq::subset_samples(objtPhyloseq, SampleType == "Males" )
  } else if (niveau == "Larvae") {
    pseq.1 <- phyloseq::subset_samples(objtPhyloseq, SampleType == "Larvae" )
  } else if (niveau == "Pulp") {
    pseq.1 <- phyloseq::subset_samples(objtPhyloseq, SampleType == "Pulp" )
  } else if (niveau == "all") {
    pseq.1 <- phyloseq::subset_samples(objtPhyloseq)
  } else if (niveau == "adults") {
    pseq.1 <- phyloseq::subset_samples(objtPhyloseq, SampleType == "Females" | SampleType == "Males" )
  } else if (niveau == "all-stages") {
    pseq.1 <- phyloseq::subset_samples(objtPhyloseq, SampleType == "Females" | SampleType == "Males" | SampleType == "Larvae")
  }

  pseq.rel <- microbiome::transform(pseq.1, "compositional")
  pseq.core <- microbiome::core(pseq.rel, detection = 0, prevalence = .5)
  core.taxa <- microbiome::taxa(pseq.core)
  tax.mat <- phyloseq::tax_table(pseq.core)
  tax.df=as.data.frame(pseq.core@tax_table@.Data)
  tax.df$OTU <- rownames(tax.df)
  core.taxa.class <- dplyr::filter(tax.df, rownames(tax.df) %in% core.taxa)
  knitr::kable(head(core.taxa.class))
  detections <- 10^seq(log10(1e-5), log10(.2), length = 10)
  prevalences <- seq(.05, 1, .05)
  healthycore <-  microbiome::plot_core(pseq.rel, plot.type = "heatmap",
                                        prevalences = prevalences,
                                        detections = detections,
                                        colours = rev(RColorBrewer::brewer.pal(5, "Spectral")),
                                        min.prevalence = .5, horizontal = F)
  df <- healthycore$data
  list <- df$Taxa
  tax <- phyloseq::tax_table(pseq.1)
  tax=as.data.frame(pseq.1@tax_table@.Data)
  tax$OTU <- rownames(tax)
  tax2 <- dplyr::filter(tax, rownames(tax) %in% list)
  tax.unit <- tidyr::unite(tax2, Taxa_level,c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "OTU"), sep = "_;", remove = TRUE)
  tax.unit$Taxa_level <- gsub(pattern="[a-z]__",replacement="", tax.unit$Taxa_level)
  df$Taxa <- tax.unit$Taxa_level
  knitr::kable(head(df))
  df2<-df[order(-df$Prevalence, -df$DetectionThreshold), ]
  healthycore$data <- df2
  pdf(paste0("core",niveau,".pdf"), width=10, height=10)
  plot(healthycore + ggplot2::theme(axis.text.y = ggplot2::element_text(face="italic")) + viridis::scale_fill_viridis())
  dev.off()
}

