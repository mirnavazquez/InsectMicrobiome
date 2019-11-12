core<-function(objtPhyloseq){
  pseq.2 <- prune_taxa(taxa_sums(objtPhyloseq) > 0, objtPhyloseq)
  pseq.1 <- subset_samples(pseq.2, SampleType == "Larvae" )
  pseq.rel <- microbiome::transform(pseq.1, "compositional")
  core.taxa.standard <- core_members(pseq.rel, detection = 0, prevalence = .5)
  pseq.core <- core(pseq.rel, detection = 0, prevalence = .5)
  core.taxa <- taxa(pseq.core)
  tax.mat <- tax_table(pseq.core)
  tax.df=as.data.frame(pseq.core@tax_table@.Data)
  tax.df$OTU <- rownames(tax.df)
  core.taxa.class <- dplyr::filter(tax.df, rownames(tax.df) %in% core.taxa)
  knitr::kable(head(core.taxa.class))
  detections <- 10^seq(log10(1e-5), log10(.2), length = 10)
  prevalences <- seq(.05, 1, .05)
  healthycore <- plot_core(pseq.rel, plot.type = "heatmap",
                           prevalences = prevalences,
                           detections = detections,
                           colours = rev(brewer.pal(5, "Spectral")),
                           min.prevalence = .5, horizontal = F)
  df <- healthycore$data
  list <- df$Taxa
  tax <- tax_table(pseq.2)
  tax=as.data.frame(pseq.2@tax_table@.Data)
  tax$OTU <- rownames(tax)
  tax2 <- dplyr::filter(tax, rownames(tax) %in% list)
  tax.unit <- tidyr::unite(tax2, Taxa_level,c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "OTU"), sep = "_;", remove = TRUE)
  tax.unit$Taxa_level <- gsub(pattern="[a-z]__",replacement="", tax.unit$Taxa_level)
  df$Taxa <- tax.unit$Taxa_level
  knitr::kable(head(df))
  df2<-df[order(-df$Prevalence, -df$DetectionThreshold), ]
  healthycore$data <- df2
  pdf("core.pdf", width=10, height=10)
  plot(healthycore + theme(axis.text.y = element_text(face="italic")) + scale_fill_viridis())
  dev.off()
}
