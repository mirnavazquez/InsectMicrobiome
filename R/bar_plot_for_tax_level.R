bar_plot_for_tax_level<-function(objPhyloseq, rangoTaxonomico, orden){
  tax=as.data.frame(objPhyloseq@tax_table@.Data)
  i <- which(colnames(tax) == rangoTaxonomico)
  color_rangoTaxonomico=levels(tax[,i])
  colors_rangoTaxonomico <- colorRampPalette(RColorBrewer::brewer.pal((length(color_rangoTaxonomico)), "Paired"))(length(color_rangoTaxonomico))
  if(rangoTaxonomico == "Genus"){
  pdf("Figure.pdf", width=20, height=15)
  } else if (rangoTaxonomico == "Family") {
    pdf("Figure.pdf", width=15, height=15)
  } else if (rangoTaxonomico == "Phylum") {
    pdf("Figure.pdf", width=10, height=10)
  } else if (rangoTaxonomico == "Class") {
    pdf("Figure.pdf", width=10, height=10)
  }
  psdat.rangoTaxonomico <- tax_glom(objPhyloseq, taxrank = rangoTaxonomico)
  ps.meltrangoTaxonomico <- psmelt(psdat.rangoTaxonomico)
  ps.meltrangoTaxonomico$SampleType <- factor(ps.meltrangoTaxonomico$SampleType, 
                                              levels= orden)
  p<-ggplot2::ggplot(ps.meltrangoTaxonomico, ggplot2::aes(x = SampleType, y = Abundance, fill = rangoTaxonomico)) +
    ggplot2::geom_bar(stat = "identity", position = "fill") + ggplot2::facet_wrap("Host") +
    ggplot2::scale_fill_manual(values = colors_rangoTaxonomico)
  print(p)
  dev.off()
}
