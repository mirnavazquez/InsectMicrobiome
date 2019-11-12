bar_plot_for_tax_level<-function(objPhyloseq, rangoTaxonomico, orden){
  tax=as.data.frame(objPhyloseq@tax_table@.Data)
  color_rangoTaxonomico=levels(tax$rangoTaxonomico)
  colors_rangoTaxonomico <- colorRampPalette(RColorBrewer::brewer.pal((length(color_rangoTaxonomico)), "Paired"))(length(color_rangoTaxonomico))
  if(rangoTaxonomico == "Genero"){
  pdf("Figure.pdf", width=20, height=15)
  } else if (rangoTaxonomico == "Family") {
    pdf("Figure.pdf", width=15, height=15)
  } else if (rangoTaxonomico == "Phylum") {
    pdf("Figure.pdf", width=10, height=10)
  } else if (rangoTaxonomico == "Class") {
    pdf("Figure.pdf", width=10, height=10)
  }
  psdat.rangoTaxonomico <- tax_glom(objPhyloseq, taxrank = "rangoTaxonomico")
  ps.meltrangoTaxonomico <- psmelt(psdat.rangoTaxonomico)
  ps.meltrangoTaxonomico$SampleType <- factor(ps.meltrangoTaxonomico$SampleType, levels=levels)
  p<-ggplot(ps.meltrangoTaxonomico, aes(x = SampleType, y = Abundance, fill = rangoTaxonomico)) +
    geom_bar(stat = "identity", position = "fill") + facet_wrap("Host") +
    scale_fill_manual(values = colors_rangoTaxonomico)
  print(p)
  dev.off()
}
