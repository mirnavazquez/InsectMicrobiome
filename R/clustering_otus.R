clustering_otus<-function(mapping, biom){
  map=import_qiime_sample_data("mapping")
  otus=import_biom("biom", parseFunction=parse_taxonomy_greengenes)
  mergedata=merge_phyloseq(otus,map)

  wh0otu = genefilter_sample(mergedata, filterfun_sample(function(x) x > 5), A=0.5*nsamples(mergedata))
  GP1otu = prune_taxa(wh0otu, mergedata)
  GP1otu = transform_sample_counts(GP1otu, function(x) 1E6 * x/sum(x))
  pseq.1otu <- subset_samples(GP1otu, SampleType == "Larvae")

  pdf("OTUclustering.pdf", width=10, height=10)
  orduotu = ordinate(pseq.1otu, "PCoA", "bray")
  potu = plot_ordination(pseq.1otu, orduotu, color="Host")
  potu = potu + geom_point(size=3, alpha=0.75)
  potu = potu + scale_colour_brewer(type="qual", palette="Set1")
  potu + ggtitle("MDS/PCoA on Bray distance, Otus")
  dev.off()
}
