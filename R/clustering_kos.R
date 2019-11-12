clustering_kos<-function(objPhyloseq){
  wh0otu = genefilter_sample(objPhyloseq, filterfun_sample(function(x) x > 5), A=0.5*nsamples(objPhyloseq))
  GP1otu = prune_taxa(wh0otu, objPhyloseq)
  GP1otu = transform_sample_counts(GP1otu, function(x) 1E6 * x/sum(x))
  pseq.1otu <- subset_samples(GP1otu, SampleType == "Larvae")

}
