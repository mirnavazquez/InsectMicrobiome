read_to_phyloseq<- function(mapping, biom){
  message(mapping)
  message(biom)
  map=phyloseq::import_qiime_sample_data(mapping)
  otus=phyloseq::import_biom(biom, parseFunction=parse_taxonomy_greengenes)
  mergedata=phyloseq::merge_phyloseq(otus,map)
  message("Creating a phyloseq object")
}

