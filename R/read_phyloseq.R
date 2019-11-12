read_to_phyloseq<- function(mapping, biom){
  map=import_qiime_sample_data("mapping")
  otus=import_biom("biom", parseFunction=parse_taxonomy_greengenes)
  mergedata=merge_phyloseq(otus,map)
  message("Creating a phyloseq object")
}
