boxplot_shannon_diversity<-function(objPhyloseq){
  objPhyloseq@sam_data$SampleType <- factor(objPhyloseq@sam_data$SampleType, levels=c("Females", "Males", "Larvae", "Pulp"))
p = plot_richness(objPhyloseq, x="SampleType", color="SampleType", measures= "Shannon")
p + ggplot2::geom_boxplot()  + ggplot2::facet_wrap("Host") +
  ggplot2::stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),
                 geom="errorbar", width=0.2) +
  ggplot2::stat_summary(fun.y=mean, geom="point") +
  ggplot2::theme_classic()
}
