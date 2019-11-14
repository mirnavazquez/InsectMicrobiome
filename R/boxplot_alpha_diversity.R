#' @title Create alpha diversity plots
#' @description This fuction takes a phyloseq object and create a pdf file with plots information.
#' @param objPhyloseq a phyloseq object.
#' @details This function is part of a package used for the analysis of microbial communities.
#' @examples
#' boxplot_alpha_diversity(mergedata, o, "SampleType", "SampleType", "Simpson", "Host")
#' @export
boxplot_alpha_diversity<-function(objPhyloseq, orden, xFactor, colFactor, divMeasure, wrap){
  objPhyloseq@sam_data$SampleType <- factor(objPhyloseq@sam_data$SampleType, levels=orden)
  lev<-levels(objPhyloseq@sam_data$SampleType)
  L.pair <- combn(seq_along(lev),2,simplify = FALSE, FUN = function(i) lev[i])
  pdf(paste0("Figure_", divMeasure, ".pdf"), width=15, height=15)
p = phyloseq::plot_richness(objPhyloseq, x=xFactor, color=colFactor, measures=divMeasure)
q <- p + ggplot2::geom_boxplot()  + ggplot2::facet_wrap(wrap) +
  ggplot2::stat_summary(fun.data="mean_sdl", fun.args = list(mult=1),
                 geom="errorbar", width=0.2) +
  ggplot2::stat_summary(fun.y=mean, geom="point") +
  ggpubr::stat_compare_means(comparisons = L.pair, label = "p.signif", symnum.args = list( cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("****", "***", "**", "*", "N.S", "N.S"))) +
  ggplot2::theme_classic()
print(q)
dev.off()
}
