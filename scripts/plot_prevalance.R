#' Prevelance distribution plots
#' 
#' Prevelance distribution plots for displaying
#' the prevleance and abundance of each phyla ~ family
#' 
#' @import ggplot2
#' @import phyloseq
#' @export
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @examples

plot_prevalence <- function(physeq){

prevdf <- apply(X = otu_table(physeq),MARGIN = ifelse(taxa_are_rows(physeq), yes = 1, no = 2),FUN = function(x){sum(x > 0)})

# Add taxonomy and total read counts to prevdf
prevdf <- data.frame(Prevalence = prevdf, TotalAbundance = taxa_sums(physeq), tax_table(physeq))

# Create a table of Phylum, their mean abundances across all samples, and the number of samples they were detected in
plyr::ddply(prevdf, "Phylum", function(df1){cbind(mean(df1$Prevalence),sum(df1$Prevalence))})

#Prevalence plot
prevdf1 <- subset(prevdf, Phylum %in% get_taxa_unique(ps0, "Phylum"))
p.prevdf1 <- ggplot(prevdf1, aes(TotalAbundance, Prevalence / nsamples(physeq),color=Family)) +
  geom_hline(yintercept = 0.05, alpha = 0.5, linetype = 2) +
  geom_point(size = 3, alpha = 0.7) +
  scale_x_log10() +
  xlab("Total Abundance") + ylab("Prevalence [Frac. Samples]") +
  facet_wrap(~Phylum) +
  theme(legend.position="none") +
  ggtitle("Phylum Prevelence in All Samples\nColored by Family")
p.prevdf1
}