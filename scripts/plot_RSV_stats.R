#' Violin RSV Distribution Plot
#' 
#' Violin plot for displaying the number of RSVs per a sample variable
#'
#' @import ggplot2
#' @import phyloseq
#' @export
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @examples
#' require(phyloseq)
#' require(ggplot2)
#' 
#' 
#' # Summary plots for RSV
plot_RSV_stats <- function(physeq){

  readsumsdf = data.frame(nreads = sort(taxa_sums(ps0), TRUE), 
                          sorted = 1:ntaxa(ps0),
                          type = "RSVs")
  
  
  # Add a column of sample sums (total number of individuals per sample)
  readsumsdf = rbind(readsumsdf,
                     data.frame(nreads = sort(sample_sums(ps0), TRUE),
                                sorted = 1:nsamples(ps0),
                                type = "Samples"))
  
  # Make a data frame with a column for the read counts of each sample for histogram production
  sample_sum_df <- data.frame(sum = sample_sums(ps0))
  
  # Make plots
  # Generates a bar plot with # of reads (y-axis) for each taxa. Sorted from most to least abundant
  # Generates a second bar plot with # of reads (y-axis) per sample. Sorted from most to least
  p.reads = ggplot(readsumsdf, aes(x = sorted, y = nreads)) +
    geom_bar(stat = "identity") +
    ggtitle("RSV Assessment") +
    scale_y_log10() +
    facet_wrap(~type, scales = "free") +
    ylab("# of Reads")
  
  # Histogram of the number of Samples (y-axis) at various read depths
  p.reads.hist <- ggplot(sample_sum_df, aes(x = sum)) + 
    geom_histogram(color = "black", fill = "firebrick3", binwidth = 2500) +
    ggtitle("Distribution of sample sequencing depth") + 
    xlab("Read counts") +
    ylab("# of Samples")
  
  # Final plot, side-by-side
  p <- grid.arrange(p.reads, p.reads.hist, ncol = 2)
  
}