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
#' data("soilrep")
#' head( sample_data(soilrep) )
#' plot_violin(physeq = soilrep, x = "warmed")
#' 
#' plot_violin(physeq, x)

plot_violin <- function(physeq, x, y, title = NULL){
  ss <- sample_sums(physeq) # sample sums
  sd <- as.data.frame(sample_data(physeq)) # sample data
  df <- merge(sd, data.frame("RSVs" = ss), by = "row.names") # merge
  
  #Violin + Jitter plots
  p <- ggplot(df, aes_string(x, y="RSVs", fill = x)) +
    geom_violin() +
    scale_y_log10() +
    geom_hline(yintercept = y, lty = 2) +
    geom_jitter(alpha=0.5, width = 0.15) +
    geom_text(data = subset(df, RSVs <= y), aes_string(x,y="RSVs", label="Description"), size=2)
  if (!is.null(title)) {
    p <- p + ggtitle(title)
}
return(p)
}
