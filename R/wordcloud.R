#' Plotting wordcloud on DCA axes
#'
#' This function plots the most representatice words of each cluster on the first two DCA axes.
#' Words are colored by cluster memebership and positioned according to their DCA coordinates.
#' Clustering is performed internally using Ward's method, and words are selected based on their
#' indicator values and p-values from \code{\link[labdsv]{indval}}.
#'
#' @param x Data frame. The abundance table with words as rows and articles as columns.
#'  All columns must be numeric. The table should not be scaled or normalized beforehand.
#'  It is recommended to apply an incidence threshold using
#'  \code{\link{optimal_incidence}} before passing the table to this function.
#' @param k Integer. The number of cluster
#' @param model A \code{decorana} object produced by \code{\link{run_dca}}.
#' @param wordnumber Integer. Number of words per cluster to be displayed. Default is 5.
#' @param colores TCharacter vector. Colors for each cluster. Must have at least \code{k} elements.
#'  If \code{NULL} (default), a built-in palette is used.
#' @param newcoords Names list. Words to manually reposition in the plot.
#'  Format: \code{list(word1 = list(x = 1.0, y = 0.5), word2 = list(x = -0.3, y = 1.2))}.
#'  Default is an empty list.
#' @param cex Numeric. Font size of the words. Default is 0.8.
#' @param pvalue Numeric. Maximum p-value threshold for fitting words from the indicator analysis.
#'  Default is 0.05.
#' @param abmean Numeric. Minimum mean abundance indicator for a word to be included. Default is 0.01.
#' @param xmin Numeric. Adjustment to the lower x-axis limit. Default is 0.
#' @param xmax Numeric. Adjustment to the upper x-axis limit. Default is 0.
#' @param ymin Numeric. Adjustment to the lower y-axis limit. Default is 0.
#' @param ymax Numeric. Adjustment to the upper y-axis limit. Default is 0.
#' @param font Integer. Font type of the words. See \code{\link[graphics]{par}} for details.
#'  Default is 2 (bold).
#' @importFrom cluster agnes
#' @importFrom stats cutree
#' @importFrom labdsv indval
#' @importFrom graphics text
#' @importFrom graphics par
#'
#' @returns A list with three elements:
#' \describe{
#'  \item{final_cluster}{Data frame with indicator values, p-values, and DCA
#'  coordinates for all words that passed the filtering criteria.}
#'  \item{subsets}{List of data frames, one per cluster, containing the top
#'  \code{wordnumber} words for that cluster.}
#'  \item{word}{The last word plotted (retained for internal use).}
#' }
#'
#' @seealso \code{\link{run_dca}}, \code{\link{ellipsis_plot}}, \code{\link{wordendo}}
#'
#' @export
#'
#' @examples
#' \donttest{
#' x <- data.frame(
#'   art1 = c(5, 4, 3, 0, 0, 0, 1),
#'   art2 = c(4, 5, 4, 0, 1, 0, 1),
#'   art3 = c(3, 3, 5, 1, 0, 0, 0),
#'   art4 = c(0, 0, 1, 5, 4, 3, 1),
#'   art5 = c(0, 1, 0, 4, 5, 4, 1),
#'   art6 = c(1, 0, 0, 3, 3, 5, 0)
#' )
#' rownames(x) <- paste0("word", 1:7)
#' model <- run_dca(x)
#' wc <- wordcloud(x, k = 2, model = model, wordnumber = 3, pvalue = 1, abmean = 0)
#' }
#'
wordcloud <- function(x, k, model, wordnumber = 5, colores = NULL, newcoords = list(), cex = 0.8,
                      pvalue = 0.05, abmean = 0.01, xmin = 0, xmax = 0, ymin = 0, ymax = 0, font = 2){

  # Default palette
  if (is.null(colores)){
  colores <- c("#FF0000","#008000","#800000","#0000FF","#9400D3","#FF00FF","#FFD700",
               "#7FFF00","#FFA500","#000080","#A52A2A","#00CED1","#8B4513","#DC143C",
               "#DA70D6","#D2691E","#00CED1","#9400D3","#ADFF2F","#FF69B4","#CD5C5C")
  }

  ## Convert to relative frequencies per article (per 1000 words)
  pa <- (x > 0) * 1
  sites_length <- unname(colSums(pa, na.rm = TRUE))
  abundance <- sweep(x, 2, sites_length, "/")
  abundance <- abundance * 1000

  # Log-transform and transpose (acticles as rows)
  abundancet <- log(t(abundance)+1)

  set.seed(123)
  modelclust <-agnes(abundancet,method="ward")
  cutmodel <- cutree(modelclust,k=k)
  temp5 <- indval(abundancet,cutmodel,numitr=1000)

  nk <- k

  # Combine indicator values with DCA coordinates
  achsen <- model$cproj[,c(1:2)]
  cluster_dca <- cbind(round(temp5$indval, digits = 3), temp5$pval, names(temp5$pval), achsen)

  # Filter by p-value and minimum mean abundance indicator
  cluster_dcap <- cluster_dca[cluster_dca[nk + 1] < pvalue,]
  final_cluster <- cluster_dcap[which(apply(cluster_dcap[, 1:nk], 1, mean) > abmean), ]
  names(final_cluster)[names(final_cluster) == "temp5$pval"] <- "pvalue"

  par(xpd = TRUE)
  par(mar=c(2, 2, 0.1, 0.1))
  plot(final_cluster$DCA1, final_cluster$DCA2,
       xlim = c(min(final_cluster$DCA1) + xmin, max(final_cluster$DCA1) - xmax),
       ylim = c(min(final_cluster$DCA2) + ymin, max(final_cluster$DCA2) - ymax),
       xlab = "DCA1", ylab = "DCA2", type = "n")
  points(0, 0, pch = 16, col = "black", cex = 0.8)

  # Select top words per cluster, sorted by indicator value then p-value
  subsets <- list()
  for (i in 1:nk){
    subsets[[i]] <- final_cluster[order(-final_cluster[, i], final_cluster$pvalue)[1:wordnumber], ]
    subsets[[i]]$cluster <- i
  }

  # Plot words, using manual coordinates if provided
  for (i in 1:nk) {
    for (j in 1:wordnumber) {
      word <- rownames(subsets[[i]])[j]
      if (word %in% names(newcoords)) {
        text(newcoords[[word]]$x, newcoords[[word]]$y,
             labels = word, col = colores[i], cex = cex, font = font)
      } else {
        text(subsets[[i]]$DCA1[j], subsets[[i]]$DCA2[j],
             labels = word, col = colores[i], cex = cex, font = font)
      }
    }
  }

  return(list(final_cluster = final_cluster, subsets = subsets, word = word))
}
