#' Plot ellipses around word clusters
#'
#' This function overlays ellipses on the DCA word cloud to visualize cluster sizes and overlaps.
#' Each ellipse is drawn around the words belonging to a cluster based on their covariance structure.
#' The ellipse orientation and size are derived from the eigenvalues of the covariance matrix of
#' the DCA coordinates.
#'
#' @param subsets List of data frames. The \code{$subsets} output from \code{\link{wordcloud}}, containing
#'  the top words per cluster with DCA coordinates.
#' @param final_cluster Data frame. the \code{$final_cluster} output from \code{\link{wordcloud}}, containing
#'  all filtered words with DCA coordinates.
#' @param colores Character vector. Colors for each cluster. Must have have at least as many elements as the
#'  number of clusters.
#' @param withborder Logical. If \code{TRUE}, ellipses are drawn with a solid border in the cluster color.
#'  Default is \code{FALSE}.
#' @param showWords Logical. If \code{TRUE}, words are printed inside the ellipse. Default is \code{FALSE}.
#' @param newcoords Named list. Words to manually reposition in the plot.
#'  Format: \code{list(word1 = list(x = 1.0, y = 0.5))}. Default is an empty list.
#' @param wordnumber Integer. Number of words to be displayed per cluster. Default is 5.
#' @param cex Numeric. Font size of the words. Default is 1.
#' @param font Integer. Font type for the displayed words. See \code{\link[graphics]{par}} for details.
#'  Default is 2 (bold).
#' @param opacity Numeric. Transparency of the ellipses, from 0.0 (fully transparent) to 1.0 (fully opaque).
#'  Default is 0.8.
#' @param coverage Numeric. Controls how large the ellipses are. A higher value means the ellipse
#'  covers a greater proportion of the words in each cluster. Default is 0.95.
#' @param group_labels Character vector. Labels for each cluster. Used only if \code{with_labels = TRUE}.
#' @param with_labels Logical. If \code{TRUE}, cluster labels are displayed in colored boxes. Requires manual
#'  position adjustment via \code{labelx} and \code{labely}. Default is \code{FALSE}.
#' @param colbox_text Character. Color of the text inside label boxes. Default is \code{"black"}.
#' @param labelx Numeric vector.  X-coordinates for the label positions.
#' @param labely Numeric vector.  Y-coordinates for the label positions.
#' @param adjx_max Numeric. Adjustment to the upper x-axis limit. Default is 0.
#' @param adjx_min Numeric. Adjustment to the lower x-axis limit. Default is 0.
#' @param adjy_max Numeric. Adjustment to the upper y-axis limit. Default is 0.
#' @param adjy_min Numeric. Adjustment to the lower y-axis limit. Default is 0.
#' @param size_text_label Numeric. Font size for labels in boxes. Default is 0.7.
#' @param width_label Numeric. Factor applied to text width when drawing label boxes. Default is 0.3.
#' @importFrom grDevices rgb
#' @importFrom plotrix draw.ellipse
#' @importFrom graphics strwidth
#' @importFrom graphics strheight
#' @importFrom graphics rect
#' @importFrom graphics text
#' @importFrom graphics par
#' @importFrom graphics points
#' @importFrom stats cov
#' @importFrom stats qchisq
#' @importFrom stats sd
#' @importFrom grDevices col2rgb
#'
#' @returns A list with five elements:
#'  \describe{
#'    \item{cc}{Data frame. All cluster subsets combined row-wise.}
#'    \item{cc2}{Data frame. The DCA coordinatesand cluster assignments.}
#'    \item{coordinates}{Data frame. The DCA1 and DCA2 coordinates for each word.}
#'    \item{clusteros}{Integer vector. Cluster assignment for each word.}
#'    \item{cluster_points}{Data frame. Coordinates of the last cluster processed.}
#'  }
#'
#' @seealso \code{\link{wordcloud}}, \code{\link{wordendro}}
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(transformative_abundance)
#' model <- run_dca(transformative_abundance)
#'
#' #considering only five clusters
#' wc <- wordcloud(transformative_abundance, k = 5, model = model,
#'                 wordnumber = 5)
#'
#' # now only the elipse plot is displayed
#' ellipsis_plot(
#'   subsets = wc$subsets,
#'   final_cluster = wc$final_cluster, showWords = TRUE, opacity = 0.2
#' )
#' }
#'
ellipsis_plot <- function(subsets, final_cluster, colores = NULL, withborder = FALSE, showWords = FALSE,
                          newcoords = list(), wordnumber = 5, cex = 0.8, font = 2, opacity = 0.8,  coverage = 0.95, with_labels = FALSE, group_labels = NULL,
                          colbox_text = "black", labelx = c(), labely = c(), adjx_max = 0, adjx_min = 0, adjy_max = 0,adjy_min = 0, size_text_label = 0.7, width_label = 0.3){

  # Default palette
  if (is.null(colores)){
    colores <- c("#FF0000","#008000","#800000","#0000FF","#9400D3","#FF00FF","#FFD700",
                 "#7FFF00","#FFA500","#000080","#A52A2A","#00CED1","#8B4513","#DC143C",
                 "#DA70D6","#D2691E","#00CED1","#9400D3","#ADFF2F","#FF69B4","#CD5C5C")
  }

  # Combine all cluster subsets into one data frame
  cc <- do.call(rbind, subsets)
  cc2 <- cc[,c((ncol(cc)-2):ncol(cc))]
  coordinates <- cc2[, c("DCA1", "DCA2")]
  clusteros <- cc2$cluster
  row.names(coordinates)<-row.names(cc2)

  par(xpd = TRUE)
  par(mar=c(2, 2, 0.1, 0.1))
  padding_x <- diff(range(final_cluster$DCA1)) * 0.15
  padding_y <- diff(range(final_cluster$DCA2)) * 0.15
  plot(final_cluster$DCA1, final_cluster$DCA2,
       xlim = c(min(final_cluster$DCA1) + adjx_min, max(final_cluster$DCA1) + adjx_max),
       ylim = c(min(final_cluster$DCA2) + adjy_min, max(final_cluster$DCA2) + adjy_max),
       asp = 1,
       xlab = "DCA1", ylab = "DCA2", type = "n")

  # Optionally plot words inside ellipses
  if (isTRUE(showWords)) {
    for (i in 1:length(subsets)) {
      for (j in 1:wordnumber) {
        word <- rownames(subsets[[i]])[c(j)]
        if (word %in% names(newcoords)) {
          x <- newcoords[[word]]$x
          y <- newcoords[[word]]$y
          text(x, y, labels = word,col = colores[i], cex = cex, font = font)
        } else {
          text(subsets[[i]]$DCA1[c(j)], subsets[[i]]$DCA2[c(j)],
               labels = word, col = colores[i], cex = cex, font = font)
        }
      }
    }
  }
  set.seed(123)

  # Compute ellipse angle from covariance matrix eigenvectors
  #compute_angle <- function(cov_matrix) {
  #  eig <- eigen(cov_matrix)
  #  max_eigenvector <- eig$vectors[, 1]
  #  angle <- atan2(max_eigenvector[2], max_eigenvector[1]) * 180 / pi
  #  return(angle)
  #}

  # Scale factor based on chi-squared distribution for the desired coverage
  scale <- sqrt(qchisq(coverage, df = 2))

  # Draw an ellipse for each cluster
  for (i in unique(clusteros)) {
    cluster_points <- coordinates[clusteros == i, ]
    cov_matrix <- cov(cluster_points[, c("DCA1", "DCA2")])

    eig <- eigen(cov_matrix)
    a_len <- sqrt(max(eig$values, 0)[1]) * scale
    b_len <- sqrt(max(eig$values[2], 0)) * scale
    angle <- atan2(eig$vectors[2, 1], eig$vectors[1, 1]) * 180 / pi


    #angle <- compute_angle(cov_matrix)

    # Compute the mean (center) of the current cluster
    center_x <- mean(cluster_points$DCA1)
    center_y <- mean(cluster_points$DCA2)

    color_with_alpha <- rgb(t(col2rgb(colores[i])) / 255, alpha = opacity)
    if (isTRUE(withborder)){
      border=colores[i]
    } else {
      border=color_with_alpha
    }
    draw.ellipse(x = center_x, y = center_y,
                 a = a_len,
                 b = b_len,
                 border = border, col = color_with_alpha, lwd = 1, angle = angle)

    # Optionally add cluster labels in colored boxes
    if (with_labels==TRUE) {
      label <- group_labels[i]
      text_x <- labelx[i]
      text_y <- labely[i]
      label_width <- strwidth(label) * width_label
      label_height <- strheight(label) * 1

      rect(text_x - label_width, text_y - label_height,
           text_x + label_width, text_y + label_height,
           col = color_with_alpha, lwd = 0.5)

      # Add the text label in the center of the rectangle
      text(text_x, text_y, label, col = colbox_text, cex = size_text_label, font = 3)
    }
  }

  return(list(cc = cc,
              cc2 = cc2, coordinates = coordinates,
              clusteros = clusteros, cluster_points = cluster_points))
}
