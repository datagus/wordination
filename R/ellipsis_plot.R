#----getting clusters in ellipsis----

#' Creating an ellipsis plot
#'
#' This function returs the same word cloud but with the ellipsis around them, showing the sizes and overalps between clusters.
#'
#' @param subsets Object retrieved from the wordcloud() function
#' @param final_cluster the Object retrived from the wordcloud()function
#' @param colores The vector of colors. The length of this vector should be at least the same as the number of cluster. This vector has to be the same for the barplot, wordcloud and dendogram.
#' @param withborder boolean. If you want that the ellipsis have a border
#' @param showWords boolean. If you want only the elipsis to appear without the words printed, put FALSE (default).
#' @param newcoords A list of of the words you want to reposition in the word cloud. You have to give a list in the following format:
#' list(word=list(x=x,y=y))
#' @param wordnumber The number of words to appear in each cluster
#' @param cex The size of the words
#' @param font Integer. The type of font you want to apply to the words. See ?par for more information.
#' @param opacity How transparent you want to clusters to be. Range from 0.0 to 1.0, being 0.0 complete transparent.
#' @param coverage This parameter indicates how big the ellipsis are. Statistically, coverage means how much of the total words in each cluster should be considered.
#' @param group_labels the labels you want to put
#' @param with_labels booloean. If you want the labels printed in rectangle boxes,but this require manual adjustment.
#' @param colbox_text The color of the text inside the label box.
#' @param labelx Vector indicating the coordinates of the labels along the x axis.
#' @param labely Vector indicating the coordinates of the labels along the y axis.
#' @param adjx_max Numeric. Adjustment to the upper x-axis limit.
#' @param adjx_min Numeric. Adjustment to the lower x-axis limit.
#' @param adjy_max Numeric. Adjustment to the upper y-axis limit.
#' @param adjy_min Numeric. Adjustment to the lower y-axis limit.
#' @param size_text_label Numeric. Font size for labels in boxes.
#' @param width_label Numeric. Factor applied to text width when drawing label boxes.
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
#' @returns An ellipsis plot
#' @export
ellipsis_plot <- function(subsets,final_cluster,colores=c(),withborder=FALSE, showWords=FALSE,
                          newcoords=list(),wordnumber=5, cex=1, font=2, opacity=0.8,  coverage = 0.95, with_labels=FALSE, group_labels=NULL,
                          colbox_text="black", labelx=c(), labely=c(), adjx_max=0, adjx_min=0, adjy_max=0,adjy_min=0, size_text_label=0.7, width_label=0.3){

  #colores <- c("#FF0000","#008000","#800000","#0000FF","#9400D3","#FF00FF","#FFD700",
   #            "#7FFF00","#FFA500","#000080","#A52A2A","#00CED1","#8B4513","#DC143C",
  #             "#DA70D6","#D2691E","#00CED1","#9400D3","#ADFF2F","#FF69B4","#CD5C5C"
  #)

  cc <- do.call(rbind, subsets) #concatenating all the dataframes in the subsets list
  cc2 <- cc[,c((ncol(cc)-2):ncol(cc))] #getting the columns
  coordinates <- cc2[, c("DCA1", "DCA2")]
  clusteros <- cc2$cluster
  row.names(coordinates)<-row.names(cc2)

  par(xpd=F)
  #par(mar=c(2,2,0.5,0.5))
  par(mar=c(4,4,0.5,0.5))
  plot(final_cluster$DCA1,final_cluster$DCA2,xlim=c(min(final_cluster$DCA1)+adjx_min,max(final_cluster$DCA1)+adjx_max),ylim=c(min(final_cluster$DCA2)+adjy_min,max(final_cluster$DCA2)+adjy_max),xlab="DCA1",ylab="DCA2",type="n", axes=TRUE)

  if(isTRUE(showWords)){
    for (i in 1:length(subsets)){
      for (j in 1:wordnumber){
        word <- rownames(subsets[[i]])[c(j)]
        if (word %in% names(newcoords)){
          x <- newcoords[[word]]$x
          y <- newcoords[[word]]$y
          text(x,y, labels=word,col=colores[i],cex=cex, font=font)
        }
        else{
          text(subsets[[i]]$DCA1[c(j)],subsets[[i]]$DCA2[c(j)], labels=word,col=colores[i],cex=cex, font=font)
        }
      }
    }
  }
  set.seed(123)
  ##---function to calculate the angle of ellipsis using covariance matrix---
  compute_angle <- function(cov_matrix) {
    eig <- eigen(cov_matrix)
    # Eigenvector corresponding to the largest eigenvalue
    max_eigenvector <- eig$vectors[, 1]
    # Calculate the angle in degrees
    angle <- atan2(max_eigenvector[2], max_eigenvector[1]) * 180 / pi
    return(angle)
  }

  coverage <- coverage
  scale <- sqrt(qchisq(coverage, df = 2))

  for (i in unique(clusteros)) {
    # Get points belonging to the current cluster
    cluster_points <- coordinates[clusteros == i, ]
    cov_matrix <- cov(cluster_points[, c("DCA1", "DCA2")])
    angle <- compute_angle(cov_matrix)

    # Compute the mean (center) of the current cluster
    center_x <- mean(cluster_points$DCA1)
    center_y <- mean(cluster_points$DCA2)

    # Draw an ellipse around the cluster points with the desired color
    color_with_alpha <- rgb(t(col2rgb(colores[i])) / 255, alpha = opacity)
    if(isTRUE(withborder)){border=colores[i]
    }else{border=color_with_alpha}
    draw.ellipse(x = center_x, y = center_y, a = sd(cluster_points$DCA1)*scale, b = sd(cluster_points$DCA2)*scale,
                 border=border,col=color_with_alpha, lwd = 1, angle=angle)

    if (with_labels==TRUE){
      # Add highlighted text label
      label <- group_labels[i]

      # Calculate label position and dimensions
      text_x <- labelx[i]
      text_y <- labely[i]
      label_width <- strwidth(label) * width_label
      label_height <- strheight(label) * 1


      rect(text_x - label_width, text_y - label_height, text_x + label_width, text_y + label_height,
           col = color_with_alpha, lwd = 0.5)

      # Add the text label in the center of the rectangle
      text(text_x, text_y, label, col = colbox_text, cex = size_text_label, font = 3)
    }
  }

  return(list(cc=cc,
              cc2=cc2,coordinates=coordinates,
              clusteros=clusteros,
              cluster_points=cluster_points
  ))
}

#' @examples to be added
