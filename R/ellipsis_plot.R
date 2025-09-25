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
#' @param wordnumber The number of words to appear in each cluster
#' @param cex The size of the words
#' @param opacity How transparent you want to clusters to be. Range from 0.0 to 1.0, being 0.0 complete transparent.
#' @param coverage This parameter indicates how big the ellipsis are. Statistically, coverage means how much of the total words in each cluster should be considered.
#' @param group_labels the labels you want to put
#' @param with_labels booloean. If you want the labels printed in rectangle boxes,but this require manual adjustment.
#' @param colbox The color of the label box or rectangle
#' @param colbox_text The color of the text inside the label box.
#' @param labelx Vector indicating the coordinates of the labels along the x axis.
#' @param labely Vector indicating the coordinates of the labels along the y axis.

#' @importFrom grDevices rgb
#' @importFrom plotrix draw.ellipse
#' @importFrom graphics strwidth
#' @importFrom graphics strheight
#' @importFrom graphics rect
#' @importFrom graphics text
#'
#' @returns An ellipsis plot
#' @export
ellipsis_plot <- function(subsets,final_cluster,colores=colores,withborder=FALSE, showWords=FALSE, wordnumber=5, cex=1, opacity=0.8,  coverage = 0.95, with_labels=FALSE,group_labels=NULL,  colbox_text="black", labelx=c(), labely=c()){

  colores <- c("#FF0000","#008000","#800000","#0000FF","#9400D3","#FF00FF","#FFD700",
               "#7FFF00","#FFA500","#000080","#A52A2A","#00CED1","#8B4513","#DC143C",
               "#DA70D6","#D2691E","#00CED1","#9400D3","#ADFF2F","#FF69B4","#CD5C5C"
  )

  cc <- do.call(rbind, subsets) #concatenating all the dataframes in the subsets list
  cc2 <- cc[,c((ncol(cc)-2):ncol(cc))] #getting the columns
  coordinates <- cc2[, c("DCA1", "DCA2")]
  clusteros <- cc2$cluster
  row.names(coordinates)<-row.names(cc2)

  par(xpd=T)
  par(mar=c(2,2,0.5,0.5))
  plot(final_cluster$DCA1,final_cluster$DCA2,xlim=c(min(final_cluster$DCA1)-1,max(final_cluster$DCA1)+1),ylim=c(min(final_cluster$DCA2)-1,max(final_cluster$DCA2)+1),xlab="DCA1",ylab="DCA2",type="n")

  if(isTRUE(showWords)){
    for (i in 1:length(subsets)){
      xadj <- 0
      yadj <- 0
      for (j in 1:wordnumber){
        text(subsets[[i]]$DCA1[c(j)]-xadj,subsets[[i]]$DCA2[c(j)], labels=rownames(subsets[[i]])[c(j)],col=colores[i],cex=cex, font=2)
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
      label_width <- strwidth(label) * 0.8
      label_height <- strheight(label) * 1


      rect(text_x - label_width, text_y - label_height, text_x + label_width, text_y + label_height,
           col = color_with_alpha, lwd = 0.5)

      # Add the text label in the center of the rectangle
      text(text_x, text_y, label, col = colbox_text, cex = 0.7, font = 3)
    }
  }

  return(list(cc=cc,
              cc2=cc2,coordinates=coordinates,
              clusteros=clusteros,
              cluster_points=cluster_points
  ))
}

#' @examples to be added
