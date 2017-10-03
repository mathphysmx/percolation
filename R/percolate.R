#' @title Function to test for horizontal AND vertical percolation
#'
#' @description Function to test for horizontal AND vertical percolation
#'
#' @param x A data.frame with 4 colums of numeric vectors: x0, y0, x1, y1.
#' These vector are the coordinates of line segments ends. See \link[graphics]{segments}.
#' @param xlim Numeric 2-dimensional vector with the limits along the abscissas.
#' @param ylim Numeric 2-dimensional vector with the limits along the ordinates.
#'
#' @return a list with:
#' p a data.frame in which each row represents a cluster,
#' the column H with value 1 if the cluster percolates Horizontally and a colum V.
#' c connectivity data.
#' g a nrow(x)-dimensional integer vector with the clusters/groups to which line segment belongs to.
#' @importFrom "igraph" "graph.adjacency","clusters"
#' @export
#'
#' @examples
#' x <- data.frame(x0=c(0,2,3,1),
#'                 y0=c(1,0,2,1.5),
#'                 x1=c(3,5,7,2),
#'                 y1=c(0,3,2,1.5))
#' xlims <- c(1,6); ylims <- c(-1,4)
#' plotSegments(x, ylim = ylims)
#' text(x[, 1:2], labels=1:4, pos = 2)
#' abline(h=ylims, v=xlims, col = 'gray')
#' pr <- percolate(x, xlim = xlims, ylim = ylims)
#' g1 <- which(pr$p[, 1] == 1) # cluster ID
#' selectCluster <- pr$g==g1
#' segments(x0= x[selectCluster,1], y0=x[selectCluster,2],
#'         x1= x[selectCluster,3], y1=x[selectCluster,4],
#'         col = "green")
percolate <- function(x, xlim, ylim){
  # Edges at the boundary
  interH <- touchBoundary(x[,1], x[,3], xlim)
  interV <- touchBoundary(x[,2], x[,4], ylim)

  # Graph theory
  in.mat<-incidence.matrix(x)
  # Feed the incidence matrix to igraph:
  G<-igraph::graph.adjacency(in.mat)
  # Use igraph's "cluster" function to find and store clusters:
  clust<-igraph::clusters(G)
  # Create a matrix to store percolation information and cluster sizes.
  percolation.info<-cbind(mat.or.vec(clust$no,2),clust$csize)
  colnames(percolation.info)<-c("H","V","Size")
  rownames(percolation.info)<-c(1:clust$no)

  # Check for horizontal percolation:
  hp <- breakThrough(interH, xlim, clust)
  percolation.info[, 1] <- hp
  #Check for vertical percolation:
  vp <- breakThrough(interV, ylim, clust)
  percolation.info[, 2] <- vp

  # Add connectivity information:
  connectivity.info<-c(length(igraph::articulation.points(G)),
                        igraph::graph.cohesion(G))
  return(list(p=percolation.info,c=connectivity.info, g=clust$membership))
}
