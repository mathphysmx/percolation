#' @title Says if there is a connected path between the boundary limits
#'
#' @description Says if there is a connected path between the boundary limits along a given direction. Two posible directions:vertical or horizontal. Not both.
#'
#' @param bi List with 2 integer vectors wm and wM. ID of line segments touching the study interval boundary. The result from \link{touchBoundary}.
#' @param lims Numeric 2-dimensinal vector with the limits of the study interval.
#' @param clust a result from \link[igraph]{graph.adjacency}.
#'
#' @return A logical vector of length equals to the number of clusters. Each component answering the question: There is a connected path from \code{lims[1]} to \code{lims[2]}?
#' @export
#' @author Daniella Ayala (\email{daniellaayala@ciencias.unam.mx}) and Francisco Mendoza-Torres (\email{mentofran@@gmail.com})
#' @examples
#' n <- 6; xl <- -0.3; xr <- 1.2
#' set.seed(123) #9
#' endpts <- data.frame(x0 = runif(n, xl, xr),
                     #' y0 = runif(n, xl, xr),
                     #' x1 = runif(n, xl, xr),
                     #' y1 = runif(n, xl, xr))
#' plotSegments(segment = endpts)
#' lims <- c(0,1)
#' abline(h=lims, v= lims, col = 'lightgray')
#' te <- touchBoundary(w0 = endpts$x0, w1 = endpts$x1, lims = lims)
#' library(igraph)
#' eAdjClust <- clusters(graph.adjacency(incidence.matrix(endpts)))
#' breakThrough(bi=te, lims=0:1, clust = eAdjClust)
breakThrough <- function(bi, lims, clust){
	percol <- rep(FALSE, clust$no)
	if(length(bi$wm)>0||length(bi$wM)>0){
	  for(i in 1:clust$no){
		  # i <- 1
		  current.cluster=which(clust$membership==i)
		  li <- intersect(current.cluster,bi$wm)
		  ui <- intersect(current.cluster,bi$wM)
		  if(length(li)>0 && length(ui)>0)
			  percol[i] <- TRUE
	  }
	}

	return(percol)
}

