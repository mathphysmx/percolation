#' @title Incidence matrix generation
#'
#' @description Incidence matrix generation
#'
#' @details
#' This functions calculates the incidence matrix of a set of line segments (in the graph theory sense).
#' It takes as entry a matrix with four columns where the first and last two entries of every row are the cartesian coordinates of a single segment's endpoints (see \link[graphics]{segments}). Thus, the number of rows corresponds to the number of line segments. This function depends on the function \link{inter.check} to determine whether two fractures intersect.
#' Interpreting this network as a non-oriented random graph, where every line segment is a node
#' and two nodes are connected if the corresponding line segments intersect, incidence.matrix()
#' builds the symmetrical ones-and-zeroes matrix of adjacency. This matrix is later fed to
#' the igraph package for further analysis.
#'
#' @inheritParams inter.check
#'
#' @return  A symmetric matrix of ones and zeroes, where a one (1)
#' in any given entry indicates that the fractures corresponding to the entry's indices
#' intersect.
#' @export
#' @author Daniella Ayala (\email{daniellaayala@ciencias.unam.mx}) and Francisco Mendoza-Torres (\email{mentofran@@gmail.com})
#'
#' @examples
#' dfn <- data.frame(x0=c(0,2,3,1),
		  #' y0=c(1,0,2,1.5),
		  #' x1=c(3,5,7,2),
		  #' y1=c(0,3,2,1.5))
#' plotSegments(dfn)
#' text(dfn[, 1:2], labels=1:4, pos = 2)
#' incidence.matrix(dfn)
incidence.matrix<-function(m){

  n<-nrow(m)

  in.mat<-mat.or.vec(n,n)

  for(i in 1:(n-1)){
    for(j in (1+i):n){
      if(inter.check(i,j,m)==1)in.mat[i,j]<-1
    }
  }

  in.mat<-in.mat+t(in.mat)

  return(in.mat)
  }
