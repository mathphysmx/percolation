#' @title Single family simulation
#'
#' @details
#' This function simulates a fracture network in a 1x1 window, and checks whether the
#' network percolates or not. In this version, the lenghts of the fractures is fixed.
#' This version of the code DOES NOT implement the previous data treatment discussed
#' (eliminating isolated fractures and fractures within the network's body with a
#' connectivity degree less than two). The user must specify the following parameters:
#'
#' These are the necessary libraries:
#' library(circular)
#' library(igraph)
#'
#' @param lambda Numeric. Intensity of the Poisson point process that determines both the number and the location of the fractures (lambda).
#' @param l Numeric. Constant length of the fractures (l).
#' @param mu Numeric. Mean direction (in radians) of the Von Mises distribution of angles.
#' @param kappa Numeric. Non-negative scalar representing the von Mises concentration parameter.
#'
#' @return
#' @export
#'
#' @examples
lambda <- 6
l <- 5
mu <- pi/2; kappa <- 10

n <- 10; xl <- -0.5; xr <- 1.5
ends0<-matrix(c(runif(n, xl, xr),runif(n,xl, xr)),ncol=2) # midpoint locations
ends1<-matrix(c(runif(n,xl, xr),runif(n,xl, xr)),ncol=2) # midpoint locations
endpts <- data.frame(ends0, ends1)
names(endpts) <- c('x0', 'y0', 'x1', 'y1')
source('/media/paco/myhdd/soft/dev/R/Rfunctions/plotSegments.R')
plotSegments(segment = endpts)
abline(h=c(0,1), v= c(0,1), col = 'lightgray')

single.sim.nopb<-function(lambda,l,mu,kappa){


  # Generate the fracture data:

  n<-rpois(1,lambda) # number of fractures
  locs<-matrix(c(runif(n,0,1),runif(n,0,1)),ncol=2) # midpoint locations
  lengths<-rep(l,n) # constant length vector
  angles<-rvonmises(n,circular(mu),kappa) # angle vector


  # Use the previous data to calculate the endpoints of every fracture:
  endpts<<-suppressWarnings(polar.to.rect(n,locs,lengths,angles))

  # Identify the fractures that intersect the boundary.
  source('/media/paco/myhdd/soft/dev/R/sourceDir.R')
  sourceDir('/media/paco/myhdd/soft/dev/R/Rfunctions')
  plotSegments(endpts); grid()
  # Edges for vertical percolation.
  interV <- touchBoundary(endpts[,2], endpts[,4])
  lower.edge<-which(endpts[,2]<0|endpts[,4]<0)
  upper.edge<-which(endpts[,2]>1|endpts[,4]>1)

  # Edges for horizontal percolation.
  interH <- touchBoundary(endpts[,1], endpts[,3])
  left.edge<-which(endpts[,1]<0|endpts[,3]<0)
  right.edge<-which(endpts[,1]>1|endpts[,3]>1)

  # Build the incidence matrix:
  in.mat<-incidence.matrix.nopb(endpts)


  # Feed the incidence matrix to igraph:
  library(igraph)
  G<-graph.adjacency(in.mat)


  # Use igraph's "cluster" function to find and store clusters:
  C<-clusters(G)
  clust<<-as.vector(clusters(G)[[1]])
  clust.num<<-C[[3]]

  # Create a matrix to store percolation information and cluster sizes.
  percolation.info<-cbind(mat.or.vec(clust.num,2),C[[2]])
  colnames(percolation.info)<-c("V","H","Size")
  rownames(percolation.info)<-c(1:clust.num)

  # Check for vertical percolation:
  if(length(left.edge)>0||length(right.edge)>0){

    for(i in 1:clust.num){

      current.cluster=which(clust==i)

      if(length(intersect(current.cluster,left.edge))>0

         && length(intersect(current.cluster,right.edge))>0) percolation.info[i,1]<-1

    }

  }

  #Check for horizontal percolation:
  if(length(lower.edge)>0||length(upper.edge)>0){

    for(i in 1:clust.num){

      current.cluster=which(clust==i)

      if(length(intersect(current.cluster,lower.edge))>0

         && length(intersect(current.cluster,upper.edge))>0) percolation.info[i,2]<-1

    }

  }

  # Add connectivity information:

  connectivity.info<<-c(length(articulation.points(G)),graph.cohesion(G))

  percolation.info<<-percolation.info

  return(list(percolation.info,connectivity.info))  }
