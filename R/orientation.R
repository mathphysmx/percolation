#' @title Signed orientation
#'
#' @description Signed orientation
#'
#' @details
#' This function determines the orientation of three points in a plane.
#' Since these points are assumed to come from a random point process, the
#' colinearity case is not considered, since the probability of this eventuality
#' is certainly zero.
#'
#' @param p Numeric two-dimensional vector in cartesian coordinates.
#' @param q Numeric two-dimensional vector in cartesian coordinates.
#' @param r Numeric two-dimensional vector in cartesian coordinates.
#'
#' @return Eihter 1 (positive orientation) or -1 (negative orientation).
#' @export
#' @author Daniella Ayala (\email{daniellaayala@ciencias.unam.mx}) and Francisco Mendoza-Torres (\email{mentofran@@gmail.com})
#'
#' @examples
#' orientation(p=c(0,0), q=c(-1,0), r=c(0,1))
#' orientation(p=c(0,0), q=c( 1,0), r=c(0,1))

orientation<-function(p,q,r){

  k=(q[2] - p[2])*(r[1] - q[1])-(q[1] - p[1]) * (r[2] - q[2])

  if(k>0) return(1)

  else return(-1)
}
