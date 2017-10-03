#' @title Verify if 2 line segments intersect
#'
#' @description Verify if 2 line segments intersect
#'
#' @details
#' This function determines whether two fractures intersect, using the criteria of
#' opposing orientations in the two-dimensional case. Since it is specifically desinged
#' to work with the rest of the functions in the model, it takes as parameter the tags
#' of the fractures which correspond to the row they occupy in the endpoints matrix, as
#' well as the matrix itself. It doesn't consider cases of colinearity, since given the
#' measures defined the probability of this eventuality is zero.
#'
#' @param a ID of the matrix.
#' @param b don't know yet.
#' @param m Matrix with four columns where the first and last two entries of every row are the cartesian coordinates of a single fracture's endpoints.
#'
#' @return the results.
#' @export
#'
inter.check<-function(a,b,m){

  c1<-as.vector(m[a,])
  p1<-c1[1:2]
  p1<-as.vector(p1)
  q1<-c1[3:4]
  q1<-as.vector(q1)

  c2<-as.vector(m[b,])
  p2<-c2[1:2]
  p2<-as.vector(p2)
  q2<-c2[3:4]
  q2<-as.vector(q2)

  if(orientation(p1, q1, p2)!= orientation(p1, q1, q2)
     && orientation(p2, q2, p1)!=orientation(p2, q2, q1)) return(1)
  else
    return(0)
  }
