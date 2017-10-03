#' Title
#'
#' @param N nas
#' @param lambda1 nas
#' @param lambda2 nas
#' @param l1 nas
#' @param l2 nas
#' @param mu1 nas
#' @param mu2 nas
#' @param kappa1 nas
#' @param kappa2 nas
#'
#' @return
#' @export
#'
#' @examples
two.fam.multiplesim<-function(N,lambda1,lambda2,l1,l2,mu1,mu2,kappa1,kappa2){

  final.sim<-mat.or.vec(3,9)

  #progress.bar <- txtProgressBar(min = 0, max = N, style = 3)

  for(i in 1:N)
  {current.sim<-as.matrix(two.fam.singlesim(lambda1,lambda2,l1,l2,mu1,mu2,kappa1,kappa2))
  final.sim=final.sim+current.sim
  #setTxtProgressBar(progress.bar, i)
  }
  return(final.sim/N)

}
