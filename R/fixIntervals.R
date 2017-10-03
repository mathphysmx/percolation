#' @title Fix interval to be increasingly ordered
#'
#' @description Fix interval to be increasingly ordered
#'
#' @param x0 Numeric vector. Left value of the interval (pair coordinates).
#' @param x1 Numeric vector. Right value of the interval (pair coordinates).
#'
#' @return A data.frame with columns \code{xl} and \code{xu}, the lower and upper values. Each row is sorted increasingly.
#' @export
#'
#' @examples
#' xl <- c(-1, 0.3, -4,  2)
#' xu <- c( 1,   1, -2, -3)
#' fixIntervals(xl, xu)
#'
fixIntervals <- function(x0, x1){
	dfx <- data.frame(x0, x1)
	reX <- (x0>x1)
	dfx[reX,] <- rev(dfx[reX,])
	names(dfx) <- c('xl', 'xu')
	return(dfx)
}

