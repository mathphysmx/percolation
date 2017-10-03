#' @title indicator function of an interval in the real line
#'
#' @description indicator function of an interval in the real line
#'
#' @param x numeric vector of values to be tested.
#' @param min numeric, scalar lower bound. -\code{Inf} is a valid value.
#' @param max numeric, scalar upper bound. \code{Inf} is a valid value.
#' @param liq character. Left inequality. ">" or ">=".
#' @param riq characher. Right inequality. "<" or "<=".
#'
#' @return a logical vector with \link[base]{TRUE} when \code{x[i]} is in the interval given by the parameters of this function. Logical values were used for memory efficiency when large scale usage of this function.
#' @export
#' @author Francisco Mendoza-Torres (\email{mentofran@@gmail.com})
#'
#' @examples
#' xe <- c(-Inf, -3, 2, 6, Inf)
#' 1 * indicator(x = xe, min = 2, max = 6, liq = ">=", riq = "<=")
#'     indicator(x = xe, min = 2, max = 6, liq = ">=", riq = "<=")
#' indicator(x = xe, min = 2, max = 6, liq = ">" , riq = "<=")
#' indicator(x = xe, min = 2, max = Inf, liq = ">=", riq = "<")
#' indicator(x = xe, min = 2, max = Inf, liq = ">=", riq = "<=")
#'
#' # Also works in the case that x is unique and min and max are vectors
#' xl <- c(-1, 0.3, -4)
#' xu <- c( 1,   1, -2)
#' indicator(0, xl, xu)
## todo:
# answer:
#http://stackoverflow.com/questions/20096514/indicator-function-in-r
#http://stackoverflow.com/questions/22514243/writing-an-indicator-function-in-r
indicator <- function(x, min, max,
		      liq = ">=", riq = "<="){
	# (x > min)&(x < min)
	ind <- (get(liq)(x, min) & get(riq)(x, max))
	return(ind)
}

