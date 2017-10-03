#' @title Verify line segment intersection with a simple boundary
#'
#' @description Does the line segments touch the boundary? Verify line segment intersection with a simple boundary.
#'
#' @param w0,w1 Numeric vector. w stands for either x or y coordinate (of both ends) of the line segments.
#' @param lims Numeric 2 dimensional vector.
#'
#' @return List with vectors \code{wm}  and \code{wM} with values where the input touches the boundary.
#' @export
#' @author Daniella Ayala (\email{daniellaayala@ciencias.unam.mx}) and Francisco Mendoza-Torres (\email{mentofran@@gmail.com})
#'
#' @examples
#' n <- 10; xl <- -0.3; xr <- 1.2
#' set.seed(123) #9
#' endpts <- data.frame(x0 = runif(n, xl, xr),
		     #' y0 = runif(n, xl, xr),
		     #' x1 = runif(n, xl, xr),
		     #' y1 = runif(n, xl, xr))
#' plotSegments(segment = endpts)
#' lims <- c(0,1)
#' abline(h=lims, v= lims, col = 'lightgray')
#' # Check for intersection with vertical lines
#' te <- touchBoundary(w0=endpts$x0, w1 = endpts$x1, lims = lims)
#' # Intersection with the leftmost vertical boundary (x=constant)
#' segments(endpts$x0[te$wm], endpts$y0[te$wm],
	 #' endpts$x1[te$wm], endpts$y1[te$wm],
	 #' col = 'blue', lwd = 3)
#' # Intersection with the rightmost vertical boundary (x=constant)
#' segments(endpts$x0[te$wM], endpts$y0[te$wM],
	 #' endpts$x1[te$wM], endpts$y1[te$wM],
	 #' col = 'green', lwd = 3)
#'
#' # Intersection with horizontal lines
#' tey <- touchBoundary(w0=endpts$y0, w1 = endpts$y1, lims = lims)
#' # Intersection with the bottom-most horizontal boundary (y=constant)
#' segments(endpts$x0[tey$wm], endpts$y0[tey$wm],
	 #' endpts$x1[tey$wm], endpts$y1[tey$wm],
	 #' col = 'pink', lwd = 3)
#' # Intersection with the uppermost horizontal boundary (y=constant)
#' segments(endpts$x0[tey$wM], endpts$y0[tey$wM],
	 #' endpts$x1[tey$wM], endpts$y1[tey$wM],
	 #' col = 'red', lwd = 3)
touchBoundary <- function(w0, w1, lims=0:1){
	x <- fixIntervals(w0, w1)
	lower <- indicator(x = lims[1],
			   min = x[, 1], max = x[, 2],
			   liq = ">=", riq = "<=")
	upper <- indicator(x = lims[2],
			   min = x[, 1], max = x[, 2],
			   liq = ">=", riq = "<=")
	ID <- 1:length(w0)
  return(list(wm = ID[lower],wM = ID[upper]))
}
