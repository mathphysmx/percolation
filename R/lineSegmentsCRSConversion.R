#' @title Line segments Coordinate Reference System conversion
#' @description Line segments conversion from a angles-length-centers to endpoints coordinates
# c is the x position of the center of the line segment
# c is the y position of the center of the line segment
# a object of class circular (circular package).It is the  of the line segment 
# l is the length of the line segment
#' @references  equation 1.78, page 56 of D:/IMP/Transferencia/ReporteMetodologiasDFN_FmendozaT.pdf
#' @examples 
#' ae <- circular(c(0, 45, 90, 135),units="degrees",
#'                template="geographics")
#' le <- c(2, sqrt(2) * 3, 3, sqrt(2) * 3)
#' ends <- data.frame(x0 = c(0, 1, 1, -1),
#'                    y0 = c(1, 1, 0,  1),
#'                    x1 = c(0, 4, 4, -4),
#'                    y1 = c(3, 4, 0,  4)
#' )
#' xe <- data.frame(a = ae, l = le,
#'                    x = (ends$x0 + ends$x1) / 2,
#'                    y = (ends$y0 + ends$y1) / 2) 
#' plotSegments(
#'   segment=list( g1 = list(x0 = ends$x0, y0 = ends$y0,
#'                           x1 = ends$x1, y1 = ends$y1)),
#'   asp=1, main="Segments")
#' points.default(xe$x, xe$y, pch=20) # adding the midpoints to the current plot
#' print(ends)
#' lineSegmentsCRSConversion(x = xe)
lineSegmentsCRSConversion <- function(x){
  
  # From geographic to mathemical coordinate system
  a_rad <- conversion.circular(x = x$a, type = "angles",
                                       units = "radians", modulo = "pi",
                                       zero = 0, rotation = "counter")

  slope <- tan(as.numeric(a_rad))
  rs <- (x$l / 2) / sqrt(1 + slope^2)
  x0 <- x$x -           rs
  x1 <- x$x +           rs
  y0 <- x$y - (slope) * rs
  y1 <- x$y + (slope) * rs
  
  return(list(df = data.frame(x0, y0, x1, y1),
              ls = list(x0 = x0, y0 = y0,
                        x1 = x1, y1 = y1)))
}
