#' @title Maximum Absolute Deviation
#'
#' @description This function computes the absolute (Euclidean) distance of a
#' point P (with the 2D-coordinates x_vector and y_vector) and a line,
#' defined by a slope and an intercept.
#' The arguments x_vector and y_vector may be scalars or vectors of the
#' same length.
#' Translation of the script given by
#' Pfister et al. (2014) <\doi{10.1016/j.cognition.2014.07.012}>.
#'
#' @param x_vector Vector of the x-coordinates of the executed trajectory.
#' @param y_vector Vector of the y-coordinates of the executed trajectory.
#' @param slope Slope of the ideal trajectory.
#' @param intercept Intercept of the ideal trajectory.
#'
#' @details
#' \preformatted{
#' The Euclidean distance d between P and the line defined by a and b can
#' be computed as a function of the x coordinate of a point G(x,y) on the
#' line (general case). To avoid confusion, I will denote px and py as p1
#' and p2, respectively.
#'
#'    d(x) = sqrt( (p1-x)^2 + (p2-y)^2 )       | ()^2
#'
#'    d(x)^2 = (p1-x)^2 + (p2-y)^2             | solve brackets
#'
#'    d(x)^2 = p1^2 - 2*p1*x + x^2 + p2^2 - 2*p2*a*x - 2*p2*b + 2*a*x*b + a^2*x^2 + b^2
#'
#'    Rearranging to bring all terms including x's to the front...
#'
#'    d(x)^2 = (a^2+1)*x^2 + (2*a*b - 2*p1 - 2*p2*a)*x + p1^2 + p2^2 - 2*p2*b + b^2
#'
#'    Deriving this latter function helps to find the minimum
#'    (absolute) distance between P and the line (note that none
#'    of the terms in the second line of the above equation contains
#'    any x's):
#'
#'    d(x)^2' = 2*(a^2+1)*x + 2*(a*b - p1 - p2*a)
#'
#'    d(x)^2' =(!)= 0
#'
#' The x coordinate corresponding to the global minimum is denoted as x0:
#'
#'    x0 = (p1 + p2*a - a*b) / (a^2+1)
#'}
#'
#' @returns List of length 3 with
#' \tabular{ll}{
#'  \code{ad}\tab Vector of the absolute distances.\cr
#'  \code{direction}\tab Vector of directions (1 = above the line,
#'     0 = on the line, -1 = below the line).\cr
#'  \code{linecoords}\tab List with the vectors x and y for x0 and y0.
#' }
#'
#' @references Wirth, R., Foerster, A., Kunde, W., & Pfister, R. (2020).
#' Design choices: Empirical recommendations for designing two-dimensional
#' finger tracking experiments. Behavior Research Methods, 52, 2394 - 2416.
#' \doi{10.3758/s13428-020-01409-0}
#'
#'
#' @export
#'



max_ad2 <- function(x_vector, y_vector, slope, intercept) {
  # input conversion to mirror variable names of RP
  px <- x_vector
  py <- y_vector
  a <- slope
  b <- intercept

  # compute linecoords
  x0 <- (px + py * a - a * b) / (a ^ 2 + 1)
  y0 <- a * x0 + b
  linecoords <- list()
  linecoords$x <- x0
  linecoords$y <- y0

  # Finally, ad is computed as the simple Euclidean distance between the two
  # points P(px,py) and G(x0,y0):
  ad <- sqrt((px - x0) ^ 2 + (py - y0) ^ 2)

  # Compute direction.
  direction <- sign(py - (a * px + b))

  return(list(
    ad = ad,
    direction = direction,
    linecoords = linecoords
  ))
}
