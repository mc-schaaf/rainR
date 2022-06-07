#' @title Area Under the Curve
#'
#' @description This function computes areas under the curve (AUCs) for
#' trajectory data with the 2D-coordinates px and py and a line, defined by
#' slope a and intercept b. The arguments px and py may be scalars or vectors
#' of the same length.
#' Translation of the script given by Pfister et al. (2014) at
#' https://doi.org/10.1016/j.cognition.2014.07.012
#'
#' @param x_vector vector of the x-coordinates of the executed trajectory
#' @param y_vector vector of the y-coordinates of the executed trajectory
#' @param slope slope of the ideal trajectory
#' @param intercept intercept of the ideal trajectory
#'
#' @details AUCs are computed by dividing the area in triangular and square-shaped
#' pieces and adding the up across the trajectory.
#'
#' @return Vector representing the cumulative AUC across course of the trajectory.
#'
#' @export
#'

auc2 <- function(x_vector, y_vector, slope, intercept) {

  max_ad2_out <- rainR::max_ad2(x_vector, y_vector, slope, intercept)
  ad <- max_ad2_out$ad
  direction <- max_ad2_out$direction
  linecoords <- max_ad2_out$linecoords

  # allocate output vector
  cum_auc <- rep(NA, length(ad))
  cum_auc[1] <- 0

  for (i in 2:length(ad)) {
    # 0. Do not increase cum_auc if coordinates do not change.
    if (linecoords$x[i] == linecoords$x[i - 1] &&
        linecoords$y[i] == linecoords$y[i - 1]) {
      cum_auc[i] <- cum_auc[i - 1]
      next
    }
    # 1. Compute distance on line
    ldist <- sqrt(((linecoords$x[i] - linecoords$x[i - 1]) ^ 2) +
                   ((linecoords$y[i] - linecoords$y[i - 1]) ^ 2))
    # 2. Check for line flips
    if (direction[i] == direction[i - 1]) {
      # If no flip is detected: Increment cum_auc (1) by
      # rectangle (2) and triangle (3).
      cum_auc[i] <- cum_auc[i - 1] +                              # (1)
        direction[i] * ldist *
        (min(ad[i], ad[i - 1]) +                                  # (2)
           sign(ad[i] - ad[i - 1]) * (ad[i] - ad[i - 1]) / 2)     # (3)
    } else {
      # Add triangle or trapezoid (verschraenktes Trapez, Wiki)
      # if trajectory hits or crosses ideal line.
      if (direction[i] == 0) {
        cum_auc[i] <- cum_auc[i - 1] + direction[i - 1] * ldist *
          ((ad[i - 1] - ad[i]) / 2)

      } else if (direction[i - 1] == 0) {
        cum_auc[i] <- cum_auc[i - 1] + direction[i] * ldist *
          ((ad[i] - ad[i - 1]) / 2)

      } else {
        cum_auc[i] <- cum_auc[i - 1] + direction[i] * ldist *
          ((ad[i] - ad[i - 1]) / 2)

      }
    }
  }

  return(cum_auc)

}
