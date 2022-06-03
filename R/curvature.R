#' @title curvature
#'
#' @description computes the curvature of a trajectory, defined by arrays of x and y coordinates,
#' as compared to an ideal trajectory, as defined by the start and end points of the trajectory
#'
#' @param x_vector vector of the x-coordinates of the executed trajectory
#' @param y_vector vector of the y-coordinates of the executed trajectory
#'
#' @return single number corresponding to the curvature
#'
#' @examples
#'
#' data("dat_one_trajectory")
#' curvature(dat_one_trajectory$xvals, dat_one_trajectory$yvals)
#'
#' @export
#'

curvature <- function(x_vector, y_vector) {
  # distance of the ideal trajectory
  d_ideal = sqrt(((x_vector[1] - x_vector[length(x_vector)]) ^ 2) +
                   ((y_vector[1] - y_vector[length(y_vector)]) ^ 2))

  # distance of the real trajectory
  ds_real = sqrt(((x_vector[2:length(x_vector)] - x_vector[1:(length(x_vector) - 1)]) ^ 2) +
                   ((y_vector[2:length(y_vector)] - y_vector[1:(length(y_vector) - 1)]) ^ 2))
  d_real = sum(ds_real)

  return(d_real / d_ideal)
}
