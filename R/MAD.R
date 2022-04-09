#' @title MAD
#'
#' @description computes the maximum absolute deviation of a vector of points definied by x and y coordinates,
#' as compared to an ideal trajectory, as defined by the start and end points
#' importantly, the ideal trajectory is thought of as being of infinite length
#'
#' @export

MAD <- function(x_vector, y_vector,
                 x_start=NULL, y_start=NULL,
                 x_end = NULL, y_end=NULL) {

  # check for optional parameters
  if(is.null(x_start)){x_start = x_vector[1]}
  if(is.null(y_start)){y_start = y_vector[1]}
  if(is.null(x_end)){x_end = x_vector[length(x_vector)]}
  if(is.null(y_end)){y_end = y_vector[length(x_vector)]}

  # shift data
  x_shift = x_vector - x_start
  y_shift = y_vector - y_start

  # rotate data to ideal trajectory, as defined by start to end points
  angle = atan2(y_end-y_start, x_end-x_start)
  sin1 = sin(-angle)
  cos1 = cos(-angle)

  x_rot = x_shift*cos1 - y_shift*sin1
  y_rot = x_shift*sin1 + y_shift*cos1

  # find the index of maximum deviation from ideal trajectory
  index = which.max(abs(y_rot))

  return(y_rot[index])

}
