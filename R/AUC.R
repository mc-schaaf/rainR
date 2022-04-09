#' @title AUC
#'
#' @description computes the (cumulative) AUC of a point defined by x and y coordinates,
#' as compared to an ideal trajectory, as defined by the start and end points
#' importantly, the ideal trajectory is thought of as being of infinite length
#'
#' @export

AUC <- function(x_vector, y_vector,
                      x_start=x_vector[1], y_start=y_vector[1],
                      allign= FALSE, cumulative = FALSE) {

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

  # compute differences between (time-) adjacent points
  d_x = x_rot[2:length(x_rot)] - x_rot[1:(length(x_rot)-1)]
  d_y = y_rot[2:length(y_rot)] - y_rot[1:(length(y_rot)-1)]

  # compute square under the curve and triangle under the curve
  AUC_increment = d_x*y_rot[1:(length(y_rot))-1] + d_x*d_y*0.5

  # cumulate over it
  c_AUC = cumsum(AUC_increment)

  if(cumulative){
    return(c(0, c_AUC))
  }
  return(c_AUC[length(c_AUC)])
}
