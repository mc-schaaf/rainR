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

  # allign start position to the starting point of the ideal trajectory
  if(allign){
    x_vector = x_vector-x_start
    y_vector = y_vector-y_start
  }
  x_end=x_vector[length(x_vector)]
  y_end=y_vector[length(y_vector)]


  # rotate data to ideal trajectory, as defined by start to end points
  angle = atan2((y_end-y_vector[1]), (x_end-x_vector[1]))

  sin1 = sin(-angle)
  cos1 = cos(-angle)

  x_rot = x_vector*cos1 - y_vector*sin1
  y_rot = x_vector*sin1 + y_vector*cos1


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
  return(rep(c_AUC[length(c_AUC)], length(x_vector)))
}


#
# dat <- data.frame(
#   group = c(rep("A", 101),
#             rep("B", 101),
#             rep("C", 101),
#             rep("D", 101)
#             ),
#   xvals = c( rep(0,50),seq(from=0,to=300,length.out = 51),
#              seq(from=0,to=300,length.out = 51), rep(300,50),
#              rep(0,25),seq(from=0,to=300,length.out = 51),rep(300,25),
#              seq(0,25,length.out = 25),seq(25,275,length.out = 51),seq(275,300,length.out = 25)
#              ),
#   yvals = c( seq(from=0,to=600,length.out = 51),rep(600,50),
#              rep(0,50), seq(from=0,to=600,length.out = 51),
#              seq(from=0,to=300,length.out = 25),rep(300,51),seq(from=300,to=600,length.out = 25),
#              seq(from=0,to=440,length.out = 25),rep(440,51),seq(from=440,to=600,length.out = 25)
#              )
# )
#
#
# show_me_what_you_goooooooot <- dat %>% group_by(group) %>% mutate(
#   MAD = RainR_AUC(xvals, yvals, cumulative = T)
# )
#
#
#
