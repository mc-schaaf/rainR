# computes the maximum absolute deviation of a point definied by x and y coordinates,
# as compared to an ideal trajectory, as defined by the start and end points
# importantly, the ideal trajectory is thought of as being of infinite length

MAD <- function(x_vector, y_vector,
                   x_start=x_vector[1], y_start=y_vector[1],
                   allign= FALSE, chatty=FALSE) {

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

  # find the index of maximum deviation from ideal trajectory
  index = which.max(abs(y_rot))


  # some plots, for demonstration purposes
  if(chatty){
    print(paste0("Trajectory angle: ", round(angle*(180/pi)), " degrees."))
    sin2 = sin(angle)
    cos2 = cos(angle)

    plot(x_rot,y_rot, main = "rotated")
    graphics::segments(x_rot[index],
             0,
             x_rot[index],
             y_rot[index],
             col = "red")
    graphics::segments(x_vector[1]*cos1-y_vector[1]*sin1,
             x_vector[1]*sin1+y_vector[1]*cos1,
             x_end*cos1-y_end*sin1,
             x_end*sin1+y_end*cos1,
             lwd = 3)
    graphics::text(x_rot[index],y_rot[index]/2, pos=4,
         paste0("MAD: ", round(y_rot[index])),
         col = "red"
    )

    plot(x_vector,y_vector, main = "unrotated")
    graphics::segments(x_rot[index]*cos2,
             x_rot[index]*sin2,
             x_vector[index],
             y_vector[index],
             col = "red")
    graphics::segments(x_vector[1],
             y_vector[1],
             x_end,
             y_end,
             lwd = 3)
    graphics::points(x_rot*cos2, x_rot*sin2)
    graphics::text(x_rot[index]*cos2-y_rot[index]*sin2/2,
         (x_rot[index]*sin2+y_rot[index]*cos2/2),
         pos=4, paste0("MAD: ", round(y_rot[index])),
         col = "red"
    )
  }


  return(rep(y_rot[index], length(y_rot)))
}


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
#   MAD = RainR_MAD(xvals, yvals, chatty = TRUE)
# )



