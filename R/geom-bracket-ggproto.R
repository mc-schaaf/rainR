#' @import ggplot2
GeomBracket <- ggproto(
  "GeomBracket",
  ggplot2::Geom,
  
  required_aes = c("x", "y"),
  
  default_aes = ggplot2::aes(
    xend = NA, 
    yend = NA,
    tipheight = 2,
    colour = "black",
    linewidth = 1,
    alpha = 1,
    lineend = "round",
    linejoin = "round"
  ),
  
  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    
    # Remove missing data, returning early if all are missing
    data <- ggplot2::remove_missing(
      df = data, 
      na.rm = na.rm,
      vars = c("x", "y", "tipheight", "colour", "linewidth", "alpha", "lineend", "linejoin"),
      name = "geom_bracket"
    )
    
    # check that data contains at least one of xend or yend
    invalid <- is.na(data$xend) & is.na(data$yend)
    if (all(invalid)) {
      if (!na.rm){
        stop("At least one of xend or yend must be supplied to bracket geom.")
      } 
      return(ggplot2::zeroGrob())
    }
    if (any(invalid)) {
      if (!na.rm) {
        rlang::warn(paste0("Removed ", sum(invalid), 
                           " rows containing missing bracket endpoints."))
      }
      data <- data[!invalid, , drop = FALSE]
    }
    data$xend[is.na(data$xend)] <- data$x[is.na(data$xend)]
    data$yend[is.na(data$yend)] <- data$y[is.na(data$yend)]
    
    # Supply the coordinate system for the plot
    if (!coord$is_linear()) {
      rlang::warn(
        "bracket geom only works correctly on linear coordinate systems"
      )
    }
    
    # transform data to panel (ggplot coordinates)
    data <- coord$transform(data, panel_params)
    
    # Construct the grob
    bracketGrob(
      x0 = data$x,
      x1 = data$xend, 
      y0 = data$y,
      y1 = data$yend,
      tipheight = data$tipheight,
      default.units = "native",
      gp = grid::gpar(
        col = data$colour,
        lwd = data$linewidth * ggplot2::.pt,
        alpha = data$alpha,  
        lineend = data$lineend,
        linejoin = data$linejoin
      )
    )
    
  }
)