#' @import ggplot2
GeomSegmentErrorbar <- ggproto(
  "GeomSegmentErrorbar",
  ggplot2::Geom,
  
  required_aes = c("x", "y"),
  
  default_aes = ggplot2::aes(
    xend = NA, 
    yend = NA,
    cap_size = 2,
    colour = "black",
    linewidth = 1,
    alpha = 1,
    lineend = "round",
    linejoin = "round"
  ),
  
  extra_params = c(
    "outline",
    "outline_scale",
    "outline_colour",
    "outline_alpha"
  ),
  
  draw_panel = function(data, panel_params, coord, 
                        outline = FALSE,
                        outline_scale = 2,
                        outline_colour = "white",
                        outline_alpha = 1,
                        na.rm = FALSE) {
    
    
    
    # Remove missing data, returning early if all are missing
    data <- ggplot2::remove_missing(
      df = data, 
      na.rm = na.rm,
      vars = c("x", "y", "cap_size", "colour", "linewidth", "alpha", "lineend", "linejoin"),
      name = "geom_segment_errorbar"
    )
    
    # check that data contains at least one of xend or yend
    invalid <- is.na(data$xend) & is.na(data$yend)
    if (all(invalid)) {
      if (!na.rm){
        stop("At least one of xend or yend must be supplied to segment errorbar geom.")
      } 
      return(ggplot2::zeroGrob())
    }
    if (any(invalid)) {
      if (!na.rm) {
        rlang::warn(paste0("Removed ", sum(invalid), 
                           " rows containing missing segment errorbar endpoints."))
      }
      data <- data[!invalid, , drop = FALSE]
    }
    
    # Infer missing endpoints
    data$xend[is.na(data$xend)] <- data$x[is.na(data$xend)]
    data$yend[is.na(data$yend)] <- data$y[is.na(data$yend)]
    
    # Supply the coordinate system for the plot
    if (!coord$is_linear()) {
      rlang::warn(
        "segment errorbar geom only works correctly on linear coordinate systems"
      )
    }
    
    # transform data to panel (ggplot coordinates)
    data <- coord$transform(data, panel_params)
    
    # Construct the grob
    segmentErrorbarGrob(
      x0 = data$x,
      x1 = data$xend, 
      y0 = data$y,
      y1 = data$yend,
      cap_size = data$cap_size,
      default.units = "native",
      
      outline = outline,
      outline_scale = outline_scale,
      outline_colour = outline_colour,
      outline_alpha = outline_alpha,
      
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





#' Segment error bars with fixed-size caps
#'
#' Draw error bars defined by start and end points. Unlike
#' [ggplot2::geom_errorbar()], this geom is based on arbitrary line segments
#' and therefore supports horizontal, vertical, diagonal, and mixed-direction
#' error bars.
#'
#' Error-bar caps are controlled via the `cap_size` aesthetic, which specifies
#' the cap length in millimetres. Because cap sizes are measured in physical
#' units rather than data units, they remain visually constant regardless of
#' axis scaling, aspect ratio, or coordinate transformations.
#' 
#' Endpoints are specified via `x`, `y`, `xend`, and `yend`. If either
#' `xend` or `yend` is omitted, it is inferred from the corresponding start
#' coordinate, allowing purely horizontal or purely vertical error bars to be
#' specified concisely.
#'
#' Optionally, an outline can be drawn behind the error bar to improve
#' visibility against complex backgrounds.
#'
#' @param mapping,data,stat,position,show.legend,inherit.aes
#'   Passed to [ggplot2::layer()].
#' @param na.rm Logical. If `FALSE` (default), missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param outline Logical indicating whether a larger outline should be drawn
#'   behind the error bar.
#' @param outline_scale Numeric scaling factor applied to the linewidth of the
#'   outline.
#' @param outline_colour Colour of the outline.
#' @param outline_alpha Alpha transparency of the outline.
#' @param ... Additional parameters passed on to the layer, including
#'   aesthetics such as `linewidth`, `colour`, `alpha`, `lineend`, and
#'   `linejoin`.
#'
#' @section Aesthetics:
#'
#' `geom_segment_errorbar()` understands the following aesthetics:
#'
#' - `x`
#' - `y`
#' - `xend`
#' - `yend`
#' - `cap_size` (cap size in millimetres)
#' - `colour`
#' - `linewidth`
#' - `alpha`
#' - `lineend`
#' - `linejoin`
#'
#' `x` and `y` are required. At least one of `xend` or `yend` must be
#' supplied.
#'
#' @return
#' A ggplot2 layer.
#'
#' @examples
#' library(ggplot2)
#'
#' # Horizontal error bars: Supply no yend
#' df <- data.frame(
#'   x = c(1, 2, 4, 5),
#'   xend = c(2, 3, 3, 4),
#'   y = c(10, 15, 20, 25),
#'   cap_size = c(2, 4, 8, 10),
#'   group = c("A", "A", "B", "B")
#' )
#'
#' ggplot(df) +
#'   geom_segment_errorbar(
#'     aes(x = x, xend = xend, y = y),
#'     cap_size = 5,
#'     linewidth = 2
#'   ) +
#'   facet_wrap(~group)
#'
#' # Vertical error bars: Supply no xend
#' ggplot(df) +
#'   geom_segment_errorbar(
#'     aes(y = x, yend = xend, x = y, cap_size = cap_size)
#'   ) +
#'   coord_flip() +
#'   facet_wrap(~group)
#'
#' # Mixed xend/yend specification and outline
#' df2 <- data.frame(
#'   x = c(1, 2, 3, 4),
#'   xend = c(4, NA, 4, 5),
#'   y = c(10, 20, 30, 20),
#'   yend = c(NA, 30, 40, 10)
#' )
#'
#' ggplot(df2, aes(x = x, xend = xend, y = y, yend = yend)) +
#'   geom_segment_errorbar(
#'     cap_size = 10,
#'     linewidth = 5,
#'     outline = TRUE,
#'     outline_colour = "white",
#'     outline_scale = 1.5
#'   ) +
#'   theme(
#'     panel.background = element_rect(fill = "blue")
#'   )
#'
#' @export
geom_segment_errorbar <- function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    
    outline = FALSE,
    outline_scale = 2,
    outline_colour = "white",
    outline_alpha = 1,
    ...
) {
  
  ggplot2::layer(
    geom = GeomSegmentErrorbar,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outline = outline,
      outline_scale = outline_scale,
      outline_colour = outline_colour,
      outline_alpha = outline_alpha,
      na.rm = na.rm,
      ...
    )
  )
}




library(ggplot2)
# Horizontal, vertical, and inferred direction errorbars
df <- data.frame(
  x = c(1, 2, 4, 5),
  xend = c(2, 3, 3, 4),
  y = c(10, 15, 20, 25),
  cap_size = c(2, 4, 8, 10), 
  group = c("x>xend", "x>xend", "x<xend", "x<xend")
)

# omit yend for basic horizontal errorbars, omitted xend similar
ggplot(df) +
  geom_segment_errorbar(
    aes(x = x, xend = xend, y = y),
    cap_size = 5,
    linewidth = 2
  ) +
  facet_wrap(~group)

# same geometry under coord_flip(), variable cap_size
ggplot(df) +
  geom_segment_errorbar(
    aes(x = x, xend = xend, y = y, cap_size = cap_size)
  ) +
  coord_flip() +
  facet_wrap(~group)


# mixed xend / yend specification (inferred directions supported)
df2 <- data.frame(
  x = c(1, 2, 3, 4),
  xend = c(4, NA, 4, 5),
  y = c(10, 20, 30, 20),
  yend  = c(NA, 30, 40, 10)
)

# supports outlined error bar for clearer visual language
ggplot(df2, aes(x = x, xend = xend, y = y, yend = yend)) +
  geom_segment_errorbar(
    cap_size = 10, 
    linewidth = 5,
    outline = TRUE, 
    outline_colour = "white", 
    outline_scale = 1.5
  ) +
  theme(
    panel.background = element_rect(fill = "blue")
  )
