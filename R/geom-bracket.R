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

    # Infer missing endpoints
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




#' Bracket annotations between two points
#'
#' Draw bracket-style annotations defined by start and end points. Unlike
#' many annotation geoms that assume horizontal orientation, brackets are
#' constructed from arbitrary line segments and therefore support horizontal,
#' vertical, diagonal, and mixed-direction brackets.
#'
#' Bracket tips are controlled via the `tipheight` aesthetic, which specifies
#' the tip length in millimetres. Because tip heights are measured in physical
#' units rather than data units, they remain visually constant regardless of
#' axis scaling, aspect ratio, or coordinate transformations.
#'
#' Endpoints are specified via `x`, `y`, `xend`, and `yend`. If either
#' `xend` or `yend` is omitted, it is inferred from the corresponding start
#' coordinate, allowing purely horizontal or purely vertical brackets to be
#' specified concisely.
#'
#' @param mapping,data,stat,position,show.legend,inherit.aes
#'   Passed to [ggplot2::layer()].
#' @param na.rm Logical. If `FALSE` (default), missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param ... Additional parameters passed on to the layer, including
#'   aesthetics such as `tipheight`, `linewidth`, `colour`, `alpha`,
#'   `lineend`, and `linejoin`.
#'
#' @section Aesthetics:
#'
#' `geom_bracket()` understands the following aesthetics:
#'
#' - `x`
#' - `y`
#' - `xend`
#' - `yend`
#' - `tipheight` (tip length in millimetres)
#' - `colour`
#' - `linewidth`
#' - `alpha`
#' - `lineend`
#' - `linejoin`
#'
#' `x` and `y` are required. At least one of `xend` or `yend` must be
#' supplied.
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' library(ggplot2)
#'
#' # Horizontal, vertical, and inferred endpoint brackets
#' df <- data.frame(
#'   x = c(1, 2, 4, 5),
#'   xend = c(2, 3, 3, 4),
#'   y = c(10, 15, 20, 25),
#'   group = c("x>xend", "x>xend", "x<xend", "x<xend")
#' )
#'
#' # basic horizontal brackets
#' ggplot(df) +
#'   geom_bracket(
#'     aes(x = x, xend = xend, y = y),
#'     tipheight = 2,
#'     linewidth = 2
#'   ) +
#'   facet_wrap(~group)
#'
#' # same geometry under coord_flip()
#' ggplot(df) +
#'   geom_bracket(
#'     aes(x = x, xend = xend, y = y),
#'     tipheight = 2,
#'     linewidth = 2
#'   ) +
#'   coord_flip() +
#'   facet_wrap(~group)
#'
#' # mixed xend / yend specification (inferred endpoints supported)
#' df2 <- data.frame(
#'   x = c(1, 2, 3, 4),
#'   xend = c(4, NA, 4, 5),
#'   y = c(10, 20, 30, 20),
#'   yend  = c(NA, 30, 40, 10)
#' )
#'
#' ggplot(df2, aes(x = x, xend = xend, y = y, yend = yend)) +
#'   geom_bracket(
#'     tipheight = 2,
#'     colour = "black",
#'     linewidth = 1
#'   )
#'
#' @import ggplot2
#' @export
geom_bracket <- function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    ...
) {

  ggplot2::layer(
    geom = GeomBracket,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}





