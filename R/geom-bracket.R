#' Bracket geom for ggplot2
#'
#' Draws bracket-style annotations between two points in Cartesian space.
#' Supports horizontal, vertical, and diagonal brackets via (x, y) to (xend, yend).
#'
#' @param mapping Set of aesthetic mappings created by \code{aes()}.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use.
#' @param position Position adjustment.
#' @param na.rm If \code{FALSE}, missing values are removed with a warning.
#' @param show.legend Logical. Should this layer be included in legends?
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics.
#' @param tipheight Height of bracket tips in millimetres.
#' @param ... Additional arguments passed to \code{layer()}.
#'
#' @details
#' If only one of \code{xend} or \code{yend} is supplied, the missing
#' coordinate is inferred from the start point.
#' The geom only supports linear coordinate systems. Non-linear
#' coordinates (e.g. polar) will produce a warning and may not
#' render correctly.
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
    tipheight = 2,
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
      tipheight = tipheight,
      na.rm = na.rm,
      ...
    )
  )
}





