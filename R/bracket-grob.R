#' Bracket grob internals
#'
#' Internal grid grob used by \code{geom_bracket()} to draw bracket shapes.
#'
#' @name bracket-grob
#' @keywords internal
NULL


#' Create a bracket grob
#'
#' Internal low-level function used by \code{geom_bracket()}.
#'
#' @param x0,y0 starting coordinates
#' @param x1,y1 ending coordinates
#' @param tipheight height of bracket tips in millimetres
#' @param default.units default unit system
#' @param name grob name
#' @param gp grid graphical parameters
#' @param vp viewport
#'
#' @return A grid grob of class \code{"bracket"}.
#'
#' @import grid
#' @keywords internal
#' @noRd
bracketGrob <- function(
    x0 = grid::unit(1/3, "npc"),
    x1 = grid::unit(2/3, "npc"),
    y0 = grid::unit(1/3, "npc"),
    y1 = grid::unit(2/3, "npc"),
    tipheight = 2,
    default.units = "npc",
    name = NULL,
    gp = grid::gpar(),
    vp = NULL
) {
  
  if (!inherits(x0, "unit")) x0 <- grid::unit(x0, default.units)
  if (!inherits(x1, "unit")) x1 <- grid::unit(x1, default.units)
  if (!inherits(y0, "unit")) y0 <- grid::unit(y0, default.units)
  if (!inherits(y1, "unit")) y1 <- grid::unit(y1, default.units)
  tipheight <- grid::unit(tipheight, "mm")
  
  grid::gTree(
    x0 = x0,
    x1 = x1,
    y0 = y0,
    y1 = y1,
    tipheight = tipheight,
    name = name,
    gp = gp,
    vp = vp,
    cl = "bracket"
  )
}


#' @importFrom grid convertX convertY convertHeight polylineGrob setChildren gList
#' @export
#' @rdname bracket-grob
makeContent.bracket <- function(x) {
  
  # convert all inputs to mm (physical coordinate system)
  x0 <- grid::convertX(x$x0, "mm", valueOnly = TRUE)
  x1 <- grid::convertX(x$x1, "mm", valueOnly = TRUE)
  y0 <- grid::convertY(x$y0, "mm", valueOnly = TRUE)
  y1 <- grid::convertY(x$y1, "mm", valueOnly = TRUE)
  th <- grid::convertHeight(x$tipheight, "mm", valueOnly = TRUE)
  
  # ensure consistent vector lengths
  n <- max(length(x0), length(x1), length(y0), length(y1), length(th))
  x0 <- rep(x0, length.out = n)
  x1 <- rep(x1, length.out = n)
  y0 <- rep(y0, length.out = n)
  y1 <- rep(y1, length.out = n)
  th <- rep(th, length.out = n)
  
  # construct unit vector (px, py) that is rotated -90deg relative to (dx, dy)
  dx <- x1 - x0
  dy <- y1 - y0
  len <- sqrt(dx^2 + dy^2)
  if (any(len == 0)) {
    stop("Zero-length brackets are not allowed.")
  }
  px <-  dy / len
  py <- -dx / len
  
  # Construct data container
  brackets <- lapply(seq_len(n), function(i) {
    data.frame(
      x = c(x0[i]+th[i]*px[i], x0[i], x1[i], x1[i]+th[i]*px[i]),
      y = c(y0[i]+th[i]*py[i], y0[i], y1[i], y1[i]+th[i]*py[i]),
      id = i
    )
  })
  brackets <- do.call(rbind, brackets)
  
  # Construct the grob
  bracket_paths <- grid::polylineGrob(
    x = brackets$x,
    y = brackets$y,
    id = brackets$id,
    default.units = "mm",
    gp = x$gp
  )
  
  grid::setChildren(x, grid::gList(bracket_paths))
  
}


