#' Segment errorbar grob internals
#'
#' Internal grid grob used by \code{geom_segment_errorbar()} to draw error-bars
#'
#' @name segmentErrorbar-grob
#' @keywords internal
NULL


#' Create a segment errorbar grob
#'
#' Internal low-level function used by \code{geom_segment_errorbar()}.
#'
#' @param x0,y0 starting coordinates
#' @param x1,y1 ending coordinates
#' @param cap_size width of the errorbar caps in millimetres.
#' @param default.units default unit system
#' @param outline logical indicating whether an outline should be drawn
#'   behind the errorbar.
#' @param outline_scale multiplicative factor applied to the linewidth 
#'   to derive the outline linewidth.
#' @param outline_colour colour of the outline.
#' @param outline_alpha alpha transparency of the outline.
#' @param name grob name
#' @param gp grid graphical parameters
#' @param vp viewport
#'
#' @return a grid grob of class \code{"segmentErrorbar"}.
#'
#' @import grid
#' @keywords internal
#' @noRd
segmentErrorbarGrob <- function(
    x0 = grid::unit(1/3, "npc"),
    x1 = grid::unit(2/3, "npc"),
    y0 = grid::unit(1/3, "npc"),
    y1 = grid::unit(2/3, "npc"),
    cap_size = 2,
    default.units = "npc",
    outline = FALSE,
    outline_scale = 2,
    outline_colour = "white",
    outline_alpha = 1,
    name = NULL,
    gp = grid::gpar(),
    vp = NULL
) {
  
  if (!inherits(x0, "unit")) x0 <- grid::unit(x0, default.units)
  if (!inherits(x1, "unit")) x1 <- grid::unit(x1, default.units)
  if (!inherits(y0, "unit")) y0 <- grid::unit(y0, default.units)
  if (!inherits(y1, "unit")) y1 <- grid::unit(y1, default.units)
  cap_size <- grid::unit(cap_size, "mm")
  
  grid::gTree(
    x0 = x0,
    x1 = x1,
    y0 = y0,
    y1 = y1,
    cap_size = cap_size,
    
    outline = outline,
    outline_scale = outline_scale,
    outline_colour = outline_colour,
    outline_alpha = outline_alpha,
    
    name = name,
    gp = gp,
    vp = vp,
    cl = "segmentErrorbar"
  )
}




#' @importFrom grid convertX convertY convertHeight polylineGrob setChildren gList
#' @export
#' @rdname segmentErrorbar-grob
makeContent.segmentErrorbar <- function(x) {
  
  # convert all inputs to mm (physical coordinate system)
  x0 <- grid::convertX(x$x0, "mm", valueOnly = TRUE)
  x1 <- grid::convertX(x$x1, "mm", valueOnly = TRUE)
  y0 <- grid::convertY(x$y0, "mm", valueOnly = TRUE)
  y1 <- grid::convertY(x$y1, "mm", valueOnly = TRUE)
  cs <- grid::convertHeight(x$cap_size, "mm", valueOnly = TRUE)
  
  # ensure consistent vector lengths
  n <- max(length(x0), length(x1), length(y0), length(y1), length(cs))
  x0 <- rep(x0, length.out = n)
  x1 <- rep(x1, length.out = n)
  y0 <- rep(y0, length.out = n)
  y1 <- rep(y1, length.out = n)
  cs <- rep(cs, length.out = n)
  
  # construct unit vector (px, py) that is rotated -90deg relative to (dx, dy)
  dx <- x1 - x0
  dy <- y1 - y0
  len <- sqrt(dx^2 + dy^2)
  if (any(len == 0)) {
    stop("Zero-length errorbars are not allowed.", call. = FALSE)
  }
  px <-  dy / len
  py <- -dx / len
  
  # Construct data container
  errorbars <- lapply(seq_len(n), function(i) {
    data.frame(
      x = c(
        x0[i]+cs[i]*px[i]/2, x0[i]-cs[i]*px[i]/2,  # cap 1 
        x1[i]+cs[i]*px[i]/2, x1[i]-cs[i]*px[i]/2,  # cap 2
        x0[i], x1[i]  # spine
      ),
      y = c(
        y0[i]+cs[i]*py[i]/2, y0[i]-cs[i]*py[i]/2,  # cap 1 
        y1[i]+cs[i]*py[i]/2, y1[i]-cs[i]*py[i]/2,  # cap 2
        y0[i], y1[i]  # spine
      ),
      id = rep((i-1) * 3 + 1:3, each = 2)
    )
  })
  errorbars <- do.call(rbind, errorbars)
  
  # Construct the grob
  bracket_paths <- grid::polylineGrob(
    x = errorbars$x,
    y = errorbars$y,
    id = errorbars$id,
    default.units = "mm",
    gp = x$gp
  )
  
  if (!isTRUE(x$outline)) {
    grid::setChildren(x, grid::gList(bracket_paths))
  } else {
    # set graphics parameters
    out_gp <- x$gp
    out_gp$col <- x$outline_colour
    out_gp$lwd <- x$gp$lwd * x$outline_scale
    out_gp$alpha <- x$outline_alpha
    
    background_bracket_paths <- grid::polylineGrob(
      x = errorbars$x,
      y = errorbars$y,
      id = errorbars$id,
      default.units = "mm",
      gp = out_gp
    )
    grid::setChildren(x, grid::gList(background_bracket_paths, bracket_paths))
  }
  
}
