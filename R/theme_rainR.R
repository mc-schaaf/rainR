#' rainR ggplot2 theme
#'
#' A bold, publication-style theme with enclosed panels, internal legends,
#' and no grid lines.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param header_family Font family for titles and headers.
#' @param base_line_size Base line width.
#' @param base_rect_size Base rectangle border width.
#' @param ink Foreground colour.
#' @param paper Background colour.
#' @param accent Accent colour.
#' @param lineend Line end style.
#' @param linejoin Line join style.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#'
#' ggplot2::ggplot(mtcars, ggplot2::aes(
#'   wt, mpg,
#'   colour = factor(cyl),
#'   shape = factor(gear)
#' )) +
#'   ggplot2::geom_point(size = 2.5) +
#'   ggplot2::facet_wrap(~am) +
#'   ggplot2::labs(
#'     title = "Fuel economy vs weight",
#'     subtitle = "Testing facets, legends, and strip styling",
#'     x = "Weight (1000 lbs)",
#'     y = "Miles per gallon",
#'     colour = "Cylinders",
#'     shape = "Gears"
#'   )
#'   theme_rainR()
#'
#' @export
theme_rainR <- function(
    base_size = 12,
    base_family = "",
    header_family = NULL,
    base_line_size = base_size / 12,
    base_rect_size = base_size / 12,
    ink = "black",
    paper = "white",
    accent = "#3366FF",
    lineend = "round",
    linejoin = "round"
) {
  # mostly adaptations of theme_classic()
  # spelled out here to avoid dependencies
  # Throughout the theme, three font sizes are used,
  # `base_size` (`rel(1)`) for normal,
  # `rel(0.8)` for small,
  # `rel(1.2)` for large.

  half_line <- base_size / 2

  t <- ggplot2::theme(

    ## Elements in this first block aren't used directly, but are inherited
    ## by others
    line = ggplot2::element_line(
      colour = ink,
      linewidth = base_line_size,
      linetype = 1,
      lineend = lineend,
      linejoin = linejoin
    ),
    rect = ggplot2::element_rect(
      fill = paper,
      colour = ink,
      linewidth = base_rect_size,
      linetype = 1,
      linejoin = linejoin
    ),
    text = ggplot2::element_text(
      family = base_family,
      face = "bold",
      colour = ink,
      size = base_size,
      lineheight = 0.9,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      margin = ggplot2::margin(),
      debug = FALSE
    ),

    title = ggplot2::element_text(family = header_family),

    spacing = ggplot2::unit(half_line, "pt"),
    margins = ggplot2::margin_auto(half_line),

    point = ggplot2::element_point(
      colour = ink,
      shape = 19,
      fill = paper,
      size = (base_size / 12) * 3,
      stroke = base_line_size
    ),

    polygon = ggplot2::element_polygon(
      fill = paper,
      colour = ink,
      linewidth = base_rect_size,
      linetype = 1,
      linejoin = linejoin
    ),

    geom = ggplot2::element_geom(
      ink = ink,
      paper = paper,
      accent = accent,
      linewidth = base_line_size,
      borderwidth = base_line_size,
      linetype = 1L,
      bordertype = 1L,
      family = base_family,
      fontsize = base_size,
      pointsize = (base_size / 12) * 3,
      pointshape = 21
    ),

    ## define the axes
    axis.line = ggplot2::element_blank(),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.text = ggplot2::element_text(),
    axis.text.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 0.8 * half_line / 2),
      vjust = 1
      ),
    axis.text.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = 0.8 * half_line / 2),
      vjust = 0
      ),
    axis.text.y = ggplot2::element_text(
      margin = ggplot2::margin(r = 0.8 * half_line / 2),
      hjust = 1
      ),
    axis.text.y.right = ggplot2::element_text(
      margin = ggplot2::margin(l = 0.8 * half_line / 2),
      hjust = 0
      ),
    axis.text.r = ggplot2::element_text(
      margin = ggplot2::margin(l = 0.8 * half_line / 2, r = 0.8 * half_line / 2),
      hjust = 0.5
    ),
    axis.ticks = ggplot2::element_line(),
    axis.ticks.length = ggplot2::rel(0.5),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,
    axis.minor.ticks.length = ggplot2::rel(0.75),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = half_line / 2),
      vjust = 1
      ),
    axis.title.x.top = ggplot2::element_text(
      margin = ggplot2::margin(b = half_line / 2),
      vjust = 0
      ),
    axis.title.y = ggplot2::element_text(
      margin = ggplot2::margin(r = half_line / 2),
      angle = 90,
      vjust = 1
    ),
    axis.title.y.right = ggplot2::element_text(
      margin = ggplot2::margin(l = half_line / 2),
      angle = -90,
      vjust = 1
    ),

    ## legend
    legend.background = ggplot2::element_blank(),
    legend.spacing = ggplot2::rel(2),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    legend.margin = NULL,
    legend.key = ggplot2::element_blank(),
    legend.key.size = ggplot2::unit(0.8, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.key.spacing = NULL,
    legend.text = ggplot2::element_text(),
    legend.title = ggplot2::element_text(
      margin = ggplot2::margin(b = 0.8 * half_line / 2),
      hjust = 0
      ),
    legend.ticks.length = ggplot2::rel(0.2),
    legend.position = "inside",
    legend.position.inside = c(0, 1),
    legend.direction = NULL,
    legend.justification = c(0, 1),
    legend.box = NULL,
    legend.box.margin = ggplot2::margin_auto(0),
    legend.box.background = ggplot2::element_blank(),
    legend.box.spacing = ggplot2::rel(2),


    ## panel and strip
    panel.background = ggplot2::element_rect(fill = paper, colour = NA),
    panel.border = ggplot2::element_rect(colour = ink, linewidth = ggplot2::rel(2)),
    panel.grid = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_line(linewidth = ggplot2::rel(0.5)),
    panel.spacing = NULL,
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    panel.ontop = FALSE,

    strip.background = ggplot2::element_rect(
      fill = paper,
      colour = ink,
      linewidth = ggplot2::rel(2)
    ),
    strip.clip = "on",
    strip.text = ggplot2::element_text(
      colour = ink,
      margin = ggplot2::margin_auto(0.8 * half_line)
      ),
    strip.text.x = NULL,
    strip.text.y = ggplot2::element_text(angle = -90),
    strip.text.y.left = ggplot2::element_text(angle = 90),
    strip.placement = "inside",
    strip.placement.x = NULL,
    strip.placement.y = NULL,
    strip.switch.pad.grid = ggplot2::unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = ggplot2::unit(half_line / 2, "pt"),


    plot.background = ggplot2::element_rect(colour = paper),
    plot.title = ggplot2::element_text(
      hjust = 0,
      vjust = 1,
      margin = ggplot2::margin(b = half_line)
    ),
    plot.title.position = "panel",
    plot.subtitle = ggplot2::element_text(
      hjust = 0,
      vjust = 1,
      face = "italic",
      margin = ggplot2::margin(b = half_line)
    ),
    plot.caption = ggplot2::element_text(
      size = ggplot2::rel(0.8),
      face = "italic",
      hjust = 1,
      vjust = 1,
      margin = ggplot2::margin(t = half_line)
    ),
    plot.caption.position = "panel",
    plot.tag = ggplot2::element_text(
      size = ggplot2::rel(1.2),
      hjust = 0.5,
      vjust = 0.5
    ),
    plot.tag.position = 'topleft',
    plot.margin = NULL,

    complete = TRUE
  )

  return(t)
}




#' Set rainR theme as default
#'
#' Sets `theme_rainR()` as the active ggplot2 theme for the current R
#' session.
#'
#' @param ... Additional arguments passed to [theme_rainR()].
#'
#' @return The previous active theme, invisibly, as returned by
#'   [ggplot2::theme_set()].
#'
#' @examples
#' old_theme <- ggplot2::theme_get()
#' on.exit(ggplot2::theme_set(old_theme), add = TRUE)
#'
#' ggplot2::theme_set(theme_rainR())
#'
#' ggplot2::ggplot(mtcars, ggplot2::aes(
#'   wt, mpg,
#'   colour = factor(cyl),
#'   shape = factor(gear)
#' )) +
#'   ggplot2::geom_point(size = 2.5) +
#'   ggplot2::facet_wrap(~am) +
#'   ggplot2::labs(
#'     title = "Fuel economy vs weight",
#'     subtitle = "Testing facets, legends, and strip styling",
#'     x = "Weight (1000 lbs)",
#'     y = "Miles per gallon",
#'     colour = "Cylinders",
#'     shape = "Gears"
#'   )
#'
#' @export
set_theme_rainR <- function(...) {
  th <- theme_rainR(...)
  if (!inherits(th, "theme")) {
    stop("theme_rainR() did not return a valid ggplot2 theme object", call. = FALSE)
  }
  ggplot2::theme_set(th)
}



#' @importFrom rlang .data
#' @keywords internal


.theme_smoke_test <- function() {
  ggplot2::ggplot(
    datasets::mtcars,
    ggplot2::aes(
      .data$wt,
      .data$mpg,
      colour = factor(.data$cyl),
      shape = factor(.data$gear)
    )
  ) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::facet_grid(. ~ .data$am) +
    ggplot2::labs(
      title = "Fuel economy vs weight",
      subtitle = "Testing facets, legends, and strip styling",
      x = "Weight (1000 lbs)",
      y = "Miles per gallon",
      colour = "Cylinders",
      shape = "Gears"
    ) +
    theme_rainR()
}
