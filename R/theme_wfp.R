# Creates a function that stores a WFP ggplot theme ----

#' theme_wfp
#'
#' A custom ggplot2 theme function designed for WFP visualizations.
#'
#' @param font_size Numeric. Base font size for the theme. Default is 10.
#' @param font_family Character. Font family to use. Default is "Open Sans".
#' @param line_size Numeric. Line size for elements like grid lines and axis lines. Default is 0.5.
#' @param rel_tiny Numeric. Relative size for tiny text. Default is 6/10.
#' @param rel_small Numeric. Relative size for small text. Default is 8/10.
#' @param rel_normal Numeric. Relative size for normal text. Default is 10/10.
#' @param rel_large Numeric. Relative size for large text. Default is 12/10.
#' @param rel_xlarge Numeric. Relative size for extra-large text. Default is 14/10.
#' @param rel_xxlarge Numeric. Relative size for extra-extra-large text. Default is 16/10.
#' @param grid Logical or Character. Whether to display grid lines. Can also specify "x", "y", "X", or "Y" for specific grid lines. Default is TRUE.
#' @param axis Logical or Character. Whether to display axis lines. Can also specify "x" or "y" for specific axes. Default is "x".
#' @param axis_text Logical or Character. Whether to display axis text. Can also specify "x" or "y" for specific axes. Default is TRUE.
#' @param axis_title Logical or Character. Whether to display axis titles. Can also specify "x" or "y" for specific axes. Default is TRUE.
#' @param axis_ticks Logical or Character. Whether to display axis ticks. Can also specify "x" or "y" for specific axes. Default is FALSE.
#' @param legend Logical. Whether to display the legend. Default is TRUE.
#' @param legend_title Logical. Whether to display the legend title. Default is FALSE.
#'
#' @return A ggplot2 theme object customized for WFP visualizations.
#' @export
theme_wfp <- function(font_size = 10, font_family = "Open Sans", line_size = .5,
                      rel_tiny = 6 / 10, rel_small = 8 / 10,  rel_normal = 10 / 10, rel_large = 12 / 10, rel_xlarge = 14 / 10, rel_xxlarge = 16 / 10,
                      grid = TRUE, axis = "x", axis_text = TRUE, axis_title = TRUE,
                      axis_ticks = FALSE, legend = TRUE, legend_title = FALSE) {

  ## establishment of margin formatting
  half_line <- font_size / 2

  ## establishment of colour palette for font and line formatting
  wfp_blue <- "#007DBC"
  dark_grey <- "#191919"
  medium_grey <- "#666666"
  light_grey <- "#CCCCCC"

  ## establishment of default theme formatting
  ret <- ggplot2::theme_minimal(base_family = font_family, base_size = font_size)

  ## modification of default theme formatting
  ret <- ret + ggplot2::theme(line = ggplot2::element_line(
    color = "black", size = line_size, linetype = 1, lineend = "butt"
  ))
  ret <- ret + ggplot2::theme(rect = ggplot2::element_rect(
    fill = NA, color = NA, size = line_size, linetype = 1
  ))
  ret <- ret + ggplot2::theme(text = ggplot2::element_text(
    family = font_family, face = "plain", color = dark_grey,
    size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
    margin = ggplot2::margin(), debug = FALSE
  ))

  ## establishment of legend formatting
  if (!legend) {
    ret <- ret + theme(legend.position = "none")
  } else {
    ret <- ret + ggplot2::theme(legend.background = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(legend.spacing = grid::unit(font_size, "pt"))
    ret <- ret + ggplot2::theme(legend.margin = ggplot2::margin(0, 0, 0, 0))
    ret <- ret + ggplot2::theme(legend.key = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(legend.key.size = grid::unit(1.2 * font_size, "pt"))
    ret <- ret + ggplot2::theme(legend.text = ggplot2::element_text(color = dark_grey, size = rel(rel_small)))
    ret <- ret + ggplot2::theme(legend.position = "top")
    ret <- ret + ggplot2::theme(legend.direction = "horizontal")
    ret <- ret + ggplot2::theme(legend.justification = 0)
    ret <- ret + ggplot2::theme(legend.box.margin = ggplot2::margin(0, 0, 0, 0))
    ret <- ret + ggplot2::theme(legend.box.background = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(legend.box.spacing = grid::unit(font_size, "pt"))
    if (!legend_title) {
      ret <- ret + theme(legend.title = element_blank())
    } else {
      ret <- ret + ggplot2::theme(legend.title = ggplot2::element_text(size = rel(rel_small),
                                                                       color = dark_grey,
                                                                       hjust = 0))
    }
  }

  ## establishment of grid formatting for panel (i.e. space behind faceted plots)
  if (inherits(grid, "character") | grid == TRUE) {
    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_line(color = light_grey, size = line_size / 2))
    ret <- ret + ggplot2::theme(panel.grid.major = ggplot2::element_line(color = light_grey, size = line_size / 2))
    ret <- ret + ggplot2::theme(panel.grid.minor = ggplot2::element_line(color = light_grey, size = line_size / 2))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    }
  } else {
    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_blank())
  }

  ## establishment of formatting for panel (i.e. space behind faceted plots)
  ret <- ret + ggplot2::theme(panel.background = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(panel.border = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(panel.spacing = grid::unit(half_line, "pt"))
  ret <- ret + ggplot2::theme(panel.ontop = FALSE)

  ## establishment of formatting for axis
  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_line(
      color = dark_grey, size = line_size,
      lineend = "square"
    ))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(
          color = dark_grey, size = line_size,
          lineend = "square"
        ))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(
          color = dark_grey, size = line_size,
          lineend = "square"
        ))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(
        color = dark_grey, size = line_size,
        lineend = "square"
      ))
      ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(
        color = dark_grey, size = line_size,
        lineend = "square"
      ))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_blank())
  }

  ## establishment of formatting for axis text
  if (inherits(axis_text, "character") | axis_text == TRUE) {
    ret <- ret + ggplot2::theme(axis.text = ggplot2::element_text(size = rel(rel_small), color = dark_grey))
    if (inherits(axis_text, "character")) {
      axis_text <- tolower(axis_text)
      if (regexpr("x", axis_text)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_text(margin = ggplot2::margin(
          t = rel_small * font_size / 4
        ), vjust = 1))
        ret <- ret + ggplot2::theme(axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(
          b = rel_small * font_size / 4
        ), vjust = 0))
      }
      if (regexpr("y", axis_text)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_text(margin = ggplot2::margin(
          r = rel_small * font_size / 4
        ), hjust = 1))
        ret <- ret + ggplot2::theme(axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(
          l = rel_small * font_size / 4
        ), hjust = 0))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_text(margin = ggplot2::margin(
        t = rel_small * font_size / 4
      ), vjust = 1))
      ret <- ret + ggplot2::theme(axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(
        b = rel_small * font_size / 4
      ), vjust = 0))
      ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_text(margin = ggplot2::margin(
        r = rel_small * font_size / 4
      ), hjust = 1))
      ret <- ret + ggplot2::theme(axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(
        l = rel_small * font_size / 4
      ), hjust = 0))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.text = ggplot2::element_blank())
  }

  ## establishment of formatting for axis title
  if (inherits(axis_title, "character") | axis_title == TRUE) {
    ret <- ret + ggplot2::theme(axis.title = ggplot2::element_text(size = rel(rel_small), color = medium_grey))
    if (inherits(axis_title, "character")) {
      axis_title <- tolower(axis_title)
      if (regexpr("x", axis_title)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_text(margin = ggplot2::margin(
          t = rel_small * font_size / 4
        ), vjust = 1))
        ret <- ret + ggplot2::theme(axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(
          b = rel_small * font_size / 4
        ), vjust = 0))
      }
      if (regexpr("y", axis_title)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(
          r = rel_small * font_size / 4
        ), vjust = 1))
        ret <- ret + ggplot2::theme(axis.title.y.right = ggplot2::element_text(angle = -90, margin = ggplot2::margin(
          l = rel_small * font_size / 4
        ), vjust = 0))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_text(margin = ggplot2::margin(
        t = rel_small * font_size / 4
      ), vjust = 1))
      ret <- ret + ggplot2::theme(axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(
        b = rel_small * font_size / 4
      ), vjust = 0))
      ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(
        r = rel_small * font_size / 4
      ), vjust = 1))
      ret <- ret + ggplot2::theme(axis.title.y.right = ggplot2::element_text(angle = -90, margin = ggplot2::margin(
        l = rel_small * font_size / 4
      ), vjust = 0))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.title = ggplot2::element_blank())
  }

  ## establishment of formatting for axis ticks
  if (inherits(axis_ticks, "character") | axis_ticks == TRUE) {
    ret <- ret + ggplot2::theme(axis.ticks.length = grid::unit(half_line / 2, "pt"))
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_line(color = dark_grey, size = line_size / 2))
    if (inherits(axis_ticks, "character")) {
      axis_ticks <- tolower(axis_ticks)
      if (regexpr("x", axis_ticks)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_line(color = dark_grey, size = line_size / 2))
      }
      if (regexpr("y", axis_ticks)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_line(color = dark_grey, size = line_size / 2))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_line(color = dark_grey, size = line_size / 2))
      ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_line(color = dark_grey, size = line_size / 2))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.ticks = element_blank())
  }

  ## establishment of formatting for strip text(i.e. heading above faceted plots)
  ret <- ret + ggplot2::theme(strip.text = ggplot2::element_text(
    hjust = 0, size = font_size,
    margin = ggplot2::margin(half_line / 2, half_line / 2, half_line / 2, half_line / 2)
  ))

  ## establishment of formatting for title
  ret <- ret + ggplot2::theme(plot.title = ggplot2::element_text(
    size = rel(rel_xlarge), color = "black", face = "bold",
    hjust = 0, vjust = -1,
    margin = ggplot2::margin(b = font_size),
  ))

  ## establishment of formatting for subtitle
  ret <- ret + ggplot2::theme(plot.subtitle = ggplot2::element_text(
    size = rel(rel_large), color = dark_grey, face = "plain",
    hjust = 0, vjust = -1,
    margin = ggplot2::margin(t = -half_line, b = font_size * rel_large)
  ))
  ret <- ret + ggplot2::theme(plot.title.position = "plot")

  ## establishment of formatting for caption (i.e. footnote or source text)
  ret <- ret + ggplot2::theme(plot.caption = ggplot2::element_text(
    size = rel(rel_small), color = medium_grey,
    hjust = 0, vjust = 0,
    margin = ggplot2::margin(t = half_line)
  ))
  ret <- ret + ggplot2::theme(plot.caption.position = "plot")

  ## establishment of formatting for tags (i.e. figure label)
  ret <- ret + ggplot2::theme(plot.tag = ggplot2::element_text(
    size = rel(rel_large), color = "black",
    hjust = 0, vjust = 0,
    margin = ggplot2::margin(t = half_line)
  ))
  ret <- ret + ggplot2::theme(plot.tag.position = "topleft")

  ## establishment of formatting for plot background
  ret <- ret + ggplot2::theme(plot.background = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(plot.margin = ggplot2::margin(rel_small, rel_small, rel_small, rel_small))

  ## establishment of formatting for font sizing
  class(ret) <- c("conditional_wfp_theme", class(ret))
  attr(ret, "font_size") <- font_size
  ret
}

## Creates a function that updates the default geom font family to Open Sans ----

#' update_geom_font_defaults
#'
#' Updates the default font family, face, size, and color for text and label geoms in ggplot2.
#'
#' @param family Character. Font family to use. Default is "Open Sans".
#' @param face Character. Font face to use (e.g., "plain", "bold"). Default is "plain".
#' @param size Numeric. Font size to use. Default is 3.5.
#' @param color Character. Font color to use. Default is "#191919" (dark grey).
#'
#' @return Updates the default settings for text and label geoms in ggplot2.
#' @export
update_geom_font_defaults <- function(family = "Open Sans", face = "plain", size = 3.5,
                                      color = "#191919") {
  ggplot2::update_geom_defaults("text", list(family = family, face = face, size = size, color = color))
  ggplot2::update_geom_defaults("label", list(family = family, face = face, size = size, color = color))
}

