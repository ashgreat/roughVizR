drop_nulls <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

sanitize_common_column <- function(x) {
  if (is.factor(x)) {
    return(as.character(x))
  }
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return(format(x, tz = "UTC"))
  }
  if (is.list(x)) {
    stop("List-columns are not supported for roughViz charts.", call. = FALSE)
  }
  x
}

sanitize_data_frame <- function(data) {
  data[] <- lapply(data, sanitize_common_column)
  as.data.frame(data, stringsAsFactors = FALSE)
}

ensure_columns <- function(data, columns, chart_type) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(
      sprintf(
        "%s chart requires column%s: %s",
        chart_type,
        if (length(missing) > 1) "s" else "",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

coerce_numeric_column <- function(x, column, chart_type) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  if (is.logical(x)) {
    return(as.numeric(x))
  }
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return(as.numeric(x))
  }
  if (is.factor(x) || is.character(x)) {
    suppressWarnings(num <- as.numeric(as.character(x)))
    if (any(is.na(num) & !is.na(x))) {
      stop(
        sprintf(
          "Column '%s' in %s chart must be numeric after coercion.",
          column,
          chart_type
        ),
        call. = FALSE
      )
    }
    return(num)
  }
  stop(
    sprintf("Column '%s' in %s chart must be numeric.", column, chart_type),
    call. = FALSE
  )
}

sanitize_axis_column <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
    return(format(x, tz = "UTC"))
  }
  if (is.factor(x)) {
    return(as.character(x))
  }
  x
}

estimate_label_margin <- function(labels, base_margin = 150, character_width = 8, padding = 24) {
  if (!length(labels)) {
    return(base_margin)
  }
  widths <- nchar(labels, type = "width")
  widths[!is.finite(widths)] <- 0
  target <- padding + character_width * max(widths)
  max(base_margin, target)
}

adjust_left_margin_for_labels <- function(margin, labels) {
  margin$left <- max(margin$left %||% 0, estimate_label_margin(labels))
  margin
}

#' Create a roughViz widget
#'
#' @description
#' Low level constructor that powers all exported chart helpers in
#' **roughVizR**. Most users will call a convenience wrapper such as
#' [roughBar()] or [roughScatter()], but this function can be handy when you
#' already have a fully formed configuration list.
#'
#' @param chartType Character string naming the roughViz.js chart to render
#'   (e.g. `"Bar"`, `"Pie"`, `"Scatter"`). See <https://github.com/jwilber/roughViz>
#'   for the complete catalogue.
#' @param config Named list describing the chart configuration. The list should
#'   normally include a `data` entry (either a `data.frame` or a nested list
#'   matching the roughViz.js API) along with any visual options accepted by the
#'   corresponding JavaScript chart.
#' @param width Optional width passed to [htmlwidgets::createWidget()]. Supply a
#'   numeric value (pixels) or valid CSS units (character strings). When omitted
#'   the widget scales responsively.
#' @param height Optional height passed to [htmlwidgets::createWidget()]. Supply
#'   a numeric value (pixels) or valid CSS units (character strings). When
#'   omitted the widget scales responsively.
#' @param elementId Optional DOM id to target when embedding the widget in a
#'   larger document.
#'
#' @return An [`htmlwidgets::htmlwidget`] ready for printing or embedding.
#'
#' @details
#' The helper functions exported by the package build up the `config` list for
#' you, validating column names and coercing values where appropriate. When
#' calling `roughViz()` directly it is your responsibility to follow the
#' roughViz.js schema. Any additional arguments are ignored; use the `config`
#' list to supply custom settings.
#'
#' @section Passing additional options:
#' Every chart accepts a `...` argument in its helper that is appended to the
#' configuration list after validation. This makes it possible to reach new
#' options introduced by roughViz.js without waiting for a package update.
#'
#' @import htmlwidgets
#'
#' @examples
#' if (interactive()) {
#'   roughViz(
#'     chartType = "Bar",
#'     config = list(
#'       data = list(
#'         labels = c("A", "B", "C"),
#'         values = c(3, 5, 2)
#'       ),
#'       title = "Quick sketch"
#'     )
#'   )
#' }
#'
#' @export
roughViz <- function(chartType, config, width = NULL, height = NULL, elementId = NULL) {

  # Create widget
  x <- list(
    chartType = chartType,
    config = config
  )

  # Create widget
  htmlwidgets::createWidget(
    name = 'roughVizR',
    x,
    width = width,
    height = height,
    package = 'roughVizR',
    elementId = elementId
  )
}

#' Create a rough bar chart
#'
#' @description
#' Draws a hand-sketched vertical bar chart using the roughViz.js engine.
#'
#' @param data A `data.frame` (preferred) or named list containing the data to
#'   visualise. When supplying a data frame you must also provide the column
#'   names in `labels` and `values`. When supplying a list it should already
#'   contain `labels` and `values` elements compatible with roughViz.js.
#' @param labels Column name (string) that identifies the x-axis labels when
#'   `data` is a data frame.
#' @param values Column name (string) that identifies the numeric values when
#'   `data` is a data frame.
#' @param title Optional chart title rendered above the plot.
#' @param width Optional width of the widget; accepts numbers (pixels) or CSS
#'   length strings.
#' @param height Optional height of the widget; accepts numbers (pixels) or CSS
#'   length strings.
#' @param color Fill colour for the bars.
#' @param highlight Highlight colour used for bar outlines and hover effects.
#' @param roughness Numeric sketchiness factor used by rough.js (`0` for crisp
#'   lines, larger values for wobble).
#' @param fillStyle Character string describing the hatch pattern. Common values
#'   include `"hachure"`, `"cross-hatch"`, `"zigzag"`, `"dashed"`, `"solid"`, and
#'   `"zigzag-line"`.
#' @param fillWeight Thickness of the fill pattern strokes (numeric); `NULL`
#'   lets roughViz.js pick a default.
#' @param strokeWidth Width of the bar outlines in pixels.
#' @param bowing Amount of curvature applied to straight lines.
#' @param simplification Numeric tolerance used to simplify curves.
#' @param font Either `0` (Gaegu), `1` (Indie Flower), or the name of a custom
#'   font family available in the page.
#' @param margin Named list with entries `top`, `right`, `bottom`, and `left`
#'   controlling whitespace around the canvas. Leave `NULL` to rely on sensible
#'   defaults that expand automatically for long labels.
#' @param axisFontSize Font size used for axis labels (CSS units or numbers).
#' @param axisRoughness Roughness setting applied to the axes.
#' @param axisStrokeWidth Stroke width for the axes.
#' @param innerStrokeWidth Numeric stroke width for bar interiors.
#' @param padding Space between bars expressed as a proportion of bar width.
#' @param titleAlign Horizontal alignment for the title. One of `"left"`,
#'   `"center"`, or `"right"`.
#' @param ... Additional named options passed straight through to roughViz.js
#'   after validation. Use this to experiment with new features that have not
#'   yet been wrapped explicitly.
#'
#' @details
#' The function sanitises the supplied data frame, ensuring that categorical
#' labels are converted to character vectors and values are coerced to numeric.
#' Missing columns trigger an informative error. Any `NULL` entries are dropped
#' from the configuration before rendering so you can safely leave options
#' unspecified.
#'
#' @return An interactive bar chart `htmlwidget` that can be printed in the R
#'   console, knitted documents, or Shiny applications.
#'
#' @examples
#' if (interactive()) {
#'   library(roughVizR)
#'
#'   sales <- data.frame(
#'     region = c("North", "South", "East", "West"),
#'     revenue = c(12, 9, 15, 6)
#'   )
#'
#'   roughBar(
#'     data = sales,
#'     labels = "region",
#'     values = "revenue",
#'     title = "Quarterly revenue",
#'     color = "coral",
#'     highlight = "#ffa07a",
#'     padding = 0.2
#'   )
#' }
#'
#' @export
roughBar <- function(data,
                     labels = NULL,
                     values = NULL,
                     title = NULL,
                     width = NULL,
                     height = NULL,
                     color = "coral",
                     highlight = "coral",
                     roughness = 1,
                     fillStyle = "hachure",
                     fillWeight = NULL,
                     strokeWidth = 1,
                     bowing = 0,
                     simplification = 0.2,
                     font = 0,
                     margin = NULL,
                     axisFontSize = "1rem",
                     axisRoughness = 0.5,
                     axisStrokeWidth = 0.5,
                     innerStrokeWidth = 0,
                     padding = 0.1,
                     titleAlign = "center",
                     ...) {

  if (length(color) > 1) {
    warning("`color` must be a single color value; using the first entry.", call. = FALSE)
    color <- color[[1]]
  }

  margin <- margin %||% list(top = 50, right = 20, bottom = 70, left = 150)

  config <- list(
    data = data,
    title = title,
    color = color,
    highlight = highlight,
    roughness = roughness,
    fillStyle = fillStyle,
    fillWeight = fillWeight,
    strokeWidth = strokeWidth,
    bowing = bowing,
    simplification = simplification,
    font = font,
    margin = margin,
    axisFontSize = axisFontSize,
    axisRoughness = axisRoughness,
    axisStrokeWidth = axisStrokeWidth,
    innerStrokeWidth = innerStrokeWidth,
    padding = padding,
    titleAlign = titleAlign,
    ...
  )
  if (is.data.frame(data)) {
    if (is.null(labels) || is.null(values)) {
      stop("'labels' and 'values' must be provided when supplying a data frame to roughBar().", call. = FALSE)
    }

    data <- sanitize_data_frame(data)
    ensure_columns(data, c(labels, values), "Bar")
    label_series <- as.character(data[[labels]])
    value_series <- coerce_numeric_column(data[[values]], values, "Bar")

    config$data <- list(
      labels = label_series,
      values = value_series
    )
  }

  config <- drop_nulls(config)

  roughViz("Bar", config, width = width, height = height)
}

#' Create a rough horizontal bar chart
#'
#' @description
#' Horizontal variant of [roughBar()] that automatically swaps the axes and
#' expands the left margin when labels are wide.
#'
#' @inheritParams roughBar
#'
#' @return An `htmlwidget` displaying a horizontal bar chart.
#'
#' @examples
#' if (interactive()) {
#'   cars <- transform(mtcars, model = rownames(mtcars))
#'   roughBarH(
#'     data = cars,
#'     labels = "model",
#'     values = "mpg",
#'     title = "Fuel economy (mpg)",
#'     color = "skyblue"
#'   )
#' }
#'
#' @export
roughBarH <- function(data,
                      labels = NULL,
                      values = NULL,
                      title = NULL,
                      width = NULL,
                      height = NULL,
                      color = "pink",
                      highlight = "pink",
                      roughness = 1,
                      fillStyle = "hachure",
                      fillWeight = NULL,
                      strokeWidth = 1,
                      bowing = 0,
                      simplification = 0.2,
                      font = 0,
                      margin = NULL,
                      axisFontSize = "1rem",
                      axisRoughness = 0.5,
                      axisStrokeWidth = 0.5,
                      innerStrokeWidth = 0,
                      padding = 0.1,
                      titleAlign = "center",
                      ...) {

  margin_missing <- missing(margin)

  if (length(color) > 1) {
    warning("`color` must be a single color value; using the first entry.", call. = FALSE)
    color <- color[[1]]
  }

  user_margin <- margin
  margin <- margin %||% list(top = 50, right = 20, bottom = 70, left = 150)

  config <- list(
    data = data,
    title = title,
    color = color,
    highlight = highlight,
    roughness = roughness,
    fillStyle = fillStyle,
    fillWeight = fillWeight,
    strokeWidth = strokeWidth,
    bowing = bowing,
    simplification = simplification,
    font = font,
    margin = margin,
    axisFontSize = axisFontSize,
    axisRoughness = axisRoughness,
    axisStrokeWidth = axisStrokeWidth,
    innerStrokeWidth = innerStrokeWidth,
    padding = padding,
    titleAlign = titleAlign,
    ...
  )
  if (is.data.frame(data)) {
    if (is.null(labels) || is.null(values)) {
      stop("'labels' and 'values' must be provided when supplying a data frame to roughBarH().", call. = FALSE)
    }

    data <- sanitize_data_frame(data)
    ensure_columns(data, c(labels, values), "BarH")
    label_series <- as.character(data[[labels]])
    value_series <- coerce_numeric_column(data[[values]], values, "BarH")

    config$data <- list(
      labels = label_series,
      values = value_series
    )

    if (is.null(user_margin)) {
      config$margin <- adjust_left_margin_for_labels(config$margin, label_series)
    }
  }

  config <- drop_nulls(config)

  roughViz("BarH", config, width = width, height = height)
}

#' Create a rough donut chart
#'
#' @description
#' Produces a filled donut chart with the characteristic roughViz hand-drawn
#' style.
#'
#' @param data A `data.frame` or named list. With a data frame you must supply
#'   column names via `labels` and `values`. With a list the expected structure
#'   is `list(labels = <character>, values = <numeric>)`.
#' @param labels String giving the column name used for labels when `data` is a
#'   data frame.
#' @param values String naming the column that contains numeric values when
#'   `data` is a data frame.
#' @param title Optional chart heading.
#' @param width Optional width of the widget (pixels or CSS lengths).
#' @param height Optional height of the widget (pixels or CSS lengths).
#' @param colors Character vector of fill colours. Leave `NULL` to let the
#'   library choose defaults.
#' @param roughness Roughness level forwarded to roughViz.js.
#' @param fillStyle Fill style, such as `"hachure"` or `"zigzag"`.
#' @param fillWeight Thickness of the fill pattern strokes.
#' @param strokeWidth Width of the slice outlines.
#' @param bowing Amount of curvature applied to straight lines.
#' @param simplification Simplification tolerance for curves.
#' @param font Font family reference (`0`, `1`, or custom name).
#' @param margin Named list describing the plot margins.
#' @param legend Logical flag controlling whether the legend is rendered.
#' @param legendPosition Position of the legend (`"top"`, `"bottom"`, `"left"`,
#'   or `"right"`).
#' @param titleAlign Horizontal alignment of the title (`"left"`, `"center"`,
#'   or `"right"`).
#' @param ... Additional named options forwarded to the underlying JavaScript
#'   widget.
#'
#' @details
#' When a data frame is supplied, the function sanitises the columns and ensures
#' that the value column is numeric after coercion. Helpful error messages are
#' raised if mandatory columns are missing.
#'
#' @return A donut chart `htmlwidget`.
#'
#' @examples
#' if (interactive()) {
#'   categories <- data.frame(
#'     segment = c("Marketing", "R&D", "Operations", "HR"),
#'     spend = c(25, 30, 28, 17)
#'   )
#'
#'   roughDonut(
#'     data = categories,
#'     labels = "segment",
#'     values = "spend",
#'     title = "Budget allocation",
#'     colors = c("#ff6b6b", "#4ecdc4", "#45b7d1", "#f9ca24"),
#'     legendPosition = "bottom"
#'   )
#' }
#'
#' @export
roughDonut <- function(data,
                       labels = NULL,
                       values = NULL,
                       title = NULL,
                       width = NULL,
                       height = NULL,
                       colors = NULL,
                       roughness = 1,
                       fillStyle = "hachure",
                       fillWeight = NULL,
                       strokeWidth = 1,
                       bowing = 0,
                       simplification = 0,
                       font = 0,
                       margin = NULL,
                       legend = TRUE,
                       legendPosition = "right",
                       titleAlign = "center",
                       ...) {

  margin <- margin %||% list(top = 50, right = 20, bottom = 70, left = 100)

  config <- list(
    data = data,
    title = title,
    colors = colors,
    roughness = roughness,
    fillStyle = fillStyle,
    fillWeight = fillWeight,
    strokeWidth = strokeWidth,
    bowing = bowing,
    simplification = simplification,
    font = font,
    margin = margin,
    legend = legend,
    legendPosition = legendPosition,
    titleAlign = titleAlign,
    ...
  )
  if (is.data.frame(data)) {
    if (is.null(labels) || is.null(values)) {
      stop("'labels' and 'values' must be provided when supplying a data frame to roughDonut().", call. = FALSE)
    }

    data <- sanitize_data_frame(data)
    ensure_columns(data, c(labels, values), "Donut")
    config$data <- list(
      labels = as.character(data[[labels]]),
      values = coerce_numeric_column(data[[values]], values, "Donut")
    )
  }

  config <- drop_nulls(config)

  roughViz("Donut", config, width = width, height = height)
}

#' Create a rough pie chart
#'
#' @description
#' Thin wrapper around [roughDonut()] that renders slices without the hollow
#' centre.
#'
#' @inheritParams roughDonut
#'
#' @return A pie chart `htmlwidget`.
#'
#' @examples
#' if (interactive()) {
#'   roughPie(
#'     data = data.frame(
#'       labels = c("A", "B", "C"),
#'       values = c(40, 35, 25)
#'     ),
#'     labels = "labels",
#'     values = "values",
#'     title = "Share of responses"
#'   )
#' }
#'
#' @export
roughPie <- function(data,
                     labels = NULL,
                     values = NULL,
                     title = NULL,
                     width = NULL,
                     height = NULL,
                     colors = NULL,
                     roughness = 1,
                     fillStyle = "hachure",
                     fillWeight = NULL,
                     strokeWidth = 1,
                     bowing = 0,
                     simplification = 0,
                     font = 0,
                     margin = NULL,
                     legend = TRUE,
                     legendPosition = "right",
                     titleAlign = "center",
                     ...) {

  margin <- margin %||% list(top = 50, right = 20, bottom = 70, left = 100)

  config <- list(
    data = data,
    title = title,
    colors = colors,
    roughness = roughness,
    fillStyle = fillStyle,
    fillWeight = fillWeight,
    strokeWidth = strokeWidth,
    bowing = bowing,
    simplification = simplification,
    font = font,
    margin = margin,
    legend = legend,
    legendPosition = legendPosition,
    titleAlign = titleAlign,
    ...
  )
  if (is.data.frame(data)) {
    if (is.null(labels) || is.null(values)) {
      stop("'labels' and 'values' must be provided when supplying a data frame to roughPie().", call. = FALSE)
    }

    data <- sanitize_data_frame(data)
    ensure_columns(data, c(labels, values), "Pie")
    config$data <- list(
      labels = as.character(data[[labels]]),
      values = coerce_numeric_column(data[[values]], values, "Pie")
    )
  }

  config <- drop_nulls(config)

  roughViz("Pie", config, width = width, height = height)
}

#' Create a rough line chart
#'
#' @description
#' Renders one or more sketchy line series with optional point markers.
#'
#' @param data A `data.frame` or list with numerical series. When working with a
#'   data frame you can point to up to three value columns using `y`, `y2`, and
#'   `y3`. Any additional numeric columns (apart from `x`) are automatically
#'   detected when those arguments are omitted.
#' @param x Optional column name supplying the x-axis values.
#' @param y Column name of the primary series to plot.
#' @param y2 Optional column name containing a second series.
#' @param y3 Optional column name containing a third series.
#' @param title Optional title displayed above the chart.
#' @param width Optional width of the widget (numeric pixels or CSS units).
#' @param height Optional height of the widget (numeric pixels or CSS units).
#' @param colors Vector of colours to recycle across the series. Ignored when
#'   `NULL`.
#' @param roughness Roughness level forwarded to roughViz.js.
#' @param fillStyle Fill style selection such as `"hachure"` or `"cross-hatch"`.
#' @param fillWeight Thickness of the fill pattern strokes.
#' @param strokeWidth Width of the line strokes.
#' @param bowing Amount of curvature applied to straight segments.
#' @param simplification Simplification tolerance for curves.
#' @param font Font family reference (`0`, `1`, or custom name).
#' @param margin Named list defining the plot margins.
#' @param axisFontSize Font size used for axis labels (CSS units or numbers).
#' @param axisRoughness Roughness setting applied to axes.
#' @param axisStrokeWidth Axis line stroke width.
#' @param circle Logical flag indicating whether to draw point markers.
#' @param circleRadius Radius of the optional circle markers.
#' @param circleRoughness Roughness level applied to the circle markers.
#' @param titleAlign Horizontal alignment of the title (`"left"`, `"center"`,
#'   or `"right"`).
#' @param ... Additional named options forwarded to roughViz.js.
#'
#' @details
#' When a data frame is supplied the function sanitises column types, coerces
#' value series to numeric (with validation), and repackages them into the list
#' format expected by roughViz.js. Missing columns trigger informative errors.
#'
#' @return A line chart `htmlwidget`.
#'
#' @examples
#' if (interactive()) {
#'   trends <- data.frame(
#'     month = month.abb[1:6],
#'     product_a = c(10, 14, 12, 18, 17, 20),
#'     product_b = c(8, 11, 13, 16, 15, 19)
#'   )
#'
#'   roughLine(
#'     data = trends,
#'     x = "month",
#'     y = "product_a",
#'     y2 = "product_b",
#'     title = "Half-year sales trend",
#'     colors = c("#f94144", "#577590"),
#'     circle = TRUE
#'   )
#' }
#'
#' @export
roughLine <- function(data,
                      x = NULL,
                      y = NULL,
                      y2 = NULL,
                      y3 = NULL,
                      title = NULL,
                      width = NULL,
                      height = NULL,
                      colors = NULL,
                      roughness = 1,
                      fillStyle = "hachure",
                      fillWeight = NULL,
                      strokeWidth = 1,
                      bowing = 0,
                      simplification = 0.2,
                      font = 0,
                      margin = NULL,
                      axisFontSize = "1rem",
                      axisRoughness = 0.5,
                      axisStrokeWidth = 0.5,
                      circle = TRUE,
                      circleRadius = 10,
                      circleRoughness = 2,
                      titleAlign = "center",
                      ...) {

  margin <- margin %||% list(top = 50, right = 20, bottom = 70, left = 100)

  config <- list(
    data = data,
    title = title,
    colors = colors,
    roughness = roughness,
    fillStyle = fillStyle,
    fillWeight = fillWeight,
    strokeWidth = strokeWidth,
    bowing = bowing,
    simplification = simplification,
    font = font,
    margin = margin,
    axisFontSize = axisFontSize,
    axisRoughness = axisRoughness,
    axisStrokeWidth = axisStrokeWidth,
    circle = circle,
    circleRadius = circleRadius,
    circleRoughness = circleRoughness,
    titleAlign = titleAlign,
    ...
  )
  if (is.data.frame(data)) {
    data <- sanitize_data_frame(data)

    if (!is.null(x)) {
      ensure_columns(data, x, "Line")
      config$x <- sanitize_axis_column(data[[x]])
    }

    series_names <- c()
    line_data <- list()

    series_args <- c(y, y2, y3)
    series_args <- series_args[!vapply(series_args, is.null, logical(1))]

    if (length(series_args)) {
      ensure_columns(data, series_args, "Line")
      for (col in series_args) {
        line_data[[col]] <- coerce_numeric_column(data[[col]], col, "Line")
        series_names <- c(series_names, col)
      }
    }

    if (!length(line_data)) {
      x_col <- if (!is.null(x)) x else character()
      numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]
      numeric_cols <- setdiff(numeric_cols, x_col)
      if (!length(numeric_cols)) {
        stop("Line chart requires at least one numeric column for y values.", call. = FALSE)
      }
      for (col in numeric_cols) {
        line_data[[col]] <- coerce_numeric_column(data[[col]], col, "Line")
        series_names <- c(series_names, col)
      }
    }

    config$data <- line_data
    if (length(series_names) == 1) {
      config$y <- series_names[[1]]
    } else if (length(series_names) >= 2) {
      config$y <- series_names[[1]]
      config$y2 <- series_names[[2]]
      if (length(series_names) >= 3) {
        config$y3 <- series_names[[3]]
      }
    }
  }

  config <- drop_nulls(config)

  if ("colors" %in% names(config) && is.list(config$colors) && !length(config$colors)) {
    config$colors <- NULL
  }

  roughViz("Line", config, width = width, height = height)
}

#' Create a rough scatter plot
#'
#' @description
#' Displays points with an optional colour grouping and highlight label.
#'
#' @param data A `data.frame` or list containing the x/y coordinates. When
#'   passing a data frame you must also provide the column names in `x` and `y`.
#' @param x String naming the column that holds the x values.
#' @param y String naming the column that holds the y values.
#' @param title Optional title placed above the chart.
#' @param width Optional width of the widget (numeric pixels or CSS units).
#' @param height Optional height of the widget (numeric pixels or CSS units).
#' @param colorVar Optional column name indicating a grouping variable used for
#'   colouring the points.
#' @param colors Character vector of colours corresponding to the distinct
#'   levels in `colorVar`.
#' @param highlightLabel Optional label that should be accentuated using the
#'   `highlight` colour.
#' @param roughness Roughness level forwarded to roughViz.js.
#' @param fillStyle Fill style for the points.
#' @param fillWeight Thickness of the fill pattern strokes.
#' @param strokeWidth Outline stroke width for the points.
#' @param bowing Amount of curvature applied to straight segments.
#' @param simplification Simplification tolerance for shapes.
#' @param font Font family reference (`0`, `1`, or custom name).
#' @param margin Named list describing widget margins.
#' @param axisFontSize Font size used for axis labels (CSS units or numbers).
#' @param axisRoughness Roughness setting applied to axes.
#' @param axisStrokeWidth Axis line stroke width.
#' @param radius Circle radius for the points.
#' @param innerStrokeWidth Stroke width used inside points.
#' @param titleAlign Horizontal alignment for the title.
#' @param ... Additional named options forwarded to roughViz.js.
#'
#' @details
#' When a data frame is supplied the function verifies that the referenced
#' columns exist, sanitises their types, and reshapes them into the column-based
#' structure that roughViz.js expects (`x`/`y` vectors plus any additional
#' fields such as `colorVar`).
#'
#' @return A scatter plot `htmlwidget`.
#'
#' @examples
#' if (interactive()) {
#'   roughScatter(
#'     data = iris,
#'     x = "Sepal.Length",
#'     y = "Petal.Length",
#'     colorVar = "Species",
#'     title = "Iris measurements",
#'     radius = 10
#'   )
#' }
#'
#' @export
roughScatter <- function(data,
                         x = NULL,
                         y = NULL,
                         title = NULL,
                         width = NULL,
                         height = NULL,
                         colorVar = NULL,
                         colors = NULL,
                         highlightLabel = NULL,
                         roughness = 1,
                         fillStyle = "hachure",
                         fillWeight = 0.5,
                         strokeWidth = 1,
                         bowing = 0,
                         simplification = 0.2,
                         font = 0,
                         margin = NULL,
                         axisFontSize = "1rem",
                         axisRoughness = 0.5,
                         axisStrokeWidth = 0.5,
                         radius = 8,
                         innerStrokeWidth = 1,
                         titleAlign = "center",
                         ...) {

  margin <- margin %||% list(top = 50, right = 20, bottom = 70, left = 100)

  config <- list(
    data = data,
    title = title,
    colors = colors,
    highlightLabel = highlightLabel,
    roughness = roughness,
    fillStyle = fillStyle,
    fillWeight = fillWeight,
    strokeWidth = strokeWidth,
    bowing = bowing,
    simplification = simplification,
    font = font,
    margin = margin,
    axisFontSize = axisFontSize,
    axisRoughness = axisRoughness,
    axisStrokeWidth = axisStrokeWidth,
    radius = radius,
    innerStrokeWidth = innerStrokeWidth,
    titleAlign = titleAlign,
    ...
  )
  if (is.data.frame(data)) {
    data <- sanitize_data_frame(data)
    if (is.null(x) || is.null(y)) {
      stop("'x' and 'y' must be provided when supplying a data frame to roughScatter().", call. = FALSE)
    }

    ensure_columns(data, c(x, y), "Scatter")
    if (!is.null(colorVar)) {
      ensure_columns(data, colorVar, "Scatter")
    }

    scatter_data <- list(
      x = coerce_numeric_column(data[[x]], x, "Scatter"),
      y = coerce_numeric_column(data[[y]], y, "Scatter")
    )

    if (!is.null(colorVar)) {
      scatter_data[[colorVar]] <- data[[colorVar]]
      config$colorVar <- colorVar
    }

    extra_cols <- setdiff(names(data), c(x, y, colorVar))
    if (length(extra_cols)) {
      for (col in extra_cols) {
        scatter_data[[col]] <- data[[col]]
      }
    }

    if (!is.null(config$highlightLabel) && !(config$highlightLabel %in% names(scatter_data))) {
      warning(sprintf("`highlightLabel` column '%s' was not found; disabling point highlight.", config$highlightLabel), call. = FALSE)
      config$highlightLabel <- NULL
    }

    config$data <- scatter_data
  }

  config <- drop_nulls(config)

  roughViz("Scatter", config, width = width, height = height)
}

#' Create a rough stacked bar chart
#'
#' @description
#' Displays stacked columns suitable for showing parts-of-a-whole across
#' categories.
#'
#' @param data A `data.frame` or named list. With a data frame one column should
#'   contain the category labels while the remaining numeric columns represent
#'   the individual stacks.
#' @param labels Column name that holds the category labels when `data` is a
#'   data frame.
#' @param title Optional heading.
#' @param width Optional width of the widget (numeric pixels or CSS units).
#' @param height Optional height of the widget (numeric pixels or CSS units).
#' @param colors Character vector of fill colours recycled across the stacks.
#' @param roughness Roughness level forwarded to roughViz.js.
#' @param fillStyle Fill style for the stack segments.
#' @param fillWeight Thickness of the fill pattern strokes.
#' @param strokeWidth Outline stroke width.
#' @param bowing Amount of curvature applied to straight segments.
#' @param simplification Simplification tolerance for shapes.
#' @param font Font family reference (`0`, `1`, or custom name).
#' @param margin Named list of margins around the plot.
#' @param axisFontSize Font size used for axis labels (CSS units or numbers).
#' @param axisRoughness Roughness setting applied to axes.
#' @param axisStrokeWidth Axis line stroke width.
#' @param innerStrokeWidth Stroke width drawn inside the stacked bars.
#' @param padding Gap between categories (0-1 range).
#' @param titleAlign Horizontal alignment of the title (`"left"`, `"center"`,
#'   or `"right"`).
#' @param ... Additional named options forwarded to roughViz.js.
#'
#' @details
#' When `data` is a data frame the function verifies the presence of `labels`
#' and at least one additional numeric column. It then coerces those numeric
#' columns to double precision before handing the data to roughViz.js.
#'
#' @return A stacked bar `htmlwidget`.
#'
#' @examples
#' if (interactive()) {
#'   revenue <- data.frame(
#'     quarter = c("Q1", "Q2", "Q3", "Q4"),
#'     hardware = c(5, 6, 4, 7),
#'     software = c(3, 4, 6, 5),
#'     services = c(2, 3, 2, 4)
#'   )
#'
#'   roughStackedBar(
#'     data = revenue,
#'     labels = "quarter",
#'     title = "Product mix by quarter",
#'     colors = c("#90be6d", "#f9c74f", "#f94144")
#'   )
#' }
#'
#' @export
roughStackedBar <- function(data,
                            labels = NULL,
                            title = NULL,
                            width = NULL,
                            height = NULL,
                            colors = NULL,
                            roughness = 1,
                            fillStyle = "hachure",
                            fillWeight = NULL,
                            strokeWidth = 1,
                            bowing = 0,
                            simplification = 0.2,
                            font = 0,
                            margin = NULL,
                            axisFontSize = "1rem",
                            axisRoughness = 0.5,
                            axisStrokeWidth = 0.5,
                            innerStrokeWidth = 0,
                            padding = 0.1,
                            titleAlign = "center",
                            ...) {

  margin <- margin %||% list(top = 50, right = 20, bottom = 70, left = 150)

  config <- list(
    data = data,
    title = title,
    colors = colors,
    roughness = roughness,
    fillStyle = fillStyle,
    fillWeight = fillWeight,
    strokeWidth = strokeWidth,
    bowing = bowing,
    simplification = simplification,
    font = font,
    margin = margin,
    axisFontSize = axisFontSize,
    axisRoughness = axisRoughness,
    axisStrokeWidth = axisStrokeWidth,
    innerStrokeWidth = innerStrokeWidth,
    padding = padding,
    titleAlign = titleAlign,
    ...
  )
  if (is.data.frame(data)) {
    if (is.null(labels)) {
      stop("'labels' must be provided when supplying a data frame to roughStackedBar().", call. = FALSE)
    }

    data <- sanitize_data_frame(data)
    ensure_columns(data, labels, "StackedBar")

    numeric_cols <- setdiff(names(data), labels)
    if (!length(numeric_cols)) {
      stop("StackedBar chart requires at least one value column besides 'labels'.", call. = FALSE)
    }

    for (col in numeric_cols) {
      data[[col]] <- coerce_numeric_column(data[[col]], col, "StackedBar")
    }

    config$data <- data
    config$labels <- labels
  }

  config <- drop_nulls(config)

  roughViz("StackedBar", config, width = width, height = height)
}

#' Shiny output for roughViz
#'
#' @description
#' Creates a UI placeholder for a roughViz widget in a Shiny application.
#'
#' @param outputId Output slot identifier used in the Shiny UI.
#' @param width Output width. Accepts valid CSS size values such as `"100%"` or
#'   `"400px"`.
#' @param height Output height. Accepts valid CSS size values such as `"100%"`
#'   or `"400px"`.
#'
#' @return A UI definition to be used in `ui` definitions.
#'
#' @examples
#' \dontrun{
#'   ui <- fluidPage(
#'     roughVizOutput("sales_plot")
#'   )
#' }
#'
#' @export
roughVizOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'roughVizR', width, height, package = 'roughVizR')
}

#' Shiny render function for roughViz
#'
#' @param expr Expression that returns a roughViz widget.
#' @param env Environment in which to evaluate `expr`.
#' @param quoted Should the expression be treated as quoted? Set to `TRUE` when
#'   calling from another function.
#'
#' @return A server-side function suitable for use inside `output$<id> <-`.
#'
#' @examples
#' \dontrun{
#'   server <- function(input, output, session) {
#'     output$sales_plot <- renderRoughViz({
#'       roughBar(
#'         data = data.frame(
#'           labels = c("A", "B", "C"),
#'           values = c(3, 5, 2)
#'         ),
#'         labels = "labels",
#'         values = "values"
#'       )
#'     })
#'   }
#' }
#'
#' @export
renderRoughViz <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) }
  htmlwidgets::shinyRenderWidget(expr, roughVizOutput, env, quoted = TRUE)
}
