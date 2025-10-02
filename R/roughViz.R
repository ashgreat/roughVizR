drop_nulls <- function(x) {
  x[!vapply(x, is.null, logical(1))]
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
  data
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

#' Create a roughViz widget
#'
#' @param chartType The type of chart to create
#' @param config Configuration list for the chart
#' @param width Widget width
#' @param height Widget height
#' @param elementId Element ID
#'
#' @import htmlwidgets
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
#' @param data Data frame or list containing the data
#' @param labels Column name for labels (x-axis)
#' @param values Column name for values (y-axis)
#' @param title Chart title
#' @param width Chart width
#' @param height Chart height
#' @param color Bar color
#' @param highlight Highlight color
#' @param roughness Roughness level (0-10)
#' @param fillStyle Fill style ('hachure', 'cross-hatch', 'zigzag', 'dashed', 'solid', 'zigzag-line')
#' @param fillWeight Fill weight
#' @param strokeWidth Stroke width
#' @param bowing Bowing effect
#' @param simplification Simplification level
#' @param font Font style (0 for 'gaegu', 1 for 'indie flower', 2 for custom)
#' @param margin List with top, right, bottom, left margins
#' @param axisFontSize Axis font size
#' @param axisRoughness Axis roughness
#' @param axisStrokeWidth Axis stroke width
#' @param innerStrokeWidth Inner stroke width
#' @param padding Padding between bars
#' @param titleAlign Title alignment ('left', 'center', 'right')
#' @param ... Additional configuration options
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
                     margin = list(top = 50, right = 20, bottom = 70, left = 150),
                     axisFontSize = "1rem",
                     axisRoughness = 0.5,
                     axisStrokeWidth = 0.5,
                     innerStrokeWidth = 0,
                     padding = 0.1,
                     titleAlign = "center",
                     ...) {

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
    config$data <- data[, unique(c(labels, values)), drop = FALSE]
    config$labels <- labels
    config$values <- values
  }

  config <- drop_nulls(config)

  roughViz("Bar", config, width = width, height = height)
}

#' Create a rough horizontal bar chart
#'
#' @inheritParams roughBar
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
                      margin = list(top = 50, right = 20, bottom = 70, left = 150),
                      axisFontSize = "1rem",
                      axisRoughness = 0.5,
                      axisStrokeWidth = 0.5,
                      innerStrokeWidth = 0,
                      padding = 0.1,
                      titleAlign = "center",
                      ...) {

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
    config$data <- data[, unique(c(labels, values)), drop = FALSE]
    config$labels <- labels
    config$values <- values
  }

  config <- drop_nulls(config)

  roughViz("BarH", config, width = width, height = height)
}

#' Create a rough donut chart
#'
#' @param data Data frame or list containing the data
#' @param labels Column name for labels
#' @param values Column name for values
#' @param title Chart title
#' @param width Chart width
#' @param height Chart height
#' @param colors Array of colors
#' @param roughness Roughness level (0-10)
#' @param fillStyle Fill style
#' @param fillWeight Fill weight
#' @param strokeWidth Stroke width
#' @param bowing Bowing effect
#' @param simplification Simplification level
#' @param font Font style
#' @param margin List with margins
#' @param legend Show legend
#' @param legendPosition Legend position ('top', 'bottom', 'left', 'right')
#' @param titleAlign Title alignment ('left', 'center', 'right')
#' @param ... Additional configuration options
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
                       margin = list(top = 50, right = 20, bottom = 70, left = 100),
                       legend = TRUE,
                       legendPosition = "right",
                       titleAlign = "center",
                       ...) {

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
    config$data <- data[, unique(c(labels, values)), drop = FALSE]
    config$labels <- labels
    config$values <- values
  }

  config <- drop_nulls(config)

  roughViz("Donut", config, width = width, height = height)
}

#' Create a rough pie chart
#'
#' @inheritParams roughDonut
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
                     margin = list(top = 50, right = 20, bottom = 70, left = 100),
                     legend = TRUE,
                     legendPosition = "right",
                     titleAlign = "center",
                     ...) {

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
    config$data <- data[, unique(c(labels, values)), drop = FALSE]
    config$labels <- labels
    config$values <- values
  }

  config <- drop_nulls(config)

  roughViz("Pie", config, width = width, height = height)
}

#' Create a rough line chart
#'
#' @param data Data frame or list containing the data
#' @param x Column name for x values
#' @param y Column name for y values (can be multiple)
#' @param y2 Optional second y series
#' @param y3 Optional third y series
#' @param title Chart title
#' @param width Chart width
#' @param height Chart height
#' @param colors Array of colors for multiple lines
#' @param roughness Roughness level
#' @param fillStyle Fill style
#' @param fillWeight Fill weight
#' @param strokeWidth Stroke width
#' @param bowing Bowing effect
#' @param simplification Simplification level
#' @param font Font style
#' @param margin List with margins
#' @param axisFontSize Axis font size
#' @param axisRoughness Axis roughness
#' @param axisStrokeWidth Axis stroke width
#' @param circle Show circles at data points
#' @param circleRadius Circle radius
#' @param circleRoughness Circle roughness
#' @param ... Additional configuration options
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
                      margin = list(top = 50, right = 20, bottom = 70, left = 100),
                      axisFontSize = "1rem",
                      axisRoughness = 0.5,
                      axisStrokeWidth = 0.5,
                      circle = TRUE,
                      circleRadius = 10,
                      circleRoughness = 2,
                      titleAlign = "center",
                      ...) {

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
#' @param data Data frame or list containing the data
#' @param x Column name for x values
#' @param y Column name for y values
#' @param title Chart title
#' @param width Chart width
#' @param height Chart height
#' @param colorVar Column name for color variable
#' @param colors Array of colors
#' @param highlightLabel Label to highlight
#' @param roughness Roughness level
#' @param fillStyle Fill style
#' @param fillWeight Fill weight
#' @param strokeWidth Stroke width
#' @param bowing Bowing effect
#' @param simplification Simplification level
#' @param font Font style
#' @param margin List with margins
#' @param axisFontSize Axis font size
#' @param axisRoughness Axis roughness
#' @param axisStrokeWidth Axis stroke width
#' @param radius Circle radius
#' @param innerStrokeWidth Inner stroke width
#' @param ... Additional configuration options
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
                         margin = list(top = 50, right = 20, bottom = 70, left = 100),
                         axisFontSize = "1rem",
                         axisRoughness = 0.5,
                         axisStrokeWidth = 0.5,
                         radius = 8,
                         innerStrokeWidth = 1,
                         titleAlign = "center",
                         ...) {

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

    config$data <- data
    config$x <- x
    config$y <- y
    if (!is.null(colorVar)) {
      config$colorVar <- colorVar
    }
  }

  config <- drop_nulls(config)

  roughViz("Scatter", config, width = width, height = height)
}

#' Create a rough stacked bar chart
#'
#' @param data Data frame or list containing the data
#' @param labels Column name for labels
#' @param title Chart title
#' @param width Chart width
#' @param height Chart height
#' @param colors Array of colors
#' @param roughness Roughness level
#' @param fillStyle Fill style
#' @param fillWeight Fill weight
#' @param strokeWidth Stroke width
#' @param bowing Bowing effect
#' @param simplification Simplification level
#' @param font Font style
#' @param margin List with margins
#' @param axisFontSize Axis font size
#' @param axisRoughness Axis roughness
#' @param axisStrokeWidth Axis stroke width
#' @param innerStrokeWidth Inner stroke width
#' @param padding Padding between bars
#' @param ... Additional configuration options
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
                            margin = list(top = 50, right = 20, bottom = 70, left = 150),
                            axisFontSize = "1rem",
                            axisRoughness = 0.5,
                            axisStrokeWidth = 0.5,
                            innerStrokeWidth = 0,
                            padding = 0.1,
                            titleAlign = "center",
                            ...) {

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
#' @param outputId Output ID
#' @param width Width
#' @param height Height
#'
#' @export
roughVizOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'roughVizR', width, height, package = 'roughVizR')
}

#' Shiny render function for roughViz
#'
#' @param expr Expression to render
#' @param env Environment
#' @param quoted Is expression quoted
#'
#' @export
renderRoughViz <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) }
  htmlwidgets::shinyRenderWidget(expr, roughVizOutput, env, quoted = TRUE)
}
