#' Create a roughViz widget
#'
#' @param chartType The type of chart to create
#' @param config Configuration list for the chart
#' @param width Widget width
#' @param height Widget height
#' @param elementId Element ID
#'
#' @import htmlwidgets
#' @importFrom jsonlite toJSON
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
    name = 'roughViz',
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
                     margin = list(top = 50, right = 20, bottom = 70, left = 100),
                     axisFontSize = "1rem",
                     axisRoughness = 0.5,
                     axisStrokeWidth = 0.5,
                     innerStrokeWidth = 0,
                     padding = 0.1,
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
    ...
  )
  
  # Handle data frame input
  if (is.data.frame(data)) {
    if (!is.null(labels) && !is.null(values)) {
      config$labels = labels
      config$values = values
    }
  }
  
  # Remove NULL values
  config <- config[!sapply(config, is.null)]
  
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
                      margin = list(top = 50, right = 20, bottom = 70, left = 100),
                      axisFontSize = "1rem",
                      axisRoughness = 0.5,
                      axisStrokeWidth = 0.5,
                      innerStrokeWidth = 0,
                      padding = 0.1,
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
    ...
  )
  
  # Handle data frame input
  if (is.data.frame(data)) {
    if (!is.null(labels) && !is.null(values)) {
      config$labels = labels
      config$values = values
    }
  }
  
  # Remove NULL values
  config <- config[!sapply(config, is.null)]
  
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
    ...
  )
  
  # Handle data frame input
  if (is.data.frame(data)) {
    if (!is.null(labels) && !is.null(values)) {
      config$labels = labels
      config$values = values
    }
  }
  
  # Remove NULL values
  config <- config[!sapply(config, is.null)]
  
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
    ...
  )
  
  # Handle data frame input
  if (is.data.frame(data)) {
    if (!is.null(labels) && !is.null(values)) {
      config$labels = labels
      config$values = values
    }
  }
  
  # Remove NULL values
  config <- config[!sapply(config, is.null)]
  
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
    ...
  )
  
  # Handle data frame input
  if (is.data.frame(data)) {
    # For line charts, we need to convert data.frame to the expected format
    # roughViz.js expects data as an object with series as keys and arrays as values
    
    # Extract x values if specified
    if (!is.null(x)) {
      if (is.character(x) && x %in% names(data)) {
        config$x = data[[x]]
      }
    }
    
    # Convert y columns to the expected format
    line_data <- list()
    
    if (!is.null(y) && is.character(y) && y %in% names(data)) {
      line_data[[y]] = as.numeric(data[[y]])
    }
    
    if (!is.null(y2) && is.character(y2) && y2 %in% names(data)) {
      line_data[[y2]] = as.numeric(data[[y2]])
    }
    
    if (!is.null(y3) && is.character(y3) && y3 %in% names(data)) {
      line_data[[y3]] = as.numeric(data[[y3]])
    }
    
    # If no y columns specified, use all numeric columns except x
    if (length(line_data) == 0) {
      x_col <- if (!is.null(x) && is.character(x)) x else NULL
      numeric_cols <- names(data)[sapply(data, is.numeric)]
      if (!is.null(x_col)) {
        numeric_cols <- numeric_cols[numeric_cols != x_col]
      }
      
      for (col in numeric_cols) {
        line_data[[col]] = as.numeric(data[[col]])
      }
    }
    
    # Use the converted data
    config$data = line_data
  }
  
  # Remove NULL values
  config <- config[!sapply(config, is.null)]
  
  # Remove colors if it's an empty list (which becomes {} in JSON)
  if ("colors" %in% names(config) && is.list(config$colors) && length(config$colors) == 0) {
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
                         fillWeight = NULL,
                         strokeWidth = 1,
                         bowing = 0,
                         simplification = 0.2,
                         font = 0,
                         margin = list(top = 50, right = 20, bottom = 70, left = 100),
                         axisFontSize = "1rem",
                         axisRoughness = 0.5,
                         axisStrokeWidth = 0.5,
                         radius = 8,
                         innerStrokeWidth = 0,
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
    ...
  )
  
  # Handle data frame input
  if (is.data.frame(data)) {
    if (!is.null(x)) config$x = x
    if (!is.null(y)) config$y = y
    if (!is.null(colorVar)) config$colorVar = colorVar
  }
  
  # Remove NULL values
  config <- config[!sapply(config, is.null)]
  
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
                            margin = list(top = 50, right = 20, bottom = 70, left = 100),
                            axisFontSize = "1rem",
                            axisRoughness = 0.5,
                            axisStrokeWidth = 0.5,
                            innerStrokeWidth = 0,
                            padding = 0.1,
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
    ...
  )
  
  # Handle data frame input
  if (is.data.frame(data)) {
    if (!is.null(labels)) {
      config$labels = labels
    }
  }
  
  # Remove NULL values
  config <- config[!sapply(config, is.null)]
  
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
  htmlwidgets::shinyWidgetOutput(outputId, 'roughViz', width, height, package = 'roughVizR')
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