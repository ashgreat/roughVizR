# Shiny Example for roughVizR R Package

library(shiny)
library(roughVizR)

ui <- fluidPage(
  titlePanel("roughViz Interactive Demo"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Chart Configuration"),
      
      selectInput("chartType", "Chart Type:",
                  choices = c("Bar", "Horizontal Bar", "Pie", "Donut", "Line", "Scatter"),
                  selected = "Bar"),
      
      sliderInput("roughness", "Roughness:",
                  min = 0, max = 10, value = 2, step = 0.5),
      
      selectInput("fillStyle", "Fill Style:",
                  choices = c("hachure", "cross-hatch", "zigzag", 
                              "dashed", "solid", "zigzag-line"),
                  selected = "hachure"),
      
      sliderInput("fillWeight", "Fill Weight:",
                  min = 0.5, max = 5, value = 1, step = 0.5),
      
      sliderInput("strokeWidth", "Stroke Width:",
                  min = 0.5, max = 5, value = 1, step = 0.5),
      
      sliderInput("bowing", "Bowing:",
                  min = 0, max = 10, value = 0, step = 1),
      
      selectInput("colorScheme", "Color Scheme:",
                  choices = c("Coral" = "coral",
                              "Sky Blue" = "skyblue",
                              "Pink" = "pink",
                              "Green" = "green",
                              "Orange" = "orange"),
                  selected = "coral"),
      
      conditionalPanel(
        condition = "input.chartType == 'Line' || input.chartType == 'Scatter'",
        checkboxInput("showCircles", "Show Circles", value = TRUE),
        sliderInput("circleRadius", "Circle Radius:",
                    min = 5, max = 20, value = 10)
      ),
      
      hr(),
      
      h4("Data Configuration"),
      
      sliderInput("dataPoints", "Number of Data Points:",
                  min = 3, max = 10, value = 5),
      
      actionButton("regenerate", "Regenerate Data", 
                   class = "btn-primary"),
      
      hr(),
      
      downloadButton("downloadChart", "Download Chart")
    ),
    
    mainPanel(
      roughVizOutput("chart", height = "500px"),
      
      hr(),
      
      h4("Generated Data"),
      tableOutput("dataTable"),
      
      hr(),
      
      h4("R Code"),
      verbatimTextOutput("code")
    )
  )
)

server <- function(input, output, session) {

  chartData <- reactive({
    input$regenerate

    n <- input$dataPoints

    if (input$chartType %in% c("Bar", "Horizontal Bar")) {
      data.frame(
        labels = LETTERS[1:n],
        values = round(runif(n, min = 5, max = 50), 1)
      )
    } else if (input$chartType %in% c("Pie", "Donut")) {
      values <- round(runif(n, min = 10, max = 100), 1)
      data.frame(
        labels = LETTERS[1:n],
        values = values / sum(values) * 100
      )
    } else if (input$chartType == "Line") {
      data.frame(
        x = 1:n,
        y = round(cumsum(rnorm(n, mean = 2, sd = 5)), 1),
        y2 = round(cumsum(rnorm(n, mean = 1, sd = 4)), 1)
      )
    } else {
      data.frame(
        x = round(runif(n * 3, min = 10, max = 100), 1),
        y = round(runif(n * 3, min = 10, max = 100), 1),
        group = rep(c("A", "B", "C"), each = n)
      )
    }
  })

  chartColors <- reactive({
    if (input$chartType %in% c("Pie", "Donut")) {
      c("#ff6b6b", "#4ecdc4", "#45b7d1", "#f9ca24", "#6c5ce7",
        "#a29bfe", "#fd79a8", "#fdcb6e", "#e17055", "#00b894")
    } else if (input$chartType == "Line") {
      c(input$colorScheme, "#4ecdc4", "#45b7d1")
    } else if (input$chartType == "Scatter") {
      c("#ff6b6b", "#4ecdc4", "#45b7d1")
    } else {
      input$colorScheme
    }
  })

  format_value <- function(x) {
    if (is.logical(x)) {
      return(if (length(x) == 1) if (x) "TRUE" else "FALSE" else paste0("c(", paste(ifelse(x, "TRUE", "FALSE"), collapse = ", "), ")"))
    }
    if (is.numeric(x)) {
      formatted <- format(x, trim = TRUE, digits = 6)
      return(if (length(x) == 1) formatted else paste0("c(", paste(formatted, collapse = ", "), ")"))
    }
    if (is.character(x)) {
      quoted <- sprintf('"%s"', x)
      return(if (length(x) == 1) quoted else paste0("c(", paste(quoted, collapse = ", "), ")"))
    }
    stop("Unsupported value type for code formatting", call. = FALSE)
  }

  format_data_code <- function(data) {
    lines <- vapply(
      names(data),
      function(col) paste0("  ", col, " = ", format_value(data[[col]])),
      character(1)
    )
    paste0("data <- data.frame(\n", paste(lines, collapse = ",\n"), "\n)")
  }

  chart_code_lines <- function(chart_type, colors) {
    arg_line <- function(name, value, comma = TRUE) {
      suffix <- if (comma) "," else ""
      paste0("  ", name, " = ", value, suffix)
    }

    switch(
      chart_type,
      "Bar" = c(
        "roughBar(",
        arg_line("data", "data"),
        arg_line("labels", "'labels'"),
        arg_line("values", "'values'"),
        arg_line("title", "'Sample Bar Chart'"),
        arg_line("color", format_value(colors)),
        arg_line("roughness", format_value(input$roughness)),
        arg_line("fillStyle", format_value(input$fillStyle)),
        arg_line("fillWeight", format_value(input$fillWeight)),
        arg_line("strokeWidth", format_value(input$strokeWidth)),
        arg_line("bowing", format_value(input$bowing), comma = FALSE),
        ")"
      ),
      "Horizontal Bar" = c(
        "roughBarH(",
        arg_line("data", "data"),
        arg_line("labels", "'labels'"),
        arg_line("values", "'values'"),
        arg_line("title", "'Sample Horizontal Bar Chart'"),
        arg_line("color", format_value(colors)),
        arg_line("roughness", format_value(input$roughness)),
        arg_line("fillStyle", format_value(input$fillStyle)),
        arg_line("fillWeight", format_value(input$fillWeight)),
        arg_line("strokeWidth", format_value(input$strokeWidth)),
        arg_line("bowing", format_value(input$bowing), comma = FALSE),
        ")"
      ),
      "Pie" = c(
        "roughPie(",
        arg_line("data", "data"),
        arg_line("labels", "'labels'"),
        arg_line("values", "'values'"),
        arg_line("title", "'Sample Pie Chart'"),
        arg_line("colors", format_value(colors)),
        arg_line("roughness", format_value(input$roughness)),
        arg_line("fillStyle", format_value(input$fillStyle)),
        arg_line("fillWeight", format_value(input$fillWeight)),
        arg_line("strokeWidth", format_value(input$strokeWidth)),
        arg_line("bowing", format_value(input$bowing), comma = FALSE),
        ")"
      ),
      "Donut" = c(
        "roughDonut(",
        arg_line("data", "data"),
        arg_line("labels", "'labels'"),
        arg_line("values", "'values'"),
        arg_line("title", "'Sample Donut Chart'"),
        arg_line("colors", format_value(colors)),
        arg_line("roughness", format_value(input$roughness)),
        arg_line("fillStyle", format_value(input$fillStyle)),
        arg_line("fillWeight", format_value(input$fillWeight)),
        arg_line("strokeWidth", format_value(input$strokeWidth)),
        arg_line("bowing", format_value(input$bowing), comma = FALSE),
        ")"
      ),
      "Line" = c(
        "roughLine(",
        arg_line("data", "data"),
        arg_line("x", "'x'"),
        arg_line("y", "'y'"),
        arg_line("y2", "'y2'"),
        arg_line("title", "'Sample Line Chart'"),
        arg_line("colors", format_value(colors)),
        arg_line("roughness", format_value(input$roughness)),
        arg_line("fillStyle", format_value(input$fillStyle)),
        arg_line("fillWeight", format_value(input$fillWeight)),
        arg_line("strokeWidth", format_value(input$strokeWidth)),
        arg_line("bowing", format_value(input$bowing)),
        arg_line("circle", format_value(input$showCircles)),
        arg_line("circleRadius", format_value(input$circleRadius), comma = FALSE),
        ")"
      ),
      "Scatter" = c(
        "roughScatter(",
        arg_line("data", "data"),
        arg_line("x", "'x'"),
        arg_line("y", "'y'"),
        arg_line("colorVar", "'group'"),
        arg_line("title", "'Sample Scatter Plot'"),
        arg_line("colors", format_value(colors)),
        arg_line("roughness", format_value(input$roughness)),
        arg_line("fillStyle", format_value(input$fillStyle)),
        arg_line("fillWeight", format_value(input$fillWeight)),
        arg_line("strokeWidth", format_value(input$strokeWidth)),
        arg_line("bowing", format_value(input$bowing)),
        arg_line("radius", format_value(input$circleRadius), comma = FALSE),
        ")"
      )
    )
  }

  build_chart <- function(chart_type, data, colors) {
    common <- list(
      roughness = input$roughness,
      fillStyle = input$fillStyle,
      fillWeight = input$fillWeight,
      strokeWidth = input$strokeWidth,
      bowing = input$bowing
    )

    switch(
      chart_type,
      "Bar" = do.call(
        roughBar,
        c(list(
          data = data,
          labels = "labels",
          values = "values",
          title = "Sample Bar Chart",
          color = colors
        ), common)
      ),
      "Horizontal Bar" = do.call(
        roughBarH,
        c(list(
          data = data,
          labels = "labels",
          values = "values",
          title = "Sample Horizontal Bar Chart",
          color = colors
        ), common)
      ),
      "Pie" = do.call(
        roughPie,
        c(list(
          data = data,
          labels = "labels",
          values = "values",
          title = "Sample Pie Chart",
          colors = colors
        ), common)
      ),
      "Donut" = do.call(
        roughDonut,
        c(list(
          data = data,
          labels = "labels",
          values = "values",
          title = "Sample Donut Chart",
          colors = colors
        ), common)
      ),
      "Line" = do.call(
        roughLine,
        c(list(
          data = data,
          x = "x",
          y = "y",
          y2 = "y2",
          title = "Sample Line Chart",
          colors = colors,
          circle = input$showCircles,
          circleRadius = input$circleRadius
        ), common)
      ),
      "Scatter" = do.call(
        roughScatter,
        c(list(
          data = data,
          x = "x",
          y = "y",
          colorVar = "group",
          title = "Sample Scatter Plot",
          colors = colors,
          radius = input$circleRadius
        ), common)
      )
    )
  }

  output$chart <- renderRoughViz({
    build_chart(input$chartType, chartData(), chartColors())
  })

  output$dataTable <- renderTable(chartData())

  output$code <- renderText({
    data <- chartData()
    colors <- chartColors()
    code_lines <- c(
      "library(roughVizR)",
      "",
      format_data_code(data),
      "",
      "# Plot",
      chart_code_lines(input$chartType, colors)
    )
    paste(code_lines, collapse = "\n")
  })

  output$downloadChart <- downloadHandler(
    filename = function() {
      paste0("roughviz_chart_", Sys.Date(), ".html")
    },
    content = function(file) {
      chart <- build_chart(input$chartType, chartData(), chartColors())
      htmlwidgets::saveWidget(chart, file, selfcontained = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
