# Shiny Example for roughViz R Package

library(shiny)
library(roughViz)

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
  
  # Generate random data
  chartData <- reactive({
    input$regenerate  # Dependency on regenerate button
    
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
        values = values / sum(values) * 100  # Convert to percentages
      )
    } else if (input$chartType == "Line") {
      data.frame(
        x = 1:n,
        y = round(cumsum(rnorm(n, mean = 2, sd = 5)), 1),
        y2 = round(cumsum(rnorm(n, mean = 1, sd = 4)), 1)
      )
    } else if (input$chartType == "Scatter") {
      data.frame(
        x = round(runif(n * 3, min = 10, max = 100), 1),
        y = round(runif(n * 3, min = 10, max = 100), 1),
        group = rep(c("A", "B", "C"), each = n)
      )
    }
  })
  
  # Generate colors based on chart type
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
  
  # Render the chart
  output$chart <- renderRoughViz({
    data <- chartData()
    colors <- chartColors()
    
    if (input$chartType == "Bar") {
      roughBar(
        data = data,
        labels = "labels",
        values = "values",
        title = "Sample Bar Chart",
        color = colors,
        roughness = input$roughness,
        fillStyle = input$fillStyle,
        fillWeight = input$fillWeight,
        strokeWidth = input$strokeWidth,
        bowing = input$bowing
      )
    } else if (input$chartType == "Horizontal Bar") {
      roughBarH(
        data = data,
        labels = "labels",
        values = "values",
        title = "Sample Horizontal Bar Chart",
        color = colors,
        roughness = input$roughness,
        fillStyle = input$fillStyle,
        fillWeight = input$fillWeight,
        strokeWidth = input$strokeWidth,
        bowing = input$bowing
      )
    } else if (input$chartType == "Pie") {
      roughPie(
        data = data,
        labels = "labels",
        values = "values",
        title = "Sample Pie Chart",
        colors = colors,
        roughness = input$roughness,
        fillStyle = input$fillStyle,
        fillWeight = input$fillWeight,
        strokeWidth = input$strokeWidth,
        bowing = input$bowing
      )
    } else if (input$chartType == "Donut") {
      roughDonut(
        data = data,
        labels = "labels",
        values = "values",
        title = "Sample Donut Chart",
        colors = colors,
        roughness = input$roughness,
        fillStyle = input$fillStyle,
        fillWeight = input$fillWeight,
        strokeWidth = input$strokeWidth,
        bowing = input$bowing
      )
    } else if (input$chartType == "Line") {
      roughLine(
        data = data,
        x = "x",
        y = "y",
        y2 = "y2",
        title = "Sample Line Chart",
        colors = colors,
        roughness = input$roughness,
        fillStyle = input$fillStyle,
        fillWeight = input$fillWeight,
        strokeWidth = input$strokeWidth,
        bowing = input$bowing,
        circle = input$showCircles,
        circleRadius = input$circleRadius
      )
    } else if (input$chartType == "Scatter") {
      roughScatter(
        data = data,
        x = "x",
        y = "y",
        colorVar = "group",
        title = "Sample Scatter Plot",
        colors = colors,
        roughness = input$roughness,
        fillStyle = input$fillStyle,
        fillWeight = input$fillWeight,
        strokeWidth = input$strokeWidth,
        bowing = input$bowing,
        radius = input$circleRadius
      )
    }
  })
  
  # Display the data table
  output$dataTable <- renderTable({
    chartData()
  })
  
  # Generate R code
  output$code <- renderPrint({
    data <- chartData()
    
    code <- paste0("library(roughViz)\n\n",
                   "# Create data\n",
                   "data <- ", capture.output(dput(data)), "\n\n",
                   "# Create chart\n")
    
    if (input$chartType == "Bar") {
      code <- paste0(code,
                     "roughBar(\n",
                     "  data = data,\n",
                     "  labels = 'labels',\n",
                     "  values = 'values',\n",
                     "  title = 'Sample Bar Chart',\n",
                     "  color = '", input$colorScheme, "',\n",
                     "  roughness = ", input$roughness, ",\n",
                     "  fillStyle = '", input$fillStyle, "',\n",
                     "  fillWeight = ", input$fillWeight, ",\n",
                     "  strokeWidth = ", input$strokeWidth, ",\n",
                     "  bowing = ", input$bowing, "\n",
                     ")")
    }
    # Add other chart types similarly...
    
    cat(code)
  })
  
  # Download handler
  output$downloadChart <- downloadHandler(
    filename = function() {
      paste0("roughviz_chart_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Create a temporary HTML file with the chart
      tempHtml <- tempfile(fileext = ".html")
      
      # Generate the chart HTML
      chart <- if (input$chartType == "Bar") {
        roughBar(
          data = chartData(),
          labels = "labels",
          values = "values",
          title = "Sample Bar Chart",
          color = chartColors(),
          roughness = input$roughness,
          fillStyle = input$fillStyle,
          fillWeight = input$fillWeight,
          strokeWidth = input$strokeWidth,
          bowing = input$bowing
        )
      }
      # Add other chart types...
      
      # Save as HTML
      htmlwidgets::saveWidget(chart, tempHtml, selfcontained = TRUE)
      file.copy(tempHtml, file)
      file.remove(tempHtml)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)