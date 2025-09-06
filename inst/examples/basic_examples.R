# Basic Examples for roughVizR R Package

library(roughVizR)

# Example 1: Simple Bar Chart
data1 <- data.frame(
  labels = c("North", "South", "East", "West", "Central"),
  values = c(10, 5, 8, 3, 7)
)

roughBar(
  data = data1,
  labels = "labels",
  values = "values",
  title = "Regional Sales",
  color = "coral",
  roughness = 2,
  fillStyle = "cross-hatch",
  width = 600,
  height = 400
)

# Example 2: Pie Chart with Custom Colors
pie_data <- data.frame(
  labels = c("Desktop", "Mobile", "Tablet", "Other"),
  values = c(45, 35, 15, 5)
)

roughPie(
  data = pie_data,
  labels = "labels",
  values = "values",
  title = "Device Usage",
  colors = c("#ff6b6b", "#4ecdc4", "#45b7d1", "#f9ca24"),
  roughness = 3,
  fillStyle = "zigzag"
)

# Example 3: Multi-series Line Chart
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun")
line_data <- data.frame(
  x = months,
  sales = c(30, 35, 28, 40, 38, 45),
  costs = c(20, 22, 18, 25, 23, 28),
  profit = c(10, 13, 10, 15, 15, 17)
)

roughLine(
  data = line_data,
  x = "x",
  y = "sales",
  y2 = "costs",
  y3 = "profit",
  title = "Q1-Q2 Performance",
  colors = c("coral", "skyblue", "green"),
  circle = TRUE,
  circleRadius = 8,
  roughness = 1.5
)

# Example 4: Scatter Plot with Groups
set.seed(42)
scatter_data <- data.frame(
  x = c(rnorm(20, mean = 30, sd = 5), 
        rnorm(20, mean = 50, sd = 5),
        rnorm(20, mean = 70, sd = 5)),
  y = c(rnorm(20, mean = 30, sd = 5),
        rnorm(20, mean = 50, sd = 5),
        rnorm(20, mean = 40, sd = 5)),
  group = rep(c("Low", "Medium", "High"), each = 20)
)

roughScatter(
  data = scatter_data,
  x = "x",
  y = "y",
  colorVar = "group",
  title = "Customer Segments",
  colors = c("#ff6b6b", "#4ecdc4", "#45b7d1"),
  radius = 10,
  roughness = 2
)

# Example 5: Donut Chart
roughDonut(
  data = pie_data,
  labels = "labels",
  values = "values",
  title = "Device Distribution (Donut)",
  colors = c("#ff6b6b", "#4ecdc4", "#45b7d1", "#f9ca24"),
  roughness = 2,
  fillStyle = "hachure"
)

# Example 6: Horizontal Bar Chart
performance_data <- data.frame(
  labels = c("Team A", "Team B", "Team C", "Team D", "Team E"),
  values = c(85, 92, 78, 88, 95)
)

roughBarH(
  data = performance_data,
  labels = "labels",
  values = "values",
  title = "Team Performance Scores",
  color = "skyblue",
  fillStyle = "cross-hatch",
  roughness = 1.5
)

# Example 7: Stacked Bar Chart
quarterly_data <- data.frame(
  labels = c("Q1", "Q2", "Q3", "Q4"),
  ProductA = c(20, 30, 25, 35),
  ProductB = c(15, 20, 18, 22),
  ProductC = c(10, 15, 12, 18)
)

roughStackedBar(
  data = quarterly_data,
  labels = "labels",
  title = "Quarterly Sales by Product",
  colors = c("#ff6b6b", "#4ecdc4", "#45b7d1"),
  roughness = 2,
  fillStyle = "zigzag"
)

# Example 8: Different Fill Styles Comparison
styles <- c("hachure", "cross-hatch", "zigzag", "dashed", "solid", "zigzag-line")

for (style in styles) {
  print(paste("Creating chart with fill style:", style))
  
  simple_data <- data.frame(
    labels = c("A", "B", "C"),
    values = c(8, 6, 4)
  )
  
  roughBar(
    data = simple_data,
    labels = "labels",
    values = "values",
    title = paste("Fill Style:", style),
    color = "coral",
    fillStyle = style,
    roughness = 2
  )
}

# Example 9: Different Roughness Levels
for (rough_level in c(0, 2, 5, 10)) {
  simple_data <- data.frame(
    labels = c("Low", "Medium", "High"),
    values = c(3, 6, 9)
  )
  
  roughBar(
    data = simple_data,
    labels = "labels",
    values = "values",
    title = paste("Roughness Level:", rough_level),
    color = "pink",
    roughness = rough_level,
    fillStyle = "hachure"
  )
}