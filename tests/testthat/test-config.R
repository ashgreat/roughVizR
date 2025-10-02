test_that("roughBar sanitises factor columns and attaches metadata", {
  data <- data.frame(
    region = factor(c("North", "South")),
    total = c(10, 12)
  )

  widget <- roughBar(data, labels = "region", values = "total")
  config <- widget$x$config

  expect_equal(config$data$labels, c("North", "South"))
  expect_equal(config$data$values, c(10, 12))
})

test_that("roughLine coerces numeric series and errors without numeric data", {
  line_data <- data.frame(
    x = 1:3,
    y = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )

  widget <- roughLine(line_data, x = "x", y = "y")
  config <- widget$x$config

  expect_equal(config$x, 1:3)
  expect_equal(config$data$y, c(1, 2, 3))

  expect_error(
    roughLine(data.frame(x = letters[1:3]), x = "x"),
    "requires at least one numeric"
  )
})

test_that("roughScatter validates required mappings", {
  scatter_data <- data.frame(
    x = rnorm(5),
    y = rnorm(5),
    group = letters[1:5]
  )

  widget <- roughScatter(scatter_data, x = "x", y = "y", colorVar = "group")
  config <- widget$x$config

  expect_equal(config$x, "x")
  expect_equal(config$y, "y")
  expect_equal(config$colorVar, "group")

  expect_error(
    roughScatter(scatter_data, x = "missing", y = "y"),
    "requires column"
  )
})

test_that("roughStackedBar requires label and numeric value columns", {
  stacked_data <- data.frame(
    labels = c("Q1", "Q2"),
    series_a = c("10", "20"),
    series_b = c("30", "40"),
    stringsAsFactors = FALSE
  )

  widget <- roughStackedBar(stacked_data, labels = "labels")
  config <- widget$x$config

  expect_equal(config$labels, "labels")
  expect_equal(config$data$series_a, c(10, 20))
  expect_equal(config$data$series_b, c(30, 40))

  expect_error(
    roughStackedBar(data.frame(labels = "A"), labels = "labels"),
    "requires at least one value"
  )
})

test_that("roughBarH expands margin for long labels by default", {
  long_labels <- sprintf("Category %s with a very long name", LETTERS[1:4])
  data <- data.frame(labels = long_labels, values = 1:4, stringsAsFactors = FALSE)

  widget <- roughBarH(data, labels = "labels", values = "values")
  config <- widget$x$config

  expect_gt(config$margin$left, 150)

  custom_margin <- list(top = 10, right = 10, bottom = 10, left = 50)
  widget_custom <- roughBarH(data, labels = "labels", values = "values", margin = custom_margin)
  expect_equal(widget_custom$x$config$margin$left, 50)
})

test_that("roughBarH works with arbitrary column names", {
  data <- data.frame(
    indicator = c("A", "B"),
    weight_with_sat_act = c(0.16, 0.05),
    stringsAsFactors = FALSE
  )

  widget <- roughBarH(data, labels = "indicator", values = "weight_with_sat_act")
  config <- widget$x$config

  expect_equal(config$data$labels, c("A", "B"))
  expect_equal(config$data$values, c(0.16, 0.05))
})
