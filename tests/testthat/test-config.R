test_that("roughBar sanitises factor columns and attaches metadata", {
  data <- data.frame(
    region = factor(c("North", "South")),
    total = c(10, 12)
  )

  widget <- roughBar(data, labels = "region", values = "total")
  config <- widget$x$config

  expect_equal(config$labels, "region")
  expect_equal(config$values, "total")
  expect_type(config$data$region, "character")
  expect_equal(config$data$total, c(10, 12))
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
