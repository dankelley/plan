# vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(plan)
filename <- system.file("extdata", "gantt.dat", package="plan")
g <- read.gantt(filename)              # make it available for other tests

test_that("can read gantt.dat file", {
  expect_silent(b <- read.gantt(filename))
})

test_that("summary works for gantt objects", {
  expect_output(summary(g), "Key, Description,")
})

test_that("plot works for gantt objects", {
  expect_silent(plot(g))
})

