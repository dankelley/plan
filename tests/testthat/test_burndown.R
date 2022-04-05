# vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(plan)
filename <- system.file("extdata", "burndown.dat", package="plan")
b <- read.burndown(filename)           # make it available for other tests

test_that("can read burndown.dat file", {
  expect_silent(b <- read.burndown(filename))
})

test_that("summary works for burndown objects", {
  expect_output(summary(b), "Start,")
})

test_that("plot works for burndown objects", {
  expect_silent(plot(b))
})

test_that("plot.burndown() handles POSIX t.stop correctly (issue 23)", {
  t.stop <- as.POSIXct(strptime("2006-04-15", "%Y-%m-%d"))
  expect_silent(plot(b, t.stop=t.stop))
})

test_that("plot.burndown() handles POSIX t.stop correctly (issue 23)", {
  t.stop <- "2006-04-15"
  expect_silent(plot(b, t.stop=t.stop))
})

test_that("as.burndown() creates same object as read.burndown", {
  b2 = as.burndown(b@data$start, b@data$deadline,
    b@data$tasks, b@data$progress)
  expect_equivalent(b, b2)
})

