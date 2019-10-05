## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(plan)
g <- read.gantt("gantt.dat")

test_that("summary works for gantt objects", {
          expect_output(summary(g), "Key, Description,")
})

test_that("plot works for gantt objects", {
          expect_silent(plot(g))
})

