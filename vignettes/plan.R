## ---- echo = FALSE-------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(plan)
data(burndown)
print(summary(burndown))

## ------------------------------------------------------------------------
library(plan)
data(burndown)
plot(burndown)

## ------------------------------------------------------------------------
library(plan)
data(gantt)
print(summary(gantt))

## ------------------------------------------------------------------------
library(plan)
data(gantt)
plot(gantt)

