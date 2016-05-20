library(plan)
burndown <- read.burndown("burndown.dat")
save(burndown, file="burndown.rda")
tools::resaveRdaFiles("burndown.rda")
