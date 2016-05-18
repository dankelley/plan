library(plan)
gantt <- read.gantt("gantt.dat")
save(gantt, file="gantt.rda")
tools::resaveRdaFiles("gantt.rda")
