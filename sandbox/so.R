sample = rep(c("A", "B"), each=7)
distance = c(25, 75, 150, 200, 250, 350, 450, 25, 75, 150, 200, 250, 350, 450)
y = c(NA, NA, 2, 3, 4, NA, 3, NA, NA, 2, 3, 3, NA, NA)
library(data.table)
dt <- data.table(sample, distance, y)
dt[, sample := factor(sample)]
