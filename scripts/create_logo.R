library(ComplexHeatmap)
library(tidyverse)
library(viridisLite)

set.seed(7)
means <- list(high = 7.5:10,
              moderate = 3:8,
              low = 0:4)
order <- sample(1:3, 3)

mat <-
  lapply(1:500, function(x) {
  r1 <- c(rnorm(10, sample(means$high, 1)), 
          rnorm(15, sample(means$moderate, 1)), 
          rnorm(9, sample(means$low, 1)))
  r2 <- c(rnorm(10, sample(means$low, 1)), 
          rnorm(15, sample(means$moderate, 1)), 
          rnorm(9, sample(means$high, 1)))
  r3 <- c(rnorm(10, sample(means$moderate, 1)), 
          rnorm(15, sample(means$high, 1)), 
          rnorm(9, sample(means$low, 1)))
  list(r1, r2, r3) %>%
    do.call(rbind, .)
}) %>%
  do.call(rbind, .)

png("logo.png", width = 242, height = 179, bg = NA)
Heatmap(scale(mat), col = viridis(20), show_heatmap_legend = FALSE) #, clustering_distance_rows = "kendall")
dev.off()