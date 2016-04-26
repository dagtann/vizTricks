set.seed(0)
n <- 200
x <- rnorm(n, 3, 1)
d <- sample(0:1, size = n, replace = TRUE)
e <- rnorm(n, 0, 4)
y <- 5 - 4*x - 9*d + 3*d*x + e
pdta <- data.frame(x, d, y)

library('ggplot2')
ggplot(data = pdta, aes(x = x, y = y)) +
  geom_point() +
  geom_rug() +
  facet_grid(d ~ .) +
  geom_smooth(method = 'lm')
