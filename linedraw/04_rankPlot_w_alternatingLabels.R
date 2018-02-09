rm(list = ls())
set.seed(124545098)
library('ggplot2')
library('extrafont')
loadfonts(quiet = TRUE)

y_intercept <- 0
n <- 107
n_labels <- 5
ranks_2_display <- c(1:n_labels, ((n-n_labels+1):n))
value <- rnorm(n)
rank <- dplyr::dense_rank(value)
labels <- replicate(n,
  paste(sample(LETTERS, 3, replace = TRUE), collapse = "")
)
flag <- 0 == rank %% 2
label_shift <- .03 * (max(value) - min(value))

label_pos <- vector(length = n, 'numeric')
label_pos[flag] <- y_intercept + label_shift
label_pos[!flag] <- y_intercept - label_shift

segment_end <- vector(length = n, 'numeric')
segment_end[y_intercept >= value] <- label_pos[y_intercept >= value] - .5 * label_shift
segment_end[y_intercept < value] <- label_pos[y_intercept < value] + .5 * label_shift

pdta <- data.frame(value, rank, labels, y_intercept, label_pos, segment_end)
ggplot(pdta,
  aes(x = rank, y = value, label = labels)
) +
  geom_segment(
    data = subset(pdta, rank %in% ranks_2_display),
    aes(yend = segment_end, xend = rank), size = .01) +
  geom_point() +
  geom_text(
    data = subset(pdta, rank %in% ranks_2_display),
    aes(y = label_pos), angle = 45, size = 2
  ) +
  ggthemes::theme_fivethirtyeight(base_family = "CMU Sans Serif")
ggsave("/Users/dag/test.pdf", width = 7, height = 7/1.618, family = "CMU Sans Serif")
