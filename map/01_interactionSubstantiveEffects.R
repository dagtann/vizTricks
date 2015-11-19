rm(list = ls())
library('foreign')
dta <- read.dta(file = '/Users/dag/Desktop/govduration.dta')
str(dta)
summary(dta)

fit <- lm(govdur ~ NP*PS + PD, data = dta)
summary(fit)

quantiles <- c(.05, .25, .5, .75, .95)
pred.dta <- expand.grid(
  NP = quantile(dta[, 'NP'], quantiles),
  PS = quantile(dta[, 'PS'], quantiles),
  PD = mean(dta[, 'PD'])
)
pred.dta <- data.frame(
  pred.dta,
  predict(
    fit, newdata = pred.dta, type = 'response',
    se.fit = TRUE, interval = 'confidence'
  )
)
rm(quantiles)
str(pred.dta)

library('ggplot2')
ggplot(data = pred.dta,
  aes(
    x = factor(format(NP, digits = 2)),
    y = factor(format(PS, digits = 3)), fill = fit.fit
  )
) +
  geom_tile(colour = 'black') +
  theme_minimal()
