#helper functions

tidy_custom.coxph <- function(x, ...) {
  s <- summary(x)$coefficients
  rse = s[, "robust se", drop = FALSE]
  data.frame(
    term = row.names(s),
    robust.se = rse,
    conf.low = s[,"coef",drop=FALSE] - 1.96*rse,
    conf.high = s[,"coef",drop=FALSE] + 1.96*rse)
}
