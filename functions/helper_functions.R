#helper functions

tidy_custom.coxph <- function(x, ...) {
  s <- summary(x)$coefficients
  data.frame(
    term = row.names(s),
    robust.se = s[, "robust se", drop = FALSE])
}