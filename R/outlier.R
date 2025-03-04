upper_outlier <- function(data, coef = 1.5) {
  u.limit = quantile(data, .75)[[1]] + IQR(data)*coef

  return(u.limit)
}

lower_outlier <- function(data, coef = 1.5) {
  l.limit = quantile(data, .25)[[1]] - IQR(data)*coef

  return(l.limit)
}

outliers <- function(data, q1 = 0.5, q2 = 0.5, coef = 1) {
  l.limit = quantile(data, 0+q1)[[1]] - IQR(data)*coef
  u.limit = quantile(data, 1-q2)[[1]] + IQR(data)*coef

  return(list(upper.limit = u.limit, lower.limit = l.limit))
}


outliers.qpcr <- function(data, q1 = 0.25, q2 = 0.75, coef = 1) {
  l.limit = quantile(data,0+q1)[[1]] - quantile(data, 0.75)-quantile(data,0.25)
  u.limit = quantile(data,0+q2)[[1]] + quantile(data, 0.75)+quantile(data,0.25)
  return(list(upper.limit = u.limit, lower.limit = l.limit))
}

outliers.qpcr2 <- function(data, q1 = 0.25, q2 = 0.75, coef = 1) {
  l.limit = sqrt(quantile(2^data,0+q1)[[1]] - IQR(2^data))
  u.limit = sqrt(quantile(2^data,0+q2)[[1]] + IQR(2^data))
  return(list(upper.limit = u.limit, lower.limit = l.limit))
}
