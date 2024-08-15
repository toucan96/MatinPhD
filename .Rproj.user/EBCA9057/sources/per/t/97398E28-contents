group_t_test <- function(data, name, value, control) {
  cname <- select(data, name)
  cvalue <- select(data, value)
  cdata <- data.frame(group = cname, value = cvalue)
  colnames(cdata) <- c("group", "value")
  groups <- levels(factor(cdata$group))

  collate <- c()
  count <- length(groups)
  n <- 0
  repeat {
    n <- n+1
    p <- t.test(
      subset(cdata, group == groups[n])$value,
      subset(cdata, group == control)$value
      )$p.value

    result <- data.frame(group = groups[n], p = p)
    collate <- rbind(collate, result)
    if (n == count) { break }
  }
  colnames(collate) <- c(name, "p-value")
  return(collate)
}

