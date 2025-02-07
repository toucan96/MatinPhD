ht_disc <- function(data, vars = 1, FUN=chisq.test, ...) {
  n<-1
  collate <- c()
  vars <- names(data)[vars]
  repeat {
    res <- broom::tidy(FUN(data[[vars[n]]], ...)) %>% as.data.frame()
    res[,"variable"] <- vars[n]
    collate <- rbind(collate, res)
    n<-n+1
    if(n==length(vars)+1) break
  }
  return(collate %>% relocate(variable) %>%
           transform(p.adj=p.adjust(p.value, method="BH")) %>%
           transform(sign=case_when(p.adj < 0.05 ~ "**", p.value<0.05~"*", p.value>=0.05 ~ "")))
}
