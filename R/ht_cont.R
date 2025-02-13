ht_cont <- function(data, group_col = 2, vars, FUN = t.test, adj="BH", ...) {
  n<-1
  collate <- c()
  vars <- colnames(data[,vars])
  repeat {
    res <- broom::tidy(FUN(data[,vars[n]] ~ data[,group_col], ...)) %>% as.data.frame()
    res[,"variable"] <- vars[n]
    collate <- rbind(collate, res)
    n<-n+1
    if(n==length(vars)+1) break
  }
  return(collate %>% relocate(variable) %>%
           transform(p.adj=p.adjust(p.value, method=adj)) %>%
           transform(sign=case_when(p.adj < 0.05 ~ "**", p.value<0.05~"*", p.value>=0.05 ~ ""))
  )
}