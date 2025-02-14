ht_table <- function(data, group_col=2, vars, min.obs=5) {
  n<-1
  collate <- list()
  vars <- colnames(data[,vars])
  ttable <- data.frame()
  repeat {
    ctable <- data.frame(data[,vars[n]],data[,group_col]) %>% set_names(c(vars[n],group_col)) %>% table()
    collate[[vars[n]]] <- ctable
    ttable[vars[n],"dimensions"] <- dim(ctable) %>% paste(collapse = " ")
    ttable[vars[n],paste("all over", min.obs)] <- all(ctable > min.obs)
    n<-n+1
    if(n==length(vars)+1) break
  }
  output <- list("contigency.table"=collate,
                 "evaluation"=ttable)
  return(output)
}
