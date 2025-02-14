<<<<<<< HEAD
httable <- function(data, group_col=2, vars, threshold=5) {
=======
ht_table <- function(data, group_col=2, vars, min.obs=5) {
>>>>>>> feature/ht_discrete_stats
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


### NOT FINALISED ###



hthsd<- function(data, group_col = 2, genes, var = "variable") {
  n<-1
  collate <- c()
  repeat {
    aov.hsd <- reduce(TukeyHSD(aov(data[,genes[n]] ~ data[,group_col])),rbind) %>% as.data.frame() %>% select(`p adj`) %>% t()
    rownames(aov.hsd)<- genes[n]
    collate <- rbind(collate, aov.hsd)
    n<-n+1
    if(n==length(genes)+1) break
  }
  return(collate %>% as.data.frame() %>% rownames_to_column(var=var))
}

htplotCount <- function(dds, genes, group){
  n <- 0
  plots <- list()
  repeat {
    n <- n+1
    plot<-plotCounts(dds, gene=genes[n], intgroup=group)
    c(plots, plot)
    if(n==length(genes)) break
  }
  plots
}

hthist <- function(data, genes, group) {
  n <- 0
  plots<-list()
  repeat {
    n <- n+1
    plot <- ggplot(data, aes_string(x=genes[n],
                                    group=group,
                                    fill=group)
    ) +
      geom_histogram(alpha = 0.5, position = "identity")
    plots<-list(plots, plot)
    if(n==length(genes)) break
  }
  plots
}
