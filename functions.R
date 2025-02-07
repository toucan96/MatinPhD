ht_table <- function(data, group_col=2, vars, threshold=5) {
  n<-1
  collate <- list()
  vars <- colnames(data[,vars])
  ttable <- data.frame()
  repeat {
    ctable <- data.frame(data[,vars[n]],data[,group_col]) %>% set_names(c(vars[n],group_col)) %>% table()
    collate[[vars[n]]] <- ctable
    ttable[vars[n],"dimensions"] <- dim(ctable) %>% paste(collapse = " ")
    ttable[vars[n],paste("all over", threshold)] <- all(ctable > threshold)
    n<-n+1
    if(n==length(vars)+1) break
  }
  output <- list("contigency.table"=collate,
                 "evaluation"=ttable)
  return(output)
}


htchi <- function(data, vars = NULL) {
  list.tables <- data[[1]]
  if (is.null(vars)) {
    vars <- data$evaluation %>% filter(`all over 5` ==TRUE) %>% rownames()
  } else vars <- vars
  n<-1
  collate <- data.frame()
  repeat {
    chi <-chisq.test(list.tables[[n]])
    collate[vars[n],"statistic"]<- chi$statistic
    collate[vars[n],"p.value"] <- chi$p.value
    n<-n+1
    if(n==length(vars)+1) break
  }
  output <- collate %>% rownames_to_column(var="variable") %>% transform(sign=case_when(p.value<0.05~"*", p.value>=0.05 ~ ""))
  return(output)
}

htfish <- function(data, vars = NULL) {
  list.tables <- data[[1]]
  if (is.null(vars)) {
    vars <- data$evaluation %>% filter(`all over 5` ==FALSE) %>% rownames()
  } else vars <- vars
  n<-1
  collate <- data.frame()
  repeat {
    fisher <-fisher.test(list.tables[[n]], workspace = 2e8)
    collate[vars[n],"p.value"] <- fisher$p.value
    n<-n+1
    if(n==length(vars)+1) break
  }
  output <- collate %>% rownames_to_column(var="variable") %>% transform(sign=case_when(p.value<0.05~"*", p.value>=0.05 ~ ""))
  return(output)
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
