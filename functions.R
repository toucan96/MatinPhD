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



