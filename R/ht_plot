ht_plot <- function(data, .x, col.by=NULL, vars, id.col = NULL, geom = "geom_boxplot", ncol = 4, trans ="identity") {
  n <- 1
  gg.list <- list()
  group<-NULL
  vars <- colnames(data[vars])
  if(geom %in% c("geom_boxplot", "geom_col", "geom_violin")) {
    data[,.x]<- data[,.x]
    group <- .x
  }
  geom.fun <- match.fun(geom)
  repeat {
    plot <- data %>% 
      ggplot(
        aes(
          x=.data[[.x]], y=.data[[vars[n]]],
          group=if(!is.null(group)){.data[[group]]},
          color=if(!is.null(col.by)){.data[[col.by]]}
        )) +
      geom.fun() + {if(!is.null(id.col))geom_text(aes(label = .data[[id.col]]))} +
      scale_y_continuous(transform=trans) + ggtitle(vars[n]) +
      theme_classic() +
      theme(legend.position = "none", plot.title.position = "plot", axis.title.y = element_blank())
    gg.list[[n]] <- plot
    n <- n+1
    if (n>length(vars)) 
      break
  }
  do.call("grid.arrange", c(gg.list, ncol=ncol))
}
