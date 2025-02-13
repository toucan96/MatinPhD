volcano <- function(data, x, y, alpha=0.05, min=1) {
  ggplot(data,
         aes(
           x= .data[[x]],
           y= -log10(.data[[y]]),
           colour = 
             ifelse(.data[[y]] < alpha & .data[[x]] > min |
                      .data[[y]] < alpha & .data[[x]] < - min, "#E7B100", 
                    ifelse(.data[[y]] < alpha, "#006F94", "lightgrey")
             )
         )) +
    geom_point() +
    scale_colour_identity()+
    scale_x_continuous(limits = c(-max(sqrt(data[[x]]^2)), max(sqrt(data[[x]]^2)))) +
    geom_hline(yintercept = -log10(alpha), linetype =2) +
    geom_vline(xintercept = c(-min,min), linetype =2) +
    theme_classic()+
    theme(legend.position = "none") 
}