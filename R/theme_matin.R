theme_matin <- function (base_size = 12, base_family = "Gill Sans MT", base_line_size = base_size/22,
          base_rect_size = base_size/22)
{
  theme_bw(base_size = base_size,
           base_family = base_family,
           base_line_size = base_line_size,
           base_rect_size = base_rect_size
           ) %+replace%
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          axis.line = element_line(colour = "black",
                                   linewidth = rel(1)
                                   ),
          axis.title = element_text(size = 14),
          axis.text = element_text(color = "#000000"),
          legend.key = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(family = "Gill Sans MT"),
          plot.title = element_text(hjust =0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.background = element_blank(),
          complete = TRUE)
}
