

theme_background <- function(bg_color){
  theme(
    panel.background = element_rect(fill = bg_color,color = NA),
    plot.background = element_rect(fill = bg_color, color = NA),
    # panel.border = element_rect(color = bg_color, fill = NA)
    #panel.border = element_blank()
    panel.border = element_blank(),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0, "cm")
  )
}
