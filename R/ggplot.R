#' Theme by Christian Rubba
#' @export
theme_cr <- function(base_size = 12, base_family = "sans") {
#   (theme_foundation(base_size = base_siz    panel.border = element_rect(colour = "gray"), base_family = base_family)
#    + 
  theme(
    text = element_text(family = "Helvetica", face = "plain",
                        colour = "black", size = 12,
                        hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    title = element_text(face = "bold"),
    strip.background = element_rect(fill = "white", colour = NA),
    strip.text = element_text(size = rel(1.2)),
    #axis.line = element_line(colour = NA, size = 0.5),
    #axis.line.x = element_line(colour = "black", size= 0.5),
    axis.text = element_text(size = rel(0.8)),
    axis.text.y = element_text(hjust = 1),
    axis.text.x = element_text(vjust = 1),
    axis.ticks = element_line(colour = "black"),
    panel.background = element_rect(fill = "#F7F7F7", size = 1), #
    panel.grid.major = element_line(colour = "#D3D3D3"), #element_blank(), #line(colour = "#D6D6D6"),
    panel.grid.minor = element_line(colour = "#D3D3D3"),
    panel.border = element_rect(fill  = NA, colour = "#607B8B", size = 0.2),
    panel.grid = element_line(colour = "blue"),
    legend.background = element_rect(fill = "white"),
    legend.text = element_text(size = rel(1.3)),
    legend.key = element_rect(fill = "white"),
    legend.box = "horizontal",
    legend.position = "bottom"
    )
#     legend.key = element_rect(fill = "white"))
  }

