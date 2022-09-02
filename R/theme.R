#' theme_Publication
#'
#' @param base_size 
#' @param base_family 
#'
#' @return NULL
#' @export
#' @import ggplot2
#' @examples
#' For future
theme_Publication <- function(base_size=8) {
  (theme(plot.title = element_text(face = "bold",
                                   size = rel(1.2), hjust = 0.5),
         panel.background = element_rect(fill="white", colour = NA),
         axis.title = element_text(face = "bold", size = 10),
         axis.title.y = element_text(angle=90,vjust =2),
         axis.title.x = element_text(vjust = -0.2),
         axis.text = element_text(size = base_size), 
         axis.line = element_line(colour="black"),
         axis.ticks = element_line(),
         panel.grid.major.y = element_line(colour="#f0f0f0"),
         panel.grid.major.x = element_blank(),
         panel.grid.minor = element_blank(),
         legend.text = element_text(size = 8),
         legend.key = element_rect(colour = NA),
         legend.position = "bottom",
         legend.direction = "horizontal",
         legend.key.size= unit(0.2, "cm"),
         legend.background = element_blank(),
         plot.margin=unit(c(10,5,5,5),"mm"),
         strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
         strip.text = element_text(face="bold")
  )
  )
}


