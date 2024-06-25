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

#' @title pal_brewer
#' @description Generate colors
#' @param pal_name palette name
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname pal_brewer
#' @export 
pal_brewer <- function(pal_name){
  pal_lst <- list(
  # A more pretty rainbow palette
    rainbow=c('#c35b25', '#d88b2e', '#dfac33', '#af952a', '#797b30', '#788f64',
              '#9fb5a0', '#819fb1', '#6f85a1', '#4e5184'),
    # A palette for tree branch
    tree=c('#d37d90', '#f4bd4a', '#eded9c', '#ed6930', '#50b751', '#7858a2',
           '#44bde3', '#868ec0', '#b21e3a', '#425199'),
    # A more pretty blue and red palette
    BlRe=c("#c96865", "#5c7eac"),
    # A palette for filling plot
    bar1=c('#ed8489', '#7c95c9', '#e0ae83', '#9573b0'),
    # Barplot
    bar2=c("#6F78B9", "#81D0D6", "#F37252", "#FDBC63"),
    # two nested group
    nestedgroup=c("#4e6645", "#afcea6", "#f4d44b", "#d67828"),
    # for taxnonomic barplot
    stackbar=c("#f2ad74", "#bed5e7", "#666ec0", "#741d23", "#762f7a", "#233752",
               "#758147", "#f39394", "#726659"),
    # longlong heatmap
    superheatmap=c("#347ab1", "#e26284", "#f1b069", "#7fab71", "#b48db4"),
    # network
    network1=c("#4c4ffe", "#cd3d32", "#739a57", "#f0e385", "#476984", "#f79321",
               "#5caede", "#7d2764", "#68d767", "#d492a4")
  )
  return(pal_lst[[pal_name]])
}