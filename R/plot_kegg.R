#' @title plot_kegg
#' @description Draw the KEGG summary plot for genome analysis. Recommend save as 18 x 16 cm.
#' @param KO_list a character vector. KO numbers. 
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[tibble]{enframe}}
#'  \code{\link[dplyr]{mutate-joins}}, \code{\link[dplyr]{distinct}}, \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{filter}}
#'  \code{\link[ggplot2]{geom_bar}}, \code{\link[ggplot2]{geom_label}}, \code{\link[ggplot2]{labs}}, \code{\link[ggplot2]{scale_continuous}}, \code{\link[ggplot2]{scale_manual}}, \code{\link[ggplot2]{theme}}
#' @rdname plot_kegg
#' @export 
#' @importFrom tibble enframe
#' @importFrom dplyr left_join distinct group_by summarise arrange filter
#' @importFrom ggplot2 geom_col geom_text labs scale_x_continuous scale_fill_manual theme

plot_kegg <- function(KO_list){
  lev1_color <- c(
    'Cellular Processes'='#8dd3c7',
    'Environmental Information Processing'='#fdae61',
    'Genetic Information Processing'='#bebada',
    'Human Diseases'='#fb8072',
    'Metabolism'='#80b1d3',
    'Organismal Systems'='#8AA923'
    )
  plot_data <- KO_list %>% 
    table() %>%
    tibble::enframe('KO', 'num') %>%
    dplyr::left_join(KO2map) %>%
    dplyr::left_join(map2lev[c('map', 'lev2')]) %>%
    dplyr::distinct(KO, lev2, .keep_all = T) %>%
    dplyr::group_by(lev2) %>%
    dplyr::summarise(tot_num=sum(num)) %>%
    dplyr::left_join(map2lev[c('lev1', 'lev2')] %>%
                distinct()) %>%
    dplyr::arrange(lev1, tot_num) %>%
    dplyr::filter(!is.na(lev2)) %>%
    dplyr::filter(!lev2=='Global and overview maps')
  
  plot_data$lev2 <- factor(plot_data$lev2, levels = plot_data$lev2)
  p_kegg <- ggplot(plot_data, aes(y=lev2, x=tot_num, fill=lev1)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(aes(label=tot_num),size=(8/2.8), hjust=0) +
    ggplot2::labs(title='KEGG annotation', x='#gene', y='', fill='') +
    ggplot2::scale_x_continuous(expand = c(0, 0, 0.1, 0)) +
    ggplot2::scale_fill_manual(values = lev1_color) +
    theme_Publication() +
    ggplot2::theme(panel.grid.major.x = element_line(colour="#f0f0f0"),
                   panel.grid.major.y = element_blank(),
                   legend.justification='left')
 p_kegg
}
