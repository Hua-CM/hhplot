#' plot_kegg
#' @description 
#' Draw the KEGG summary plot for genome analysis. Recommend save as 18 x 16 cm.
#' @param KO_list a character vector. KO numbers. 
#'
#' @export 
#' @import ggplot2
#' @importFrom magrittr %>%
#' @examples
#' For future
#' 
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
    enframe('KO', 'num') %>%
    left_join(hhplot:::KO2map) %>%
    left_join(hhplot:::map2lev[c('map', 'lev2')]) %>%
    distinct(KO, lev2, .keep_all = T) %>%
    group_by(lev2) %>%
    summarise(tot_num=sum(num)) %>%
    left_join(hhplot:::map2lev[c('lev1', 'lev2')] %>%
                distinct()) %>%
    arrange(lev1, tot_num) %>%
    filter(!is.na(lev2)) %>%
    filter(!lev2=='Global and overview maps')
  
  plot_data$lev2 <- factor(plot_data$lev2, levels = plot_data$lev2)
  p_kegg <- ggplot(plot_data, aes(y=lev2, x=tot_num, fill=lev1)) +
    geom_col() +
    geom_text(aes(label=tot_num),size=(8/2.8), hjust=0) +
    labs(title='KEGG annotation', x='#gene', y='', fill='') +
    scale_x_continuous(expand = c(0, 0, 0.1, 0)) +
    scale_fill_manual(values = lev1_color) +
    hhplot::theme_Publication() +
    theme(panel.grid.major.x = element_line(colour="#f0f0f0"),
          panel.grid.major.y = element_blank(),
          legend.justification='left')
 p_kegg
}
