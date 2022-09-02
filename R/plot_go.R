#' Title
#' @description 
#' Recommend save as 18 x 16 cm.
#' @param GO_list 
#'
#' @return
#' @export
#'
#' @examples
plot_go<-function(GO_list){
  lev_color <- c(
    'BP'='#c57373',
    'MF'='#82aedd',
    'CC'='#93c294')
  
  plot_data <- GO_list %>%
    table() %>%
    as.data.frame() %>%
    `names<-`(c('GO', 'tot_num')) %>%
    right_join(hhplot:::GO_lev2) %>% 
    arrange(lev, tot_num) %>%
    filter(!is.na(tot_num))
  plot_data$Description <- factor(plot_data$Description, levels = plot_data$Description)
  p_go <- ggplot(plot_data, aes(y=Description, x=tot_num, fill=lev)) +
    geom_col() +
    geom_text(aes(label=tot_num),size=(8/2.8), hjust=0) +
    labs(title='GO annotation', x='#gene', y='', fill='') +
    scale_x_continuous(expand = c(0, 0, 0.1, 0)) +
    scale_fill_manual(values = lev_color, labels=c('BP'='Biological Process', 
                                                   'MF'='Molecular Function',
                                                   'CC'='Cellular component')) +
    hhplot::theme_Publication() +
    theme(panel.grid.major.x = element_line(colour="#f0f0f0"),
          panel.grid.major.y = element_blank(),
          legend.justification='left')
  p_go
}