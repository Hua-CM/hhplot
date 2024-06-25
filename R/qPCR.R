#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param cq.table PARAM_DESCRIPTION
#' @param ref.group PARAM_DESCRIPTION, Default: 'CK'
#' @param ref.gene PARAM_DESCRIPTION, Default: 'GAPDH'
#' @param stat.method PARAM_DESCRIPTION, Default: 't.test'
#' @param remove.sample.outliers PARAM_DESCRIPTION, Default: TRUE
#' @param remove.tech.outliers PARAM_DESCRIPTION, Default: TRUE
#' @param plot.type PARAM_DESCRIPTION, Default: 'boxplot'
#' @param fig.ncol PARAM_DESCRIPTION, Default: 3
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname cal_2ddCt
#' @export 
#' @importFrom dplyr group_by mutate ungroup filter summarise n select case_when add_row distinct left_join
#' @importFrom stats sd
#' @importFrom rstatix t_test wilcox_test
#' @importFrom ggplot2 ggplot aes geom_boxplot facet_wrap geom_text labs theme element_text geom_bar geom_errorbar geom_jitter geom_hline
#' @importFrom ggthemes theme_pander
cal_2ddCt <- function(cq.table,
                      ref.group='CK',
                      ref.gene='GAPDH',
                      stat.method='t.test',
                      remove.sample.outliers=TRUE,
                      remove.tech.outliers=TRUE,
                      plot.type='boxplot',
                      fig.ncol = 3
                      ){
  cq.table <- cq.table %>%
    dplyr::select(cq, group,	gene,	biorep)
  target.genes <- setdiff(unique(cq.table$gene), ref.gene)
  # Remove outliers based on Cq value in technique replicates
  if (remove.tech.outliers) {
    cq.table <- cq.table %>%
      dplyr::group_by(group, gene, biorep) %>%
      dplyr::mutate(is.outlier = findoutliner2(cq)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(is.outlier == FALSE) # Output these not outliers
  }
  else {
    cq.table <- cq.table
  }
  res.dCT <- cq.table %>%
    pivot_wider(names_from=gene,
                values_from = cq,
                values_fn = mean) %>%
    mutate(across(target.genes, \(x){2^-(x-.data[[ref.gene]])})) %>%
    select(group, biorep, all_of(target.genes)) %>%
    pivot_longer(all_of(target.genes),
                 names_to = 'gene',
                 values_to = 'expression') # 2dCT
  # Statistic test
  res.dCT.summary <- res.dCT %>%
    dplyr::group_by(group, gene) %>%
    dplyr::summarise(mean.expression = mean(expression), 
                  sd.expression = stats::sd(expression),
                  n.biorep = dplyr::n(), 
                  se.expression = mean.expression/sqrt(n.biorep)) %>%
    dplyr::ungroup()
  if (stat.method == "t.test") {
    test_func <- rstatix::t_test
  }
  else if(stat.method == "wilcox.test"){
    test_func <- rstatix::wilcox_test
  }
  res.dCT.stat <- res.dCT %>%
    dplyr::group_by(gene) %>%
    test_func(expression~group, ref.group = ref.group) %>%
    dplyr::ungroup() %>%
    dplyr::select(gene, group=group2, p) %>%
    dplyr::mutate(signif = dplyr::case_when(p < 0.001 ~ "***",
                                            p > 0.001 & p < 0.01 ~ "**",
                                            p > 0.01 & p < 0.05 ~ "*",
                                            TRUE ~ "NS")) %>%
    dplyr::add_row(group = ref.group, p = NA, signif = NA) %>%
    right_join(res.dCT.summary)
  res.dCT.stat <- res.dCT %>%
    dplyr::group_by(group, gene) %>% 
    dplyr::mutate(max.temp = max(expression)) %>%
    dplyr::ungroup() %>%
    dplyr::select(group, gene, max.temp) %>%
    dplyr::distinct() %>%
    dplyr::left_join(res.dCT.stat)
  # Plot
  if(plot.type=="boxplot"){
    p <- ggplot2::ggplot(res.dCT, ggplot2::aes(group, expression, fill = group)) +
      ggplot2::geom_boxplot(width = 0.6) + 
      ggplot2::facet_wrap(. ~ gene,
                          scales = "free_y",
                          ncol = fig.ncol) +
      ggplot2::geom_text(data=res.dCT.stat, 
                         ggplot2::aes(group, mean.expression, label = "."),
                         check_overlap = TRUE,
                         size = 15, 
                         color = "red") +
      ggplot2::geom_text(data=res.dCT.stat,
                         ggplot2::aes(group, max.temp * 1.08, label = signif),
                         check_overlap = TRUE, 
                         size = 3, color = "black") +
      ggthemes::theme_pander() + 
      ggplot2::labs(y = "Relative expression") +
      ggplot2::theme(legend.position = "none", 
                     strip.text.x = ggplot2::element_text(face = "italic"))
  }else if(plot.type=="barplot"){
    p <- ggplot2::ggplot(data = res.dCT.stat,
                         ggplot2::aes(group, mean.expression, fill = group)) +
      ggplot2::geom_bar(data= res.dCT.stat, stat = "identity", width = 0.6) +
      ggplot2::geom_errorbar(data=res.dCT.stat,
                             ggplot2::aes(group,
                                          ymin = mean.expression - sd.expression,
                                          ymax = mean.expression + sd.expression),
                             width = 0.2) +
      ggplot2::geom_jitter(data=res.dCT,
                           ggplot2::aes(group, expression),
                           width = 0.1,
                           alpha = 0.4) +
      ggplot2::geom_hline(data=res.dCT.stat,
                          ggplot2::aes(yintercept = max.temp * 1.1), color = NA) +
      ggplot2::facet_wrap(. ~ gene, scales = "free_y", ncol = fig.ncol) +
      ggplot2::geom_text(data=res.dCT.stat,
                         ggplot2::aes(group, max.temp * 1.08, label = signif),
                         check_overlap = TRUE,
                         size = 4,
                         color = "red") +
      ggthemes::theme_pander() +
      ggplot2::labs(y = "Relative expression") + 
      ggplot2::theme(legend.position = "none", 
                     strip.text.x = ggplot2::element_text(face = "italic"))
  }
  # return result
  return(list('RelaExpress'=res.dCT,
              'statistic'=res.dCT.stat,
              'plot'=p))
}