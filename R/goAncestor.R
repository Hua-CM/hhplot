#' Title
#'
#' @param gene2GO A dataframe with two columns: GeneID GOID 
#'
#' @return A named list of GO IDs
#' @importFrom stringr str_starts
#' @importFrom tibble deframe
#' @importFrom GO.db GOBPANCESTOR
#' @importFrom GO.db GOMFANCESTOR
#' @importFrom GO.db GOCCANCESTOR
#' @importFrom rlang eval_tidy
#' @importFrom rlang caller_env
#'
#' @examples
#' For future
goAncestor <- function(gene2GO){
  ancestor_BP <- as.list(GO.db::GOBPANCESTOR)
  ancestor_MF <- as.list(GO.db::GOMFANCESTOR)
  ancestor_CC <- as.list(GO.db::GOCCANCESTOR)
  
  gene2go <-gene2GO %>% 
    tibble::deframe() %>% 
    base::split(names(.),unname(.))
  
  query_gene <- function(go_lst){
    
    out_go <- lapply(go_lst, function(y){
      c(ancestor_BP[[y]],
        ancestor_MF[[y]],
        ancestor_CC[[y]])
        }
      ) %>% 
      unlist() %>% 
      unique() %>%
      `[`(.,stringr::str_starts(.,"GO"))
  out_go
  }
  
  lapply(gene2go, query_gene)
}