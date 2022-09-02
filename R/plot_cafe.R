#' Draw CAFE5 result
#' 
#' @param cafe File path. The CAFE result directory
#' @param tree_95 (optional) File path. The tree generated from the MCMCTree software with 95% HPD
#' @param label2species (optional)  File path. A table with two columns used to replace labels in the tree. Two columns: Label/Species name
#'
#' @return A CAFE result tree
#' @import ggtree
#' @import tidytree
#' @import treeio
#' @import stringr
#' @export
#'
#' @examples
#' For future
plot_cafe <- function(cafe, tree_95=NULL, label2species=NULL){
  args <- list()
  for (file_ in list.files(cafe)){
    if(str_ends(file_, "tre"))
    {
      args[['tree']]<- paste(cafe, file_, sep = '/')
    }
    else if(str_ends(file_, "probabilities.tab")){
      args[['pvalue']]<- paste(cafe, file_, sep = '/')
    }
    else if(str_ends(file_, "count.tab")){  
      args[['count']]<- paste(cafe, file_, sep = '/')
    }
    else if(str_ends(file_, "change.tab")){ 
      args[['change']]<- paste(cafe, file_, sep = '/')
    }
  }
  
  pvalue <- read.delim(args[['pvalue']], check.names = FALSE)
  names(pvalue)[1] <- 'FamilyID'
  pvalue <- pvalue[-length(pvalue)]
  changeF <- read.delim(args[['change']], check.names = FALSE)
  countF <- read.delim(args[['count']], check.names = FALSE)
  timetree<-read.nexus(args[['tree']])
  timetree <- timetree[[1]] %>% as_tibble()
  timetree$label <- timetree$label %>% str_extract(".*>")
  if (!is.null(tree_95)){
    treeCI <- read.mcmctree(opt$tree)
    treeCI <- treeCI %>% as_tibble()
    names(treeCI) <- str_replace(names(treeCI), "0.95HPD", "HPD95")
    treeCI$reltime <- treeCI$reltime*100
    treeCI$HPD95 <- lapply(treeCI$HPD95, function(x){as.numeric(x)*100})
    timetree <-timetree %>% left_join(treeCI[c("parent", "node", "HPD95", "reltime")])
  }
  if (!is.null(label2species)){
    meta_species <- read.delim(label2species, check.names = FALSE, sep = '\t', header = FALSE, col.names = c('tmp_label', 'species'))
    timetree$tmp_label <- timetree$label %>% str_replace("<.*>","")
    timetree<-timetree %>% left_join(meta_species) %>% select(-tmp_label)
  }else{
    timetree$species <- timetree$label %>% str_replace("<.*>","")
  }
  
  get_change <- function(nodelabel){
      family_list <- pvalue %>% filter(!!sym(nodelabel) < 0.05) %>% pull(FamilyID)
      changelist <- changeF %>% 
        filter(changeF$FamilyID %in% family_list) %>% 
        select(FamilyID, !!sym(nodelabel))  %>% 
        mutate(situation=if_else(!!sym(nodelabel)>0, "expansion", "contraction")) %>%
        group_by(situation) %>%
        summarise(cluster = list(unique(FamilyID))) %>%
        deframe()
      num_list <- changeF %>% 
        select(!!sym(nodelabel))  %>% 
        mutate(situation=case_when(!!sym(nodelabel)>0 ~ "e_all", !!sym(nodelabel)<0 ~ "c_all")) %>%
        group_by(situation) %>%
        summarise(tmp=n()) %>%
        deframe() %>%
        `[`(c("c_all", "e_all"))
      if (NA %in% names(num_list)){
        if("c_all" %in% names(num_list)){
          num_list<-c(num_list["c_all"],0)
        }else if ("e_all" %in% names(num_list)){
          num_list<-c(0, num_list["e_all"])
        }else{
          num_list <- c(0,0)
        }
        names(num_list) <- c("c_all", "e_all")
      }
      num_list['rest'] =  countF %>% filter(!!sym(nodelabel)>0) %>% nrow() - sum(num_list)
      return(list(changelist, num_list))
    }
  
  # get contraction and expansion
  meta_info <- data.frame()
  pie_info <- data.frame()
  for (label_ in timetree$label){
    changelist<-get_change(label_)
    one_row <- vector()
    for (i in names(changelist[[1]])){one_row[i]<-length(changelist[[1]][[i]])}
    a<-data.frame(t(one_row))
    if (is.null(a$contraction)){a$contraction=0}
    if (is.null(a$expansion)){a$expansion=0}
    a['label'] = label_
    meta_info<-rbind(meta_info,a)
    # pie plot info
    b <- data.frame(t(changelist[[2]]))
    if(all(is.na(b))){next} # skip root node
    b['node'] = timetree$node[timetree$label == label_]
    pie_info<-rbind(pie_info,b)
  }
  
  # plot pie
  plot_pie <- function(pie_info){
    fig_list <- list()
    for (node_ in pie_info$node){
      fig <- pie_info %>% 
        filter(node==node_) %>%
        pivot_longer(cols = -node, names_to = "ftype", values_to = "num") %>% 
        ggplot() + 
        geom_bar(aes(x=1, y=num, fill=ftype), 
                 position = position_stack(), 
                 stat = "identity") +
        scale_fill_manual(values = c("#eb292d", "#177834", "#619cff"))+
        coord_polar(theta = "y") + 
        theme_void() + 
        guides(fill="none")
      fig_list <- append(fig_list, list(fig))
    }
    names(fig_list) <- pie_info$node
    return(fig_list)
  }
  pie_plots <- plot_pie(pie_info)
  # separate pie plots to node list and label list
  tip_plot_lst <- list()
  node_plot_lst <- list()
  tip_names <- c()
  node_names <- c()
  for (node_ in names(pie_plots)){
    if(isTip(timetree, node_)){
      tip_plot_lst <- append(tip_plot_lst, list(pie_plots[[node_]]))
      tip_names <- c(tip_names, node_)
    } else{
      node_plot_lst <- append(node_plot_lst, list(pie_plots[[node_]]))
      node_names <- c(node_names, node_)
    } 
  }
  names(tip_plot_lst) <- tip_names
  names(node_plot_lst) <- node_names
  # plot tree 
  ## use expression contaim 
  expression1 <- c()
  expression2 <- c()
  expression3 <- c()
  for (row_idx in seq_len(nrow(timetree))){
    var1<-meta_info[row_idx,]$contraction
    var2<-meta_info[row_idx,]$expansion
    expression1 <- append(expression1, deparse(bquote('-'*.(var1)*phantom('/+'*.(var2)))))
    expression2 <- append(expression2, deparse(bquote(phantom('-'*.(var1))*'/'*phantom('+'*.(var2)))))
    expression3 <- append(expression3, deparse(bquote(phantom('-'*.(var1)*'/')*'+'*.(var2))))
  }
  timetree$expression1 <- expression1
  timetree$expression2 <- expression2
  timetree$expression3 <- expression3
  
  
  ## the unit for vjust and hjust in ggtree was just as the x-axis(1 Myr in molecular clock)
  treelimit <- timetree$branch.length %>% replace_na(0) %>% max() * 1.5
  just_unit <- treelimit/120
  timetree1 <- timetree %>% left_join(meta_info) %>% as.treedata()
  treeplot <- ggtree(timetree1) + 
    geom_text(data=td_filter(!isTip), 
              aes(x=branch, 
                  label = expression1),
              parse = T, size=4, vjust=1.2, color="#eb292d") +
    geom_text(data=td_filter(!isTip), 
              aes(x=branch, 
                  label = expression2),
              parse = T, size=4, vjust=1.2, color="#619cff") +
    geom_text(data=td_filter(!isTip), 
              aes(x=branch, 
                  label = expression3),
              parse = T, size=4, vjust=1.2, color="#177834") +
    geom_tiplab(aes(label = expression1),
                parse = T, size=4, vjust=2, hjust=0.3, color="#eb292d") +
    geom_tiplab(aes(label = expression2),
                parse = T, size=4, vjust=2, hjust=0.3, color="#619cff") +
    geom_tiplab(aes(label = expression3),
                parse = T, size=4, vjust=2, hjust=0.3, color="#177834") +
    geom_tiplab(aes(label=species),
                fontface="italic") +
    geom_inset(node_plot_lst, width = .05, height = .05, vjust = -0.5, x="branch") + 
    geom_inset(tip_plot_lst,width = .05, height = .05, hjust = -just_unit*25) +
    theme_tree2() +
    xlim(NA, treelimit)
  
  # 95% 
  if(exists("treeCI")){
    treeplot <- treeplot + 
      geom_range(range='HPD95', center='reltime', color='#6565B8', alpha=.6, size=2) +
      geom_nodelab(aes(label=round(reltime,2)), size=4, hjust = -0.1) 
  }
  treeplot
}