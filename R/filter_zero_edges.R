




#' Title filter_zero_edges
#'
#'
#'
#' @return AN edges DF where all edges = 0 were filtered out
#'
#'
#'
#'
#' @import dplyr
#'
#'
#' @param edges

filter_zero_edges <- function(edges){
  edges <- edges %>% dplyr::filter(width != 0)
  warning("Edges which had a width = 0 were filtered out")
  edges
}



#if(nodes_labels[1] != "none"){colnames(data_nodes) <- node_labels}
