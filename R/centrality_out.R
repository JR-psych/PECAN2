




#' Title centrality_out
#'

#' @return df with out_degree centrality for each node
#'
#'
#' @examples
#' @import dplyr

#'
#' @param cen_edges edges
#' @param n_nodes   number of nodes within the network
#' @param all_edges_strength sum of the strength of all edges within the network
centrality_out <- function(cen_edges, n_nodes, all_edges_strength){
  #browser()
  out_d <- cen_edges %>% dplyr::group_by(from) %>% dplyr::summarise(out_degree = sum(strength), # sum of all outgoing edges fpr each node
                                                         out_connect = n(), # how many nodes is one node influencing
                                                         per_degree = sum(strength)/all_edges_strength,
                                                         per_out_connect = (n())/(n_nodes -1)) %>% rename("id" = from)
  #browser()
  out_d}








