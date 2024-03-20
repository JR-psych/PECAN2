




#' Title
#'

#'
#' @return DF with in_degree centrality measures
#'
#'
#' @examples
#' @import dplyr

#'
#' @param cen_edges edges df
#' @param n_nodes   number of nodes within the network
#' @param all_edges_strength sum of the strength of all edges within the network
centrality_in <- function(cen_edges, n_nodes, all_edges_strength){
  #browser()
  in_df <- cen_edges %>% dplyr::group_by(to) %>% dplyr::summarise(in_degree = sum(strength), # sum of all incoming edges
                                                      in_connect = n(), # to how many nodes is one node connected
                                                      per_indegree = sum(strength)/all_edges_strength,
                                                      per_in_connect = (n())/(n_nodes -1)) %>% rename("id" = to)
  #browser()
  in_df}





