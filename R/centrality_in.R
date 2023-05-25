




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
#' @param all_edges_width sum of the width of all edges within the network
centrality_in <- function(cen_edges, n_nodes, all_edges_width){

  in_df <- cen_edges %>% dplyr::group_by(to) %>% dplyr::summarise(in_degree = sum(width), # sum of all incoming edges
                                                      in_connect = n(), # to how many nodes is one node connected
                                                      per_indegree = sum(width)/all_edges_width,
                                                      per_in_connect = (n())/(n_nodes -1)) %>% rename("id" = to)

  in_df}





