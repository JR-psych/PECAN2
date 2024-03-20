
#' Title vis_max_10
#'
#' @param net_edges
#' @param use_ranking
#' @param min_rank
#' @param vis.env
#' @import dplyr
#' @return
#'
#' @examples
size_max <- function(net_nodes, nodes_size_max){

  if(!is.numeric(nodes_size_max) & !is.null(nodes_size_max)) {stop("nodes_size_max must be numeric or 'auto'")}
  if(is.null(nodes_size_max)){nodes_size_max <- as.numeric(max(net_nodes$size))}
  if(nodes_size_max <= 0){stop("nodes_size_max must be greater than 0")}
  if(nodes_size_max < 33){net_nodes <- net_nodes %>% dplyr::mutate(size = size*(33/nodes_size_max))}
  if(nodes_size_max > 33){net_nodes <- net_nodes %>% dplyr::mutate(size = size/(nodes_size_max/33))}

  net_nodes
}




