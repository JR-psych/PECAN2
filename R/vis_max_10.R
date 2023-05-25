
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
vis_max_10 <- function(net_edges, max10){
      if(is.numeric(max10) & max10 > 0){
      if(max10 < 10){net_edges <- net_edges %>% dplyr::mutate(width = width*(10/max10))} else
      {if(max10 > 10){net_edges <- net_edges %>% dplyr::mutate(width = width/(max10/10))}}
    } else {stop("max10 must be numeric")}
  net_edges
  }







