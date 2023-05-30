
#' Title vis_edge_color
#'
#' @param net_edges
#' @param use_ranking
#' @param min_rank
#' @param vis.env
#' @import dplyr
#' @return
#'
#' @examples
vis_edge_color <- function(net_edges, edges_color){
  range_width <- max(net_edges$width) - min(net_edges$width)
  range_div <- range_width/3
  rd1 <- min(net_edges$width) + range_div
  rd2 <- min(net_edges$width) + 2*(range_div)
  # setting egdes color
  if(edges_color == "default10"){net_edges <- net_edges %>% dplyr::mutate(color.color = ifelse(width <= 4, "grey", ifelse(width <= 7,"#454545", "black")))}
  else{if(edges_color == "d10_range"){net_edges <- net_edges %>% dplyr::mutate(color.color = ifelse(width <= rd1, "grey", ifelse(width <= rd2,"#454545", "black")))}
          else{net_edges <- net_edges %>% dplyr::mutate(color.color = edges_color)}}
  net_edges
}

