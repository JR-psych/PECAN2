
#' Title vis_nodes_color_max
#'
#'
#' @param vis.env
#'
#' @return
#'
#' @examples
vis_nodes_color_max <- function(net_nodes,nodes_color_max){
  if(nodes_color_max < max(net_nodes$color)){stop("nodes_color_max must be greater or equal than the maximum value of nodes_color")}
  if(nodes_color_max != 10){
   if(nodes_color_max < 10){net_nodes$color <- net_nodes$color * (10/nodes_color_max)}
    if(nodes_color_max > 10){net_nodes$color <- net_nodes$color / (nodes_color_max/10)}}
  net_nodes}






