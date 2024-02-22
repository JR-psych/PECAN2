
#' Title vis_nodes_color_max
#'
#'
#' @param vis.env
#' @import dplyr
#' @return
#'
#' @examples
vis_nodes_color_max <- function(net_nodes,nodes_color_max){
  if(nodes_color_max != 10){
   if(nodes_color_max < 10){net_nodes$color <- net_nodes$color * (10/nodes_color_max)}
    if(nodes_color_max > 10){net_nodes$color <- net_nodes$color / (nodes_color_max/10)}}
  net_nodes}






