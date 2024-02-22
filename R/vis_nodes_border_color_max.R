
#' Title vis_nodes_color_max
#'
#'
#' @param vis.env
#' @import dplyr
#' @return
#'
#' @examples
vis_nodes_border_color_max <- function(net_nodes,nodes_border_color_max){
  if(nodes_border_color_max != 10){
    if(nodes_border_color_max < 10){net_nodes$border_color <- net_nodes$border_color * (10/nodes_border_color_max)}
    if(nodes_border_color_max > 10){net_nodes$border_color <- net_nodes$border_color / (nodes_border_color_max/10)}}
  net_nodes}






