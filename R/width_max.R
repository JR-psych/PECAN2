
#' Title vis_max_10
#'
#' @param edges_width_max
#' @param net_edges
#'
#' @import dplyr
#' @return
#'
#' @examples
width_max <- function(net_edges, edges_width_max){
      if(is.null(edges_width_max)){edges_width_max <- as.numeric(max(net_edges$width))}
      if(!is.numeric(edges_width_max)) {stop("edges_width_max must be numeric or NULL")}
      if(edges_width_max < max(net_edges$width)){stop("edges_width_max must be greater or equal than the maximum value of edges_width")}


      if(edges_width_max <= 0){stop("edges_width_max must be greater than 0")}
      if(edges_width_max < 10){net_edges$width <- net_edges$width * (10/edges_width_max)}
      if(edges_width_max > 10){net_edges$width <- net_edges$width / (edges_width_max/10)}

  net_edges
  }







