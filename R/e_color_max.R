e_color_max <- function(net_edges, edges_color_max){

  if(!is.numeric(edges_color_max) & !is.null(edges_color_max)) {stop("edges_color_max must be numeric or NULL")}
  if(is.null(edges_color_max)){edges_color_max <- as.numeric(max(net_edges$color))}
  if(edges_color_max <= 0){stop("edges_color_max must be greater than 0")}
  if(edges_color_max < 10){net_edges$color <- net_edges$color * (10/edges_color_max)}
  if(edges_color_max > 10){net_edges$color <- net_edges$color / (edges_color_max/10)}

  net_edges
}



