

#' Title
#'
#' @param nodes
#' @param edges
#'
#' @return
#' @export
#'
#' @examples
pecan2igraph <- function(nodes,edges, directed = TRUE){
  # create the graph
  graph <- igraph::graph_from_data_frame(edges, directed = directed, vertices = nodes)

  # set edges attributes
  if("width" %in% colnames(edges)){E(graph)$weight <- edges$width} # edges weight
  if ("color" %in% colnames(edges)){E(graph)$color <- edges$color} # edges color
  if ("width" %in% colnames(edges)){E(graph)$width <- abs(edges$width)} # edges width

  # set nodes attributes

  if ("color" %in% colnames(nodes)){V(graph)$color <- nodes$color} # nodes color
  if ("value" %in% colnames(nodes)){V(graph)$size <- nodes$value} # nodes value --> size
  if ("label" %in% colnames(nodes)){V(graph)$label <- nodes$label} # nodes labels
  if ("id" %in% colnames(nodes)){V(graph)$name <- nodes$id} # nodes id --> names

  graph}
