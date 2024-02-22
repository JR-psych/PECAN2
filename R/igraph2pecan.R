

#' Title
#'
#' @param nodes
#' @param edges
#'@import igraph
#' @return
#' @export
#'
#' @examples
igraph2pecan <- function(graph){
  #directed = TRUE
  # create the graph
  # is_directed(g_directed)
  nodes <- data.frame(id = V(graph)$name)
  if("size" %in% names(V(graph))){nodes$value <- V(graph)[["size"]]}
  if("label" %in% names(V(graph))){nodes$label <- V(graph)[["label"]]}
  if("color" %in% names(V(graph))){nodes$color <- V(graph)[["color"]]}

  edges <- data.frame(from = ends(graph, E(graph))[, 1],
                      to = ends(graph, E(graph))[, 2])

  if("weight" %in% names(E(graph))){weight_attribute <- E(graph)[["weight"]]}
  else{edges <- edges %>% dplyr::mutate(width = 7.5)
       warning("'weight' column in graph is not defined. Width was set to 7.5")}
  if("color" %in% names(E(graph))){edges$color <- E(graph)[["color"]]}



  nodes_edges <- list("nodes" = nodes,"edges" = edges)}
