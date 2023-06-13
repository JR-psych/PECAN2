




#' Title pecanQGCT
#'

#'
#' @return A centrality table made with qgraph
#' @export
#'
#' @examples
#' @import dplyr
#'
#' @import igraph
#' @import qgraph

#'
#' @param edges Edges df
#' @param nodes Nodes df
#'
#'
#' @param standardized  see centralityTable function from qgraph
#' @param relative see centralityTable function from qgraph
#' @param weighted see centralityTable function from qgraph
#' @param signed see centralityTable function from qgraph
#' @param labels see centralityTable function from qgraph
#'
pecanQGCT <- function(edges,nodes, standardized = TRUE, relative = FALSE, weighted =
                                 TRUE, signed = TRUE,labels = "none"){

  edges <- edges
  nodes <- nodes
  net_edges <- edges
  net_nodes <- nodes

  # geraph settings
  qg_standardized <- standardized
  qg_relative <-  relative
  qg_weighted <- weighted
  qg_signed <- signed
  qg_labels <- labels


  # rename so we can import to igraph
  net_edges <- net_edges %>% dplyr::rename(weight = "width") %>% dplyr::select(from,to,weight)

  # get the graph so we can get the adj mat..  then we can use the centralityTable function of qg
  g <- igraph::graph_from_data_frame(net_edges, directed = TRUE, vertices = net_nodes)
  mat <- as.matrix(igraph::as_adjacency_matrix(g, attr="weight"))


  if(qg_labels != "none"){ct <- qgraph::centralityTable(mat, labels = qg_labels, standardized = qg_standardized, relative = qg_relative, weighted =
                                                          qg_weighted, signed = qg_signed)}
  else {ct <- qgraph::centralityTable(mat, standardized = qg_standardized, relative = qg_relative, weighted =
                                        qg_weighted, signed = qg_signed)}

  ct
  }





