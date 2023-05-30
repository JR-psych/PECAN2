




#' Title pecanQGCP
#'
#'
#' @return A centrality plot made with qgraph
#' @export
#'
#' @examples
#' @import dplyr
#' @import tidyverse
#' @import igraph
#' @import qgraph

#'
#' @param edges Edges df
#' @param nodes nodes df
#' @param theme_bw see centralityPlot function from qgraph
#' @param print see centralityPlot function from qgraph
#' @param verbose see centralityPlot function from qgraph
#' @param weighted see centralityPlot function from qgraph
#' @param signed see centralityPlot function from qgraph
#' @param orderBy see centralityPlot function from qgraph
#' @param decreasing see centralityPlot function from qgraph
#' @param scale see centralityPlot function from qgraph
#' @param include see centralityPlot function from qgraph
#' @param labelssee centralityPlot function from qgraph
#'
pecanQGCP <- function(edges,nodes,theme_bw = TRUE,print = TRUE,verbose = TRUE,weighted = TRUE,signed = TRUE,orderBy = "default",
                         decreasing = FALSE,scale = "raw0", include = c("OutStrength","InStrength","Closeness","Betweenness"),labels = "none"){

  edges <- edges
  nodes <- nodes
  net_edges <- edges
  net_nodes <- nodes
  # qgraph settings
  qg_scale <- scale
  qg_include <-  include
  qg_theme_bw = theme_bw
  qg_print = print
  qg_verbose = verbose
  qg_weighted = weighted
  qg_signed = signed
  qg_orderBy = orderBy
  qg_decreasing = decreasing
  qg_labels = labels


  # rename width so one can import to igraph
  net_edges <- net_edges %>% dplyr::rename(weight = "width") %>% dplyr::select(from,to,weight)

  #create graph with our dfs and then get the adj matrix of our graph so we can use qgraph
  g <- igraph::graph_from_data_frame(net_edges, directed = TRUE, vertices = net_nodes)
  mat <- as.matrix(igraph::as_adjacency_matrix(g, attr="weight"))

  # use qgraph to create the CP
  if(qg_labels != "none"){cp <- qgraph::centralityPlot(mat, labels = qg_labels, scale = qg_scale, include = qg_include, theme_bw = qg_theme_bw,
                                               print = qg_print, verbose = qg_verbose, weighted = qg_weighted, signed = qg_signed, orderBy = qg_orderBy,
                                               decreasing = qg_decreasing)}
  else {cp <- qgraph::centralityPlot(mat, scale = qg_scale, include = qg_include, theme_bw = qg_theme_bw,
                            print = qg_print, verbose = qg_verbose, weighted = qg_weighted, signed = qg_signed, orderBy = qg_orderBy,
                            decreasing = qg_decreasing)}

  cp
  }





