




#' Title pecanExtract
#'
#'
#'
#' @return a list with a node df and an edge df for a specific person
#' @export
#'
#' @examples
#' @import Hmisc
#' @import dplyr
#' @import xts
#' @import purrr
#' @import tidyr
#'
#' @param data df which includes the nodes and edges. Requires a column with the name "p_id"
#' @param p_id id of the person of interest...
#' @param edges Specify the columns of the edges e.g. 2:12. Edge names need to have a "to" and a "from" specification within their name and a separator.
#' Names can be numbers E.g 1_2 equals: "from" = 1 and "to" = 2 or words: Pain_Insomnia equals "from" = Pain and "to" = Insomnia
#' @param nodes Specify the columns of the nodes e.g. 13:17. Node names must match the edge names!
#' @param edges_sep How are "from" and "to" separated in the names?
#' @param label_in_names Logical. Are there additional labels in the node names. Must be separated and be second part of the name. e.g 1_Pain or Insomnia_I couldnt sleep
#' @param labels_sep How are the labels separated in the node names
#' @param labels Its possible to just add vector with the labels
#'
pecanExtract <- function(data, p_id, edges, nodes, edges_sep,label_in_names = FALSE,labels_sep = "none",labels = "none"){
  data <- data
  id <- p_id
  edges <- edges
  nodes <- nodes
  edges_sep <- edges_sep
  labels_sep <- labels_sep
  node_labels <- labels

  # get the df  on the person level
  data_filterd <- data %>% dplyr::filter(p_id == id)

  # prepare edges df and only select edges which are not NA
  data_edges <- data_filterd %>% dplyr::select(edges)
  edges_index <- purrr::map_lgl(data_edges, ~ all(!is.na(.)))
  data_edges <- data_edges[, edges_index]


  # prepare nodes df and only select nodes which are not NA
  data_nodes <- data_filterd %>% dplyr::select(nodes)
  nodes_index <- purrr::map_lgl(data_nodes, ~ all(!is.na(.)))
  data_nodes <- data_nodes[, nodes_index]


  # needed for pivot_longer
  data_edges <- data_edges %>% dplyr::mutate_if(is.character,as.numeric)
  data_nodes <- data_nodes %>% dplyr::mutate_if(is.character,as.numeric)

  # create edges df in long format
  edges_long <- tidyr::pivot_longer(data_edges, cols = everything(), names_to = "edges", values_to = "width")
  edges_long <- edges_long %>% tidyr::separate(edges,c("from","to"),edges_sep) # sep edge names into from an to column

  # create nodes df in long format
  nodes_long <- tidyr::pivot_longer(data_nodes, cols = everything(), names_to = "id", values_to = "value")


  # separate lables in names if there
  if(label_in_names == TRUE){nodes_long <- nodes_long %>% tidyr::separate(id,c("id","label"),labels_sep)}
  else{if(labels[1] != "none"){
    #id_order_check <- as.numeric(nodes_long$id)
    #if(xts::isOrdered(id_order_check, increasing = TRUE) == FALSE){warning("Node id´s are not increasingly ordered. This might cause problems when the labels are assigned")}
    node_labels <- node_labels[nodes_index]
    nodes_long <- nodes_long %>% dplyr::mutate(labels = node_labels)}}

  # check if all edges also exist as a node
  if(all(edges_long$from %in% nodes_long$id) == FALSE){from_missing <- edges_long$from[which(edges_long$from %nin% nodes_long$id)]
  fmw <- paste(from_missing, "are/is part of an edge (from) but are/is not specified as a node")
  warning(fmw)}

  # check if all edges also exist as a node
  if(all(edges_long$to %in% nodes_long$id) == FALSE){to_missing <- edges_long$to[which(edges_long$to %nin% nodes_long$id)]
  tmw <- paste(to_missing,"are/is part of an edge (to) but are/is not specified as a node")
  warning(tmw)}

  # filter edges with a value of zero
  if(any(0 %in% edges_long$width)){edges_long <- filter_zero_edges(edges = edges_long)}

  ## add p_id to node df

  nodes_long <- nodes_long %>% dplyr::mutate(p_id = id)

  # create list with finals dfs
  list("nodes" = nodes_long,
       "edges" = edges_long)

}



#if(nodes_labels[1] != "none"){colnames(data_nodes) <- node_labels}
