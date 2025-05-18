



#' Title
#'
#' @param data This parameter should be a dataframe where each row represents a person or timepoint (can also just be one row),
#'  and the columns contain specific values for the nodes and edges corresponding to that person or timepoint.
#' @param id_col
#' @param id_number
#' @param nodes Specifies the positions of nodes columns in the dataframe. E.g. 1:10
#' @param edges Specifies the positions of edges columns in the dataframe. E.g. 11:100.  Each edge should be represented by a
#' combination of node names separated by a specified separator. For example, if an edge connects nodes "pain" and "insomnia",
#' it could be represented as "pain_insomnia" in the dataframe.
#' @param edges_direction
#' @param edges_sep
#' @param nodes_labels
#' @param label_pattern
#' @param nodes_attributes
#' @param attribute_pattern
#' @param attribute_name
#' @import purrr
#' @import dplyr
#' @import magrittr
#' @import tidyr
#' @import Hmisc
#' @return
#' @export
#'
#' @examples
pecanExtract <- function(data,
                         id_row = 1,
                         nodes,
                         edges,
                         edges_direction = "from_to", # from_to. to_from
                         edges_sep,
                         nodes_labels = NULL,
                         label_pattern = NULL,
                         nodes_attributes = NULL,
                         attribute_pattern = NULL,
                         drop_nodes_NA0 = "na_zero", # na_zero, na, zero, none
                         drop_edges_NA0 = "na_zero", # na_zero, na, zero, none
                         drop_attributes_NA0 = "na_zero"){ # na_zero, na, zero, none


  #Check whether drop settings are eight
  if(!(drop_nodes_NA0 %in% c("na_zero","na","zero","none"))){stop("drop_nodes_NA0 must be 'na_zero', 'na', 'zero' or 'none'")}
  if(!(drop_edges_NA0 %in% c("na_zero","na","zero","none"))){stop("drop_edges_NA0 must be 'na_zero', 'na', 'zero' or 'none'")}
  if(!(drop_attributes_NA0 %in% c("na_zero","na","zero","none"))){stop("drop_attributes_NA0 must be 'na_zero', 'na', 'zero' or 'none'")}

  #check whether id_row is numeric
  if(!is.numeric(id_row)){stop("id_row must be numeric")}

  # check whether data is a data frame
  if(!is.data.frame(data)){stop("data must be a dataframe")}

  # make sure it is a real df and filter the id row
  data_filterd <- as.data.frame(data)
  data_filterd <- data_filterd[id_row,,drop = FALSE]
  rownames(data_filterd) <- NULL


  # prepare edges df and only select edges and nodes df
  data_edges <- data_filterd[,edges,drop = FALSE]
  data_nodes <- data_filterd[, nodes, drop = FALSE]

  # make sure value columns for nodes and edges are numeric
  data_edges <- data_edges %>% dplyr::mutate(across(where(~ !is.numeric(.)), as.numeric))
  data_nodes <- data_nodes %>% dplyr::mutate(across(where(~ !is.numeric(.)), as.numeric))

  # create nodes_long df
  nodes_long <- tidyr::pivot_longer(data_nodes, cols = everything(), names_to = "id", values_to = "value")

  # drop NA and Zero values if chosen
  if(drop_nodes_NA0 == "na_zero" | drop_nodes_NA0 == "na"){nodes_long <- nodes_long[!is.na(nodes_long$value),,drop = FALSE]
  rownames(nodes_long) <- NULL}
  if(drop_nodes_NA0 == "na_zero" | drop_nodes_NA0 == "zero"){if(any(0 %in% nodes_long$value)){nodes_long <- nodes_long[nodes_long$value != 0,,drop = FALSE]
  rownames(nodes_long) <- NULL
  warning("Nodes which had a value = 0 were filtered out")
  att_long}}

  # add labels if chosen
  if(!is.null(nodes_labels)){
    if(is.null(label_pattern)){stop("label_pattern is not defined")}

    data_labels <- data_filterd[,nodes_labels,drop = FALSE] #select label columns
    data_labels <- setNames(data_labels, gsub(label_pattern, "", colnames(data_labels))) # get rid of the pattern so they a re named like the nodes
    data_labels <- data_labels %>% dplyr::mutate(across(where(~ !is.character(.)), as.character)) # make sure they are character columns
    labels_long <- tidyr::pivot_longer(data_labels, cols = everything(), names_to = "id", values_to = "label") # create long_df

    labels_long <- labels_long[!is.na(labels_long$label),,drop = FALSE] #drop NA values
    rownames(labels_long) <- NULL
    if(!all(labels_long$id %in% nodes_long$id)){
      warning("Some labels have no corresponding node and were filterd out")}

    nodes_long <- merge(nodes_long, labels_long, by = "id", all.x = TRUE, all.y = FALSE) # merge with nodes_long df

  }

  # add second attribute if chosen
  if(!is.null(nodes_attributes)){
    if(is.null(attribute_pattern)){stop("attribute_pattern is not defined")}

    data_att <- data_filterd[,nodes_attributes, drop = FALSE] #select attribute columns
    data_att <- setNames(data_att, gsub(attribute_pattern, "", colnames(data_att))) # get rid of the pattern so they are named like the nodes
    data_att <- data_att %>% dplyr::mutate(across(where(~ !is.numeric(.)), as.numeric)) # make sure they are numeric columns

    att_long <- tidyr::pivot_longer(data_att, cols = everything(), names_to = "id", values_to = "attribute") # create long_df

    # drop NA and zero values if chosen
    if(drop_attributes_NA0 == "na_zero" | drop_attributes_NA0 == "na"){att_long <- att_long[!is.na(att_long$attribute),,drop = FALSE]
    rownames(att_long) <- NULL}
    if(drop_attributes_NA0 == "na_zero" | drop_attributes_NA0 == "zero"){if(any(0 %in% att_long$attribute)){att_long <- att_long[att_long$attribute != 0,,drop = FALSE]
    rownames(att_long) <- NULL
    warning("Attributes which had a width = 0 were filtered out")
    att_long}}


    if(!all(att_long$id %in% nodes_long$id)){
      warning("Some attributes have no corresponding node and were filterd out")}

    nodes_long <- merge(nodes_long, att_long, by = "id", all.x = TRUE, all.y = FALSE) # merge with nodes_long df

  }


  # create edges_long df
  edges_long <- tidyr::pivot_longer(data_edges, cols = everything(), names_to = "edges", values_to = "width")
  edges_long <- edges_long  %>% tidyr::separate_wider_delim(edges,names = c("from","to"), delim = edges_sep) # sep edge names into from an to column

  # drop NA and zero values if chosen
  if(drop_edges_NA0 == "na_zero" | drop_edges_NA0 == "na"){edges_long <- edges_long[!is.na(edges_long$width),,drop = FALSE]
  rownames(edges_long) <- NULL}
  if(drop_edges_NA0 == "na_zero" | drop_edges_NA0 == "zero"){if(any(0 %in% edges_long$width)){edges_long <- edges_long[edges_long$width != 0,,drop = FALSE]
  rownames(edges_long) <- NULL
  warning("Edges which had a width = 0 were filtered out")
  edges_long}}


  # select only edges that have corresponding nodes
  nEdges_og <- as.numeric(nrow(edges_long))
  keep_edges <- NULL

  for (i in seq_len(nrow(edges_long))){
    if(edges_long[[i,"from"]] %in% nodes_long$id & edges_long[[i,"to"]] %in% nodes_long$id){
      keep_edges <- cbind(keep_edges,i)}
  }

  keep_edges <- as.vector(keep_edges)
  edges_long <- edges_long[keep_edges,,drop = FALSE]
  rownames(edges_long) <- NULL
  if(nEdges_og != as.numeric(nrow(edges_long))){
    warning("Edges have been dropped as no reference node was found")}

  # change edges direction if data were stored differently
  if(!(edges_direction %in% c("to_from","from_to"))){stop("edges_direction must be 'to_from' or 'from_to'")}
  if(edges_direction == "to_from"){
    edges_long <- edges_long %>% dplyr::rename(to = from, from = to)
    edges_long <- edges_long %>% dplyr::select(from,to,everything())}


  # create list with finals dfs
  final_ex <-  list("nodes" = nodes_long,
                    "edges" = edges_long,
                    "arguments" = list(id_row = id_row,
                                       nodes = nodes,
                                       edges = edges,
                                       edges_direction = edges_direction,
                                       edges_sep = edges_sep,
                                       nodes_labels = nodes_labels,
                                       label_pattern = label_pattern,
                                       nodes_attributes = nodes_attributes,
                                       attribute_pattern = attribute_pattern,
                                       drop_nodes_NA0 = drop_nodes_NA0,
                                       drop_edges_NA0 = drop_edges_NA0,
                                       drop_attributes_NA0 = drop_attributes_NA0))
  final_ex}

















