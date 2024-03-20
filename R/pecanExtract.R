


#' Title
#'
#' @param data
#' @param edges
#' @param nodes
#' @param edges_sep
#' @param labels
#' @param id_col
#' @param id_number
#' @param nodes_labels
#' @param label_pattern
#' @param nodes_attributes
#' @param attribute_pattern
#' @param attribute_name
#' @param edges_direction
#' @import purrr
#' @import dplyr
#' @import magrittr
#' @import tidyr
#' @import Hmisc
#' @return
#' @export
#'
#' @examples
pecanExtract <- function(data, id_col = NULL, id_number = NULL,
                         edges, nodes, labels, edges_sep,
                         nodes_labels = NULL, label_pattern = NULL,
                         nodes_attributes = NULL, attribute_pattern = NULL,
                         attribute_name = "attribute", edges_direction = "from_to"){


  ex_info_id_col <- id_col
  ex_info_id_number <- id_number
  ex_info_sep = edges_sep
  # get the df  on the person level
  if(!is.data.frame(data)){stop("data must be a dataframe")}

  data_filterd <- as.data.frame(data)

  if(is.null(id_col)){data_filterd <- data[1,,drop = FALSE]}
  else{
    if("internal_id" %in% names(data_filterd)){stop("no column should be named internal_id")}
    colnames(data_filterd)[
      which(names(data_filterd) == id_col)] <- "internal_id"
    if(is.null(id_number)){stop("id_number is null")}

    data_filterd <- data_filterd[data_filterd$internal_id == id_number,]
    }


  # prepare edges df and only select edges which are not NA

  #browser()
  data_edges <- data_filterd %>% dplyr::select(edges)
  #browser()
  edges_index <- purrr::map_lgl(data_edges, ~ all(!is.na(.)))
  #browser()
  data_edges <- data_edges[, edges_index]
 # browser()
  # prepare nodes df and only select nodes which are not NA
 # browser()
  data_nodes <- data_filterd %>% dplyr::select(nodes)
 # browser()
  nodes_index <- purrr::map_lgl(data_nodes, ~ all(!is.na(.)))
 # browser()
  data_nodes <- data_nodes[, nodes_index]

 # browser()
  # needed for pivot_longer
  data_edges <- data_edges %>% dplyr::mutate_if(is.character,as.numeric)
  data_nodes <- data_nodes %>% dplyr::mutate_if(is.character,as.numeric)
 # browser()
  # create edges df in long format
  edges_long <- tidyr::pivot_longer(data_edges, cols = everything(), names_to = "edges", values_to = "width")
  edges_long <- edges_long %>% tidyr::separate(edges,c("from","to"),edges_sep) # sep edge names into from an to column
 # browser()
  # create nodes df in long format
  nodes_long <- tidyr::pivot_longer(data_nodes, cols = everything(), names_to = "id", values_to = "value")
 # browser()
  # add labels if they are there
  if(!is.null(nodes_labels)){
    if(is.null(label_pattern)){stop("label_pattern is not defined")}

    data_labels <- data_filterd %>% dplyr::select(nodes_labels)
    labels_index <- purrr::map_lgl(data_labels, ~ all(!is.na(.)))
    data_labels <- data_labels[, labels_index]
    data_labels <- setNames(data_labels, gsub(label_pattern, "", colnames(data_labels)))

    data_labels <- data_labels %>% dplyr::mutate_if(is.character,as.numeric)

    labels_long <- pivot_longer(data_labels, cols = everything(), names_to = "id", values_to = "label")

    if(!all(labels_long$id %in% nodes_long$id)){
      warning("Some labels have no corresponding node and were filterd out")}

    nodes_long <- merge(nodes_long, labels_long, by = "id", all.x = TRUE, all.y = FALSE)

  }
#  browser()
  # add second attribute

  if(!is.null(nodes_attributes)){
    if(is.null(attribute_pattern)){stop("attribute_pattern is not defined")}

    data_att <- data_filterd %>% dplyr::select(nodes_attributes)
    att_index <- purrr::map_lgl(data_att, ~ all(!is.na(.)))
    data_att <- data_att[, att_index]
    data_att <- setNames(data_att, gsub(attribute_pattern, "", colnames(data_att)))

    data_att <- data_att %>% dplyr::mutate_if(is.character,as.numeric)

    att_long <- pivot_longer(data_att, cols = everything(), names_to = "id", values_to = attribute_name)

    if(!all(labels_long$id %in% nodes_long$id)){
      warning("Some labels have no corresponding node and were filterd out")}

    nodes_long <- merge(nodes_long, labels_long, by = "id", all.x = TRUE, all.y = FALSE)

  }








  # separate lables in names if there


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
 # browser()

if(edges_direction == "to_from"){
    edges_long <- edges_long %>% dplyr::rename(to = from, from = to)
    edges_long <- edges_long %>% dplyr::select(from,to,everything())}

 # nodes_long <- nodes_long %>% dplyr::mutate(p_id = p_id_og)
 # browser()
  # create list with finals dfs
  list("nodes" = nodes_long,
       "edges" = edges_long,
       "info" = list(edges_sep = ex_info_sep, id_col = ex_info_id_col, id_number = ex_info_id_number))

}








#data.frame(pain = 8,
#           insomnia = 7,
#           pain.label = "aua",
#           insomnia.label = "muede")

#data.frame(id = c(pain,insomnia),
#           value = c(8,7),
#           label = c("aua","muede"))

#library(tidyr)
#library(dplyr)
# Your original dataframe
#original_df <- data.frame(pain = 8,
#                          insomnia = 7,
#                          pain.label = "aua",
#                          insomnia.label = "muede")
#only_nodes <- original_df %>% select(1,2)

# Use pivot_longer to reshape the dataframe
#long_df <- tidyr::pivot_longer(
#  data = only_nodes,
#  cols = everything(),
#  names_to = "id",
#  values_to = "value"
#)

#pat <- paste0("\\",".label")
#pat2 <- ".label"
#ol <- original_df %>% select(3,4)


#ol2 <- setNames(ol, gsub(pat2, "", colnames(ol)))

#llabels <- tidyr::pivot_longer(
#  data = ol2,
#  cols = everything(),
#  names_to = "id",
#  values_to = "label"
#)

#att_df <-




  # Extract the labels from the original dataframe
#  long_df$label <- ifelse(grepl("label", long_df$id), original_df[paste0(long_df$id, ".label")], "")

# Remove the "label" suffix from the id column
#long_df$id <- sub("\\.label", "", long_df$id)

# Reorder the columns
#long_df <- long_df[, c("id", "value", "label")]

# Print the resulting dataframe
#print(long_df)






# Use gather to reshape the dataframe
#long_df <- tidyr::gather(original_df, key = "id", value = "value", -c(pain.label, insomnia.label))

# Extract the labels from the original dataframe
#long_df$label <- ifelse(grepl("pain", long_df$id), original_df$pain.label, original_df$insomnia.label)
#
# Remove the "label" suffix from the id column
#long_df$id <- sub("\\.label", "", long_df$id)

# Reorder the columns
#long_df <- long_df[, c("id", "value", "label")]

# Print the resulting dataframe
#print(long_df)




#data_gewuerze <- data.frame(id = c(1,2,3), salz = c(1,2,NA))
#data_pfeffer <-  data.frame(id = c(1,2,3,4), pfeffer = c(1,2,3,5))
#data_zimt <- data.frame(id = c(1,2), zimt = c(3,5))


















