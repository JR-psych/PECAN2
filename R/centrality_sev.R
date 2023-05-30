




#' Title centrality_sev
#'
#'
#'
#' @return A DF with severity weighted out_degree centrality measures: See Klintwall 2021
#'
#'
#' @examples
#' @import dplyr

#'
#' @param cen_edges
#' @param in_df
#' @param nodes_id_value
#' @param out_in_value
centrality_sev <- function(cen_edges,in_df,nodes_id_value,out_in_value){

# Assign the complete indegree of a node tp all incoming edges
id_indegree <- in_df %>% dplyr::select(id,in_degree) %>% dplyr::rename("to" = "id")

edges_indegree <- dplyr::full_join(cen_edges,id_indegree, by = "to")



# Assign the severity to a node to all incoming edges

nodes_id_value_new <- nodes_id_value %>% dplyr::rename("to" = id)

edges_indegree_nodes <- dplyr::full_join(edges_indegree,nodes_id_value_new, by = "to")


# Calculate the Severity Weigthed Outdegree for each edge

edges_indegree_nodes <- edges_indegree_nodes %>% dplyr::mutate(iwo = ((width/in_degree)*value))

# Sum up the severity weigthed outgeree for each node (sum of outgoing edges)

sev_w_od <- edges_indegree_nodes %>% dplyr::group_by(from) %>% dplyr::summarise(sev_w_outdegree = sum(iwo)) %>% dplyr::rename("id" = from)

# Create final data frame

final_df <- dplyr::full_join(out_in_value,sev_w_od, by = "id")

final_df}



