




#' Title pecanCen
#'

#'@export
#' @examples
#' @import dplyr

#'
#' @param edges DF with edge columns "from", "to", "strength"
#' @param nodes DF with nodes columns "id", "value"
#' @param sev_weighted logical, should a severity weigthed out_degreee be calculated?
pecanCen <- function(edges,nodes,
                     centrality_by,
                     sev_weighted = NULL,
                     nodes_label = "auto"){

  # check requirements

  if(is.data.frame(edges) == FALSE){stop("edges must be a dataframe")}
  if(is.data.frame(nodes) == FALSE){stop("nodes must be a dataframe")}

  edges <- as.data.frame(edges)
  nodes <- as.data.frame(nodes)


  if("id" %in% colnames(nodes) == FALSE){stop("id column in nodes is missing")}
  if(!is.null(sev_weighted)){
                             if(sev_weighted %in% colnames(nodes) == FALSE){
                             stop("'sev_weighted' column in nodes is missing")
                           }}

  if("from" %in% colnames(edges) == FALSE){stop("from column in edges is missing")}
  if("to" %in% colnames(edges) == FALSE){stop("to column in edges is missing")}
  if(centrality_by %in% colnames(edges) == FALSE){stop("centrality_by column in edges is missing")}


  cen_edges <- edges[,c("from","to")]
  cen_edges$strength <- edges[,centrality_by]
  cen_nodes <- nodes[,"id",drop = FALSE]
  if(!is.null(sev_weighted)){
    if(!(sev_weighted %in% names(nodes))){stop("sev_weighted not in nodes")}
    cen_nodes$value <- nodes[,sev_weighted]}

  if(!is.null(nodes_label)){
    if(nodes_label == "auto"){
      if("label" %in% colnames(nodes)){cen_nodes$label <- as.character(nodes$label)}
      }
    else{cen_nodes$label <- as.character(nodes[,nodes_label])}
    }

  #browser()
  cen_nodes_og <- cen_nodes
  all_edges_strength <- sum(cen_edges$strength)
  n_nodes <- nrow(cen_nodes)

  #calculate out degree centrality
  out_df <- centrality_out(cen_edges = cen_edges, all_edges_strength = all_edges_strength, n_nodes = n_nodes)

  # caclculate in degree centrality
  in_df  <- centrality_in(cen_edges = cen_edges, all_edges_strength = all_edges_strength, n_nodes = n_nodes)

  # merge out and in
  out_in <- full_join(out_df,in_df, by = "id")

  # merge with values of the nodes
  out_in_value <- full_join(out_in,cen_nodes, by = "id")

  #browser()

  #out_df_value <- full_join(out_df,cen_nodes_og, by = "id") not sure if still needed

  # add sev weighted out degree if TRUE
  if(!is.null(sev_weighted)){final_df <- centrality_sev(in_df = in_df,cen_edges = cen_edges,
                                                      out_in_value =  out_in_value,
                                                      cen_nodes = cen_nodes)}
  else {final_df <- out_in_value}


  # Add labels if possible
 # if("label" %in% colnames(cen_nodes)){node_labels <- cen_nodes %>% dplyr::select(id,label)
#  final_df <- dplyr::full_join(final_df,node_labels, by = "id")}

  # SOrt df
  final_df_sort <- final_df[order(final_df$id, decreasing = FALSE),]

  # sort columns
  if("label" %in% colnames(cen_nodes)){
#    browser()
    final_df_sort <- final_df_sort %>% dplyr::select(id,label,everything())}

  # replace nas with 0
  final_df_sort <- final_df_sort %>% replace(is.na(.), 0)

  final_df_sort}

#test_final <<- final_df_sort}





# in_d_only <- in_d_df %>% select(id,in_degree) %>% rename("to" = "id")
#
# edges_in_d <- full_join(cen_edges,in_d_only, by = "to")

#cen_nodes_og2 <- cen_nodes_og %>% rename("to" = id)

#edges_in_d_nodes <- full_join(edges_in_d,cen_nodes_og2, by = "to")


#edges_in_d_nodes <- edges_in_d_nodes %>% mutate(iwo = ((strength/in_degree)*value))

#sev_w_od <- edges_in_d_nodes %>% group_by(from) %>% summarise(sev_w_outdegree = sum(iwo)) %>% rename("id" = from)

#final_df <- full_join(od_ind_nv,sev_w_od, by = "id")}



