




#' Title pecanCen
#'

#'
#' @examples
#' @import dplyr

#'
#' @param edges DF with edge columns "from", "to", "width"
#' @param nodes DF with nodes columns "id", "value"
#' @param sev_weighted logical, should a severity weigthed out_degreee be calculated?
pecanCen <- function(edges,nodes, sev_weighted = TRUE){

  # check requirements

  if(is.data.frame(edges) == FALSE){stop("edges must be a dataframe")}
  if(is.data.frame(nodes) == FALSE){stop("nodes must be a dataframe")}

  if("id" %in% colnames(nodes) == FALSE){stop("id column in nodes is missing")}
  if(sev_weighted == TRUE){if("value" %in% colnames(nodes) == FALSE){stop("value column is missing in nodes")}}


  if("from" %in% colnames(edges) == FALSE){stop("from column in edges is missing")}
  if("to" %in% colnames(edges) == FALSE){stop("to column in edges is missing")}
  if("width" %in% colnames(edges) == FALSE){stop("width column in edges is missing")}


  cen_edges <- edges %>% dplyr::select(from,to,width)
  cen_nodes <- nodes
  nodes_id_value <- cen_nodes %>% dplyr::select(id,value) # %>% rename("id" = id)
  all_edges_width <- sum(cen_edges$width)
  n_nodes <- nrow(cen_nodes)


  if("label" %in% colnames(cen_nodes)){cen_nodes <- cen_nodes %>% dplyr::select(id,value,label)} else{
                                       cen_nodes <- cen_nodes %>% dplyr::select(id,value)}



  #calculate out degree centrality
  out_df <- centrality_out(cen_edges = cen_edges, all_edges_width = all_edges_width, n_nodes = n_nodes)

  # caclculate in degree centrality
  in_df  <- centrality_in(cen_edges = cen_edges, all_edges_width = all_edges_width, n_nodes = n_nodes)

  # merge out and in
  out_in <- full_join(out_df,in_df, by = "id")

  # merge with values of the nodes
  out_in_value <- full_join(out_in,nodes_id_value, by = "id")


  #out_df_value <- full_join(out_df,nodes_id_value, by = "id") not sure if still needed

  # add sev weighted out degree if TRUE
  if(sev_weighted == TRUE){final_df <- centrality_sev(in_df = in_df,cen_edges = cen_edges,nodes_id_value = nodes_id_value, out_in_value =  out_in_value)}
   else {final_df <- out_in_value}


  # Add labels if possible
  if("label" %in% colnames(cen_nodes)){node_labels <- cen_nodes %>% dplyr::select(id,label)
                                       final_df <- dplyr::full_join(final_df,node_labels, by = "id")}

  # SOrt df
  final_df_sort <- final_df[order(final_df$id, decreasing = FALSE),]

  # sort columns
  if("label" %in% colnames(cen_nodes)){
  final_df_sort <- final_df_sort %>% dplyr::select(id,label,everything())}

  # replace nas with 0
  final_df_sort <- final_df_sort %>% replace(is.na(.), 0)

  final_df_sort}

  #test_final <<- final_df_sort}





 # in_d_only <- in_d_df %>% select(id,in_degree) %>% rename("to" = "id")
#
 # edges_in_d <- full_join(cen_edges,in_d_only, by = "to")

  #nodes_id_value2 <- nodes_id_value %>% rename("to" = id)

  #edges_in_d_nodes <- full_join(edges_in_d,nodes_id_value2, by = "to")


  #edges_in_d_nodes <- edges_in_d_nodes %>% mutate(iwo = ((width/in_degree)*value))

  #sev_w_od <- edges_in_d_nodes %>% group_by(from) %>% summarise(sev_w_outdegree = sum(iwo)) %>% rename("id" = from)

  #final_df <- full_join(od_ind_nv,sev_w_od, by = "id")}



