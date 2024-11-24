




#' Title pecanCen
#'
#' @param edges edges data frame with a "from" and "to" column
#' @param nodes nodes data frame with an "id" column
#' @param centrality_by column name which contains the edge strengths in the edges data frame
#' @param sev_weighted Should severity weighted out degree be calculated. Must be set to the column name of the nodes data frame containing the node severity values.
#'                     Default is NULL.
#' @param absV Logical. Set to TRUE to calculate nodes Strength by using absolute values. Set to FALSE to calculate Expected Influence. Default is FALSE
#' @param nodes_label If node labels should be on the output this can be set to the column name of the nodes data frame containing these labels. Default is NULL
#' @export

pecanCen <- function(edges,nodes,
                     centrality_by,
                     sev_weighted = NULL,
                     nodes_label = NULL,
                     absV = FALSE){

  # check requirements
  degree_type <- "Expected Influence"

  if(is.data.frame(edges) == FALSE){stop("edges must be a dataframe")}
  if(is.data.frame(nodes) == FALSE){stop("nodes must be a dataframe")}

  edges <- as.data.frame(edges)
  nodes <- as.data.frame(nodes)


  if("id" %in% colnames(nodes) == FALSE){stop("id column in nodes is missing")}
  if(!is.null(sev_weighted)){
                             if(!(sev_weighted %in% colnames(nodes))){
                             stop("'sev_weighted' column in nodes is missing")
                           }}

  if("from" %in% colnames(edges) == FALSE){stop("from column in edges is missing")}
  if("to" %in% colnames(edges) == FALSE){stop("to column in edges is missing")}
  if(centrality_by %in% colnames(edges) == FALSE){stop("centrality_by column in edges is missing")}


  cen_edges <- edges[,c("from","to"), drop = FALSE]
  cen_edges$strength <- edges[,centrality_by]
  cen_nodes <- nodes[,"id",drop = FALSE]
  if(!is.null(sev_weighted)){
    if(!(sev_weighted %in% names(nodes))){stop("sev_weighted not in nodes")}
    cen_nodes$value <- nodes[,sev_weighted, drop = TRUE]
    if(absV == TRUE){cen_nodes$value <- abs(cen_nodes$value)}}

  if(!is.null(nodes_label)){
    if(!(nodes_label %in% names(nodes))){stop("nodes_label not in nodes")}
    cen_nodes$label <- as.character(nodes[,nodes_label])


    }

  if(absV == TRUE){cen_edges$strength <- abs(cen_edges$strength)
                   degree_type <- "Strength"}

  if(any(is.na(cen_nodes))){stop("No Na values allowed in nodes")}
  if(any(is.na(cen_edges))){stop("No Na values allowed in edges")}


  if(any(0 %in% cen_edges$strength)){cen_edges <- cen_edges %>% dplyr::filter(strength != 0)
                                     warning("Edges which had a width = 0 were filtered out")
                                     cen_edges}

  #browser()
  cen_nodes_og <- cen_nodes
  cen_edges_og <- cen_edges


  keep_edges <- NULL

  for (i in seq_len(nrow(cen_edges))){
    if(cen_edges[[i,"from"]] %in% cen_nodes$id & cen_edges[[i,"to"]] %in% cen_nodes$id){
      keep_edges <- cbind(keep_edges,i)}
  }

  keep_edges <- as.vector(keep_edges)
  cen_edges <- cen_edges[keep_edges,,drop = FALSE]
  rownames(cen_edges) <- NULL

  if(as.numeric(nrow(cen_edges_og)) != as.numeric(base::nrow(cen_edges))){
    warning("Edges have been dropped as no reference node was found")}


  all_edges_strength <- sum(cen_edges$strength) ## na.rm = TRUE? noch einbauen?
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
  else{final_df <- out_in_value}


  # Add labels if possible
 # if("label" %in% colnames(cen_nodes)){node_labels <- cen_nodes %>% dplyr::select(id,label)
#  final_df <- dplyr::full_join(final_df,node_labels, by = "id")}

  # SOrt df
  final_df_sort <- final_df[order(final_df$id, decreasing = FALSE),]

  final_df_sort$degree_type <- degree_type

  # sort columns
  if("label" %in% colnames(cen_nodes)){
#    browser()
    final_df_sort <- final_df_sort %>% dplyr::select(id,label,degree_type,everything())}
  else{final_df_sort <- final_df_sort %>% dplyr::select(id,degree_type,everything())}

  # replace nas with 0
  final_df_sort[is.na(final_df_sort)] <- 0

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



