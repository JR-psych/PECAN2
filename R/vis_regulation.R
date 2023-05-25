
#' Title
#'
#' @param net_edges
#' @param use_ranking
#' @param min_rank
#' @param vis.env
#' @import dplyr
#'
#' @return
#'
#' @examples
vis_regulation <- function(net_edges, net_nodes, regulation_by, cut_by, cut_off){
  if(cut_by[1] != "none"){cut_by_test <- net_edges[[cut_by]]
  if((is.numeric(cut_by_test)) == FALSE){stop("Cut_by must be numeric")}

  if(any(net_edges[,cut_by] < 0)){stop("no negative values in 'cut_by' allowed")}}


  ## if one wants to use regularization on edges
  if(regulation_by != "none"){

    if(regulation_by == "nodes"){if(is.numeric(cut_off)) {cut = trunc(nrow(nodes)*cut_off)} else {cut = nrow(nodes)} # give the maximum number of edges allowed
      ## regulation by nodes we only want to include a certain number of edges based on the number of nodes we have in the network
      ## cut_off is a weighting variable
      ## but_by is the column on which the regulation should depend on
      if(exists("cut_by") == FALSE){stop("cut_by is missing")}
      else {cut_df <- net_edges[[cut_by]] ## create vector with all values we should regulate by
      cut_df <- sort(cut_df, decreasing = TRUE) ## sort this vector decreasing to have the highest values first
      cut_df2 <- cut_df[cut] ## find the value equal to the maximum number of edges allowed
      na_c <- as.numeric(nrow(net_edges)) ##plan b if number of elements allowed is higher then the number of edges
      na_cut <- cut_df[na_c] ## so we take the last elemtn value
      final_cutoff <- ifelse(is.na(cut_df2) == TRUE,na_cut,cut_df2) ## decide which value to choose
      net_edges <- net_edges %>% dplyr::filter(net_edges[,cut_by] >= final_cutoff)}} ## filter our edges dataframe by our new cut_off
    ## regulation if we only want to include a certain number of edges// Cut_off defines that number
    if(regulation_by == "number"){if(is.numeric(cut_off)){cut = cut_off} else {stop("Cut_off is non numeric or not defined")} # check if cutoff is ther or numeric
      if(exists("cut_by") == FALSE){stop("OHneim! 404 cut_by not found")}
      else {cut_df <- sort(net_edges[,cut_by], decreasing = TRUE)
      cut_df2 <- cut_df[cut]
      na_c <- as.numeric(nrow(net_edges))
      na_cut <- cut_df[na_c]
      final_cutoff <- ifelse(is.na(cut_df2) == TRUE,na_cut,cut_df2)
      net_edges <- net_edges %>% dplyr::filter(net_edges[,cut_by] >= final_cutoff)}}
    ## regulation if width variable should be higher than a certain value
    if(regulation_by == "value"){if(is.numeric(cut_off)){cut = cut_off} else {stop("Cut_off is non numeric or not defined")}
      if(exists("cut_by") == FALSE){stop("OHneim! 404 cut_by not found")}
      else{net_edges <- net_edges %>% dplyr::filter(net_edges[,cut_by] >= cut_off)}}

    if(regulation_by != "nodes" & regulation_by != "number" & regulation_by != "value" & regulation_by != "none"){stop("regulation_by must be 'nodes','number' or 'value'")}
       net_edges}

}







