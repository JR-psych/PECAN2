
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
vis_regulation <- function(net_edges, net_nodes, edges_regulation){

  reg_type <- edges_regulation$reg_type
  reg_by <- edges_regulation$reg_by
  reg_value <- edges_regulation$reg_value

 # net_edges$internal_reg <- abs(net_edges[,reg_by])

  # If regulation is not NULL, check for required components
  ## if one wants to use regularization on edges
if(reg_type == "nodes") {
      cut <- trunc(nrow(net_nodes)*reg_value)  # give the maximum number of edges allowed
     # browser()
      ## regulation by nodes we only want to include a certain number of edges based on the number of nodes we have in the network
      ## cut_off is a weighting variable
      ## but_by is the column on which the regulation should depend on
      cut_df <- sort(net_edges[,"internal_reg"], decreasing = TRUE) ## create vector with all values we should regulate by
       ## sort this vector decreasing to have the highest values first
      cut_off1 <- cut_df[cut] ## find the value equal to the maximum number of edges allowed
     # browser()
      na_c <- as.numeric(nrow(net_edges)) ##plan b if number of elements allowed is higher then the number of edges
     # browser()
      na_cut <- cut_df[na_c] ## so we take the last elemtn value
     # browser()
      final_cutoff <- ifelse(is.na(cut_off1),na_cut,cut_off1) ## decide which value to choose
     # browser()
      net_edges <- net_edges %>% dplyr::filter(internal_reg >= final_cutoff) ## filter our edges dataframe by our new cut_off
      net_edges <- net_edges %>% dplyr::select(-internal_reg)}
## regulation if we only want to include a certain number of edges// Cut_off defines that number
    if(reg_type == "number"){
      cut_df <- sort(net_edges[,"internal_reg"], decreasing = TRUE)
      cut_off1 <- cut_df[reg_value]
      na_c <- as.numeric(nrow(net_edges))
      na_cut <- cut_df[na_c]
      final_cutoff <- ifelse(is.na(cut_off1),na_cut,cut_off1)
      net_edges <- net_edges %>% dplyr::filter(internal_reg >= final_cutoff)
      net_edges <- net_edges %>% dplyr::select(-internal_reg)}
    ## regulation if width variable should be higher than a certain value
    if(reg_type == "value"){
      net_edges <- net_edges %>% dplyr::filter(internal_reg >= reg_value)
      net_edges <- net_edges %>% dplyr::select(-internal_reg)}
net_edges}



