
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
vis_Rank <- function(net_edges, use_ranking, min_rank, vis.env){
  if(use_ranking != "none"){ranking_test <- net_edges[[use_ranking]]
                            if(is.numeric(ranking_test) == FALSE){stop("Use_ranking must be numeric")}}



  if(use_ranking != "none"){if(any(net_edges[,use_ranking] < 0)){stop("no negative values in ranking variable allowed")}}

  ## filter edges that are below the by min_rank defined rank
  if(use_ranking != "none"){
    if(is.numeric(min_rank)){net_edges <- net_edges %>% dplyr::filter(net_edges[,use_ranking] <= min_rank)
                            # test_edges <<- net_edges %>% dplyr::filter(net_edges[,use_ranking] <= min_rank)
                             } #assign("net_edges", net_edges, envir = vis.env)
    else{stop("min_rank is missing or is not numeric")}
  }
  net_edges
  }







